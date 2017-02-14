source("./_rscripts/calculateEloRatings.R")
library(reshape2)
require(doParallel)

predictSeasonElo<-function(elo_data, schedule, n_sims=10){
    elo_data<-elo_data
    nhl_teams<-unique(c(as.character(schedule$Home), as.character(schedule$Visitor)))
    start_date<-as.Date(head(schedule$Date, 1))
    elo<-tail(elo_data[elo_data$Date < start_date,],1)
    elo_long<-melt(elo, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)

    schedule$EloDiff<-apply(schedule, 1, function(x) elo_long[elo_long$Team == make.names(x[2]),"Rating"]-elo_long[elo_long$Team == make.names(x[3]),"Rating"])

    schedule$PHome<-predictEloResults.vec(schedule$EloDiff, h_adv = 0)

    results <- matrix(predictEloWins.vec(schedule$PHome, n_sims), ncol=n_sims, byrow = TRUE)

    team_performance<-matrix(NA, nrow=length(nhl_teams), ncol=n_sims * 3)
    rownames(team_performance)<-make.names(nhl_teams)
    for(i in 1:n_sims){
        for(team in nhl_teams){
            team_performance[make.names(team), i]<-sum(results[schedule$Home == team,i]>0.5) + sum(results[schedule$Visitor == team, i]<0.5)
            team_performance[make.names(team), i+n_sims]<-sum(results[schedule$Home == team, i]==0.4)+sum(results[schedule$Visitor == team, i]==0.4)
            team_performance[make.names(team), i+2*n_sims]<-2 * team_performance[make.names(team), i] + team_performance[make.names(team), i+n_sims]

        }
    }
    team_results<-data.frame("Team"=nhl_teams)
    team_results$WinsMax<-apply(team_performance[,1:n_sims], 1, max)
    team_results$WinsMin<-apply(team_performance[,1:n_sims], 1, min)
    team_results$Wins<-apply(team_performance[,1:n_sims], 1, mean)
    team_results$WinsSD<-apply(team_performance[,1:n_sims], 1, sd)
    team_results$OTLoss<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossMax<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossMin<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossSD<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$PointsMax<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, max)
    team_results$PointsMin<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, min)
    team_results$Points<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, mean)
    team_results$PointsSD<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, sd)

    return(team_results)
}

predictEloResults.vec<-Vectorize(FUN = function(elo_diff, h_adv=0){return(1/(1 + (10^(((-1*elo_diff)+h_adv)/400))))},vectorize.args = 'elo_diff')
predictEloWins.vec<-Vectorize(FUN = function(pWin, n_sims){return(sample(c(0,0.4, 0.6, 1), size = n_sims, replace = TRUE, prob=c(1-pWin, 0.1-0.1*pWin, 0.1*pWin, pWin)))},vectorize.args = 'pWin')

scoreEloSeasonPredicted<-function(elo_data, schedule, actual_points, n_sims=10){
    results<-predictSeasonElo(elo_data = elo_data, schedule = schedule, n_sims = n_sims)

    d<-merge(results, actual_points, by="Team")

    return(sum(dnorm(x=d$ActualPoints, mean = d$Points, sd=d$PointsSD)))
}

eloSeasonPlotData<-function(nhl_data, nhl14, nhl14actual, nhl15, nhl15actual, nhl16, nhl16actual, n_sims=10000){
    colnames(nhl_data)<-c("Date","HomeTeam","AwayTeam","Result","Diff")
    cl <- makeCluster(3, outfile="./stdout.log")
    registerDoParallel(cl)
    exportFuns<-c('seasonScoreElo','predictSeasonElo','scoreEloSeasonPredicted', 'predictEloResults.vec', 'predictEloWins.vec', 'calculateEloRatings', 'splitDates', 'scoreEloVar', 'seasonScore', 'calculateEloRatings', '.eloSeason', 'predictEloResult', 'newRankings', 'variableK', 'getPredictedResults')
    #
    scores<-foreach(i=4:6, .export=exportFuns, .combine = 'c', .packages = c('reshape2')) %:% foreach(j=seq(from=0.5, to=1.5, by=0.5)) %:% foreach(k=c(1,3,5)) %:% foreach(l=seq(from=0,to=100,by=25)) %:% foreach(m=seq(from=1200,to=1400,by=100)) %dopar% seasonScoreElo(p=c(i, j, k, l, m), nhl_data = nhl_data, nhl14 = nhl14, nhl14actual = nhl14actual, nhl15 = nhl15, nhl15actual = nhl15actual, nhl16 = nhl16, nhl16actual = nhl16actual, n_sims = n_sims)

    stopCluster(cl)

    #scores<-as.data.frame(apply(as.data.frame(do.call('rbind', scores)), 2, unlist))

    return(scores)

    #ggplot(multiScores, aes(x=kPrime, y=gammaK, z=multiLL6, fill=multiLL6)) + geom_tile() + coord_equal() + geom_contour(color = "white", alpha = 0.5) + scale_fill_distiller(palette="Spectral", na.value="white") + theme_bw()
}

seasonScoreElo<-function(p=c('kPrime'=10, 'gammaK'=1, 'regressStrength'=3, 'homeAdv'=0, 'newTeam'=1300), nhl_data, nhl14, nhl14actual, nhl15, nhl15actual, nhl16, nhl16actual, n_sims){
    set.seed(1)

    #Calculate Elo by parameter
    kPrime=p[1]
    gammaK=p[2]
    regressStrength=p[3]
    homeAdv=p[4]
    newTeam=p[5] # Thus, as p5 goes up, newteam ratings go down. Typically p[5] = 50, newTeam=1300
    message(paste0("Calculating Elo with kPrime=",kPrime," gammaK=",gammaK," newTeam=",newTeam," regStrength=",regressStrength," homeAdv=",homeAdv))
    elo<-calculateEloRatings(nhl_data, k=kPrime, gammaK = gammaK, new_teams = newTeam, regress_strength = regressStrength, home_adv = homeAdv, k_var = TRUE, meta=FALSE)$Ratings

    message("Calculating Score")
    #compare season
    set.seed(1)
    score2014 <- scoreEloSeasonPredicted(elo_data = elo, schedule = nhl14, actual_points = nhl14actual, n_sims = n_sims)
    set.seed(1)
    score2015 <- scoreEloSeasonPredicted(elo_data = elo, schedule = nhl15, actual_points = nhl15actual, n_sims = n_sims)
    set.seed(1)
    score2016 <- scoreEloSeasonPredicted(elo_data = elo, schedule = nhl16, actual_points = nhl16actual, n_sims = n_sims)

    score<-mean(c(score2014,score2015, score2016))-sd(c(score2014,score2015, score2016))
    message(paste0("Score: ", score))
    #message("Done Scoring")
    #return scores
    return(list('Score'=score, 'gammaK'=gammaK, 'kPrime'=kPrime, 'regressStrength'=regressStrength, 'homeAdv'=homeAdv, 'newTeam'=newTeam))
    #return(score)
}

#require(optimx)
#optimx(par = c(6,1,3,35,1300), fn = seasonScoreElo, lower = c(1,0.1,1,0, 1000), upper=c(20, 5, 10, 100, 1500), control = list(all.methods = TRUE, follow.on = TRUE, save.failures = TRUE, maximize = TRUE, parscale=c(10,1,1,100,1000)), nhl_data = nhl_data, nhl14 = nhl14, nhl14actual = nhl14actual, nhl15 = nhl15, nhl15actual = nhl15actual, nhl16 = nhl16, nhl16actual = nhl16actual, n_sims = 10000)
