library(reshape2)

predictSeasonElo<-function(elo_data, schedule, n_sims=10){
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

    team_results
}

predictEloResults.vec<-Vectorize(FUN = function(elo_diff, h_adv=0){return(1/(1 + (10^(((-1*elo_diff)+h_adv)/400))))},vectorize.args = 'elo_diff')
predictEloWins.vec<-Vectorize(FUN = function(pWin, n_sims){return(sample(c(0,0.4, 0.6, 1), size = n_sims, replace = TRUE, prob=c(1-pWin-0.1, 0.05, 0.05, pWin-0.1)))},vectorize.args = 'pWin')

sum(dnorm(x=p$Points-abs(p$Points-p$ActualPoints), mean = p$Points, sd=p$PointsSD))
