library(reshape2)

predictSeasonElo<-function(elo_data, schedule, n_sims=10){
    nhl_teams<-unique(c(as.character(schedule$Home), as.character(schedule$Visitor)))
    start_date<-as.Date(head(schedule$Date, 1))
    elo<-tail(elo_data[elo_data$Date < start_date,],1)
    elo_long<-melt(elo, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)

    schedule$EloDiff<-apply(schedule, 1, function(x) elo_long[elo_long$Team == make.names(x[2]),"Rating"]-elo_long[elo_long$Team == make.names(x[3]),"Rating"])

    schedule$PHome<-predictEloResults.vec(schedule$EloDiff, h_adv = 0)

    results <- matrix(predictEloWins.vec(schedule$PHome, n_sims), ncol=n_sims, byrow = TRUE)

    team_performance<-matrix(NA, nrow=length(nhl_teams), ncol=n_sims)
    rownames(team_performance)<-make.names(nhl_teams)
    for(i in 1:n_sims){
        for(team in nhl_teams){
            team_performance[make.names(team), i]<-sum(results[schedule$Home == team,i]==1) + sum(results[schedule$Visitor == team, i]==0)
        }
    }
    team_results<-data.frame("Team"=nhl_teams)
    team_results$Max<-apply(team_performance, 1, max)
    team_results$Min<-apply(team_performance, 1, min)
    team_results$Wins<-apply(team_performance, 1, mean)
    team_results$SD<-apply(team_performance, 1, sd)

    team_results$Points<-2*team_results$Wins

    team_results
}

predictEloResults.vec<-Vectorize(FUN = function(elo_diff, h_adv=0){return(1/(1 + (10^(((-1*elo_diff)+h_adv)/400))))},vectorize.args = 'elo_diff')
predictEloWins.vec<-Vectorize(FUN = function(pWin, n_sims){return(sample(c(0,1), size = n_sims, replace = TRUE, prob=c(1-pWin, pWin)))},vectorize.args = 'pWin')
