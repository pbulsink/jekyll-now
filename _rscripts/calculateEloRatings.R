#' Calculate Elo Ratings for a series of games
#'
#' @param schedule A four column schedule, containing [Date, HomeTeam, AwayTeam, Result [0, 0.5, 1]].
#' @param ratings_history optional: A past history of team ranking, to be expanded, as eloHist object. Default: None
#' @param k optional: The Elo K value. Default: 8
#'# @param playoffs_boost optional: A boolean whether or not to enahnce importance of playoff games. Default: False
#' @param mean_value optional: The mean value to regress results to at the end of the season. Default: 1500
#' @param new_teams optional: The ranking that new teams should have. Default: 1500
#' @return The Elo Rating for each team through time as eloHist object
#' @examples
#' calculateEloRatings(nhl20102011)
#' calculateEloRatings(nhl20152016, ratings_history=hist_elo, k=10, mean_value=1505, new_teams=1350)
calculateEloRatings<-function(schedule, ratings_history=NULL, k=8, playoffs_boost=FALSE, mean_value=1500, new_teams=1500)
{
    #Ensuring Opts are ok.
    stopifnot(ncol(schedule) == 4, nrow(schedule) > 0)
    team_names=unique(c(schedule$HomeTeam, schedule$AwayTeam))
    nteams<-length(team_names)

    if(!ratings_history){
        ratings_history=data.frame(rep(new_teams, nteams))
        rownames(ratings_history)<-team_names
        class(ratings_history)<-'eloHist'
    }
    
    stopifnot(class(ratings_history) == 'eloHist')
    stopifnot(is.integer(k), k <= 0, k > 100)
    stopifnot(is.integer(mean_value))
    stopifnot(is.integer(new_teams))
    stopifnot(is.logical(playoffs_boost))

    #Massage Data & Extract Extras
    cnames(schedule)<-c("Date", "HomeTeam", "AwayTeam", "Result")
    game_dates<-sort(unique(schedule$Date))
    split_dates<-splitDates(game_dates)

    #If possible, get current ELOs


    #For unique date in all dates
        #For each game in date
            #Calculate new Elos
        #Update Elos
        #Try Detect Season Breaks
            #If Season, regress to mean
    
    return(ratings_history)
}


#' Calculate the win chance percent for HomeTeam
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @return A number between 0 and 1 corresponding to the win chances of the Home Team.
#' @example predictEloResult(1350, 1625)
predictEloResult<-function(home_rank, away_rank){
    return(1/(1+(10^((away_rank-home_rank)/400))))
}

#' Calculate the new ranking of both teams after an interaction
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @param result The result of the game, either [0, 0.5, 1] corresponding to an Away win, a draw, or a Home win.
#' @param k optional: The Elo K value. Default: 8
#' @return A vector with two new ratings for Home and Away team, respectively
#' @example newRankings(1350, 1625, 0)
newRankings<-function(home_rank, away_rank, result, k=8) {
    #result is in set [0, 0.5, 1]
    h_rank<-home_rank + k*(result - predictEloResult(home_rank, away_rank))
    a_rank<-away_rank + k*((1-result) - (1 - predictEloResult(home_rank, away_rank)))
    return(c(h_rank, a_rank))
}

#'Split dates to by season if multiple seasons are calculated together
#'
#' @param game_dates The dates of games to be split by season as a vector of Dates, or as a df with dates in game_dates$Date.
#' @return A list of vectors of dates.
splitDates<-function(game_dates) {
    if (is.data.frame(game_dates)){
        game_dates<-as.date(game_dates$Date)
    }

    stopifnot(class(game_dates) == "Date")

    start_year<-as.numeric(format(game_dates[1],'%Y'))
    end_year<-as.numeric(format(game_dates[length(game_dates)], '%Y'))
    if (end_year-start_year <= 1){
        return(list(game_dates))
    }
    split_dates<-vector("list", length = end_year-start_year)
    for(i in c(1:end_year-start_year)){
        s<-game_dates[game_dates > as.Date(paste0(i+start_year-1, "-08-01")) & game_dates < as.Date(paste0(i+start_year, "-07-31"))]
        if(!length(s)==0) {
            split_dates[[i]]<-s
        }
    }
    return(split_dates)
}

#'Calculate 1 season worth of elo
#'
.eloSeason<-function(){
  
}

#' Regress to mean, typically at end of season
#' 
.regressToMean<-function(rmean=1500, rstrength=3){
  
}