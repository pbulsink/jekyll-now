#' Calculate Elo Ratings for a series of games
#'
#' @param schedule A four column schedule, containing [Date, HomeTeam, AwayTeam, Result [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]].
#' @param ratings_history optional: A past history of team ranking, to be expanded Default: None
#' @param k optional: The Elo K value. Default: 8
#'# @param playoffs_boost optional: A boolean whether or not to enahnce importance of playoff games. Default: False
#' @param mean_value optional: The mean value to regress results to at the end of the season. Default: 1500
#' @param new_teams optional: The ranking that new teams should have. Default: 1500
#'
#' @return The Elo Rating for each team through time
#' @examples
#' calculateEloRatings(nhl20102011)
#' calculateEloRatings(nhl20152016, ratings_history=hist_elo, k=10, mean_value=1505, new_teams=1350)
calculateEloRatings <- function(schedule, ratings_history = NULL, k = 8, playoffs_boost = FALSE, mean_value = 1500, new_teams = 1500, meta = TRUE) {
    # Ensuring Opts are ok.
    stopifnot(ncol(schedule) == 4, nrow(schedule) > 0)
    team_names = unique(c(schedule$HomeTeam, schedule$AwayTeam))
    nteams <- length(team_names)
    
    if (!ratings_history) {
        ratings_history = data.frame(rep(new_teams, nteams))
        rownames(ratings_history) <- team_names
    }
    
    stopifnot(is.integer(k), k <= 0, k > 100)
    stopifnot(is.integer(mean_value))
    stopifnot(is.integer(new_teams))
    stopifnot(is.logical(playoffs_boost))
    
    # Massage Data & Extract Extras
    cnames(schedule) <- c("Date", "HomeTeam", "AwayTeam", "Result")
    game_dates <- sort(unique(schedule$Date))
    split_dates <- splitDates(game_dates)
    
    stopifnot(length(splitDates) > 0)
    
    if (length(split_dates) >= 2) {
        for (i in c(1:length(split_dates - 1))) {
            ratings_history <- .eloSeason((schedule[schedule$Date %in% unique(split_dates[i]), ]), new_teams = new_teams)
            if (meta) {
                metaElo <- calculateEloMetas(ratings_history, split_dates[i])
            }
            ratings_history <- .regressToMean(ratings_history, rmean = mean_value, rdate = split_dates[[i + 1]][1] - 1)
        }
    }
    ratings_history <- .eloSeason((schedule[schedule$Date %in% unique(split_dates[length(split_dates)]), ]))
    
    return(ratings_history)
}


#' Calculate the win chance percent for HomeTeam
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#'
#' @return A number between 0 and 1 corresponding to the win chances of the Home Team.
#'
predictEloResult <- function(home_rank, away_rank) {
    return(1/(1 + (10^((away_rank - home_rank)/400))))
}

#' Calculate the new ranking of both teams after an interaction
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @param result The result of the game, either [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1] corresponding to an Away win, Away Win (OT), Away Win (SO) a draw, or a Home win (same adjustors).
#' @param k optional: The Elo K value. Default: 8
#'
#' @return A vector with two new ratings for Home and Away team, respectively
#'
newRankings <- function(home_rank, away_rank, result, k = 8) {
    # result is in set [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]
    h_rank <- home_rank + k * (result - predictEloResult(home_rank, away_rank))
    a_rank <- away_rank + k * ((1 - result) - (1 - predictEloResult(home_rank, away_rank)))
    return(c(h_rank, a_rank))
}

#'Split dates to by season if multiple seasons are calculated together
#'
#' @param game_dates The dates of games to be split by season as a vector of Dates, or as a df with dates in game_dates$Date.
#' @param season_split The annual date by which to split seasons. As string '-MM-DD' format. For NHL, August 1 is chosen, so '-08-01'
#'
#' @return A list of vectors of dates.
splitDates <- function(game_dates, season_split = "-08-01") {
    if (is.data.frame(game_dates)) {
        game_dates <- sort(unique(as.Date(game_dates$Date)))
    }
    
    stopifnot(class(game_dates) == "Date")
    
    start_year <- as.numeric(format(game_dates[1], "%Y"))
    end_year <- as.numeric(format(game_dates[length(game_dates)], "%Y"))
    if (end_year - start_year <= 1) {
        return(list(game_dates))
    }
    split_dates <- rep(NULL, end_year - start_year)
    for (i in c(1:end_year - start_year)) {
        s <- game_dates[game_dates >= as.Date(paste0(i + start_year - 1, season_split)) & game_dates < as.Date(paste0(i + start_year, season_split))]
        if (!length(s) == 0) {
            split_dates[[i]] <- s
        }
    }
    
    # This removes null (unfilled) 'years' in the data.
    split_dates <- split_dates[!sapply(split_dates, is.null)]
    
    return(split_dates)
}

#'Calculate 1 season worth of elo
#'
.eloSeason <- function() {
    
}

#' Regress to mean, typically at end of season, but at any rdate.
#'
#'@param ratings_history The ratings to regress.
#'@param rmean The mean to regress towards
#'@param rstrength The amount by which to regress to the mean, in the formula rstrength/(rstrength + 1). ie. regression by 1/rstrength
#'@param rdate A date to appy the new rating by. This is default to the day after the last rating.
#'
#'@return ratings_history, with one additional row with the regressed value.
.regressToMean <- function(ratings_history, rmean = 1500, rstrength = 3, rdate = NULL) {
    if (is.null(rdate)) {
        rdate = as.Date(ratings_history[nrow(ratings_history), "Date"] + 1)
    }
    stopifnot(class(rdate) == "Date")
    
    ratings <- as.numeric(ratings_history[nrow(ratings_history), c(2:ncol(ratings_history))])
    newratings <- (ratings * rstrength + rmean)/(rstrength + 1)
    ratings_history[nrow(ratings_history) + 1, "Date"] <- rdate
    ratings_history[nrow(ratings_history), c(2:ncol(ratings_history))] <- newratings
    return(ratings_history)
}

#' Prepare historical data or future data for Elo Calculations
#'
#' @param schedule
#' @param splitPastPresent
#'
#' @return a named list with 'pastGames' and 'futureGames' as [Date, HomeTeam, AwayTeam, Result [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]]
prepareEloData <- function(schedule, splitPastPresent = TRUE) {
    
}

#' Load data from directory, and pass to prepareEloData if requested
#'
#' @param directory Directory where data is stored
#' @param prepare Whether to pass to prepareEloData automatically. Default True.
#' @param dropTeams Drop teams of given names
#' @param splitPastPresent Whether to split past and present teams in prepareEloData.
loadEloData <- function(directory, prepare = TRUE, dropTeams = NULL, splitPastPresent = TRUE) {
    
}
