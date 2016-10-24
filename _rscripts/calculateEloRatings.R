#' Calculate Elo Ratings for a series of games
#'
#' @param schedule A four column schedule, containing [Date, HomeTeam, AwayTeam, Result [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]].
#' @param ratings_history optional: A past history of team ranking, to be expanded Default: None
#' @param k optional: The Elo K value. Default: 8
#' @param mean_value optional: The mean value to regress results to at the end of the season. Default: 1500
#' @param new_teams optional: The ranking that new teams should have. Default: 1500
#' @param regress_strength optional: The amount by which to regress to mean. Set = 0 to turn off regression. Default: 3 (1/3 regression to mean.)
#' @param home_adv: The home advantage, in rating points. Default: 35
#'
#' @return named list(ratings, meta) containg the Elo Rating for each team through time, and meta statistics if requested
#' @examples
#' calculateEloRatings(nhl20102011)
#' calculateEloRatings(nhl20152016, ratings_history=hist_elo, k=10, mean_value=1505, new_teams=1350)
calculateEloRatings <- function(schedule, ratings_history = NULL, k = 8, mean_value = 1500, new_teams = 1500, meta = TRUE, regress_strength=3, home_adv=35) {
    # Ensuring Opts are ok.
    stopifnot(ncol(schedule) == 4, nrow(schedule) > 0)
    names(schedule) <- c("Date", "HomeTeam", "AwayTeam", "Result")
    schedule$HomeTeam <- as.character(schedule$HomeTeam)
    schedule$AwayTeam <- as.character(schedule$AwayTeam)
    team_names = unique(c(schedule$HomeTeam, schedule$AwayTeam))
    nteams <- length(team_names)

    if (is.null(ratings_history)) {
        ratings_history <- data.frame("Date"=as.Date(schedule[1,"Date"] -1))
    }

    stopifnot(is.numeric(k), k >= 0, k < 100)
    stopifnot(is.numeric(mean_value))
    stopifnot(is.numeric(new_teams))

    # Massage Data & Extract Extras
    game_dates <- sort(unique(schedule$Date))
    split_dates <- splitDates(game_dates)

    stopifnot(length(splitDates) > 0)

    if(meta){
        meta_elo <- data.frame("season.end" = as.Date(character()), "mean" = numeric(), "max.team" = character(), "max.date" = as.Date(character()), "max.val" = numeric(), "min.team" = character(), "min.date" = as.Date(character()),
                            "min.val" = numeric(), "best.avg.team" = character(), "best.avg.team.avg" = numeric(), "worst.avg.team" = character(), "worst.avg.team.avg" = numeric())
    }

    if (length(split_dates) >= 2) {
        #For more than one season, use a progress bar. Can be a long calculation
        pb<-txtProgressBar(min = 0, max = length(split_dates), initial = 0)

        for (i in c(1:(length(split_dates)-1))) {


            newseason<-.eloSeason(schedule=schedule, dates=split_dates[[i]], ratings = ratings_history[nrow(ratings_history),,drop=FALSE], new_teams = new_teams, k=k, home_adv = home_adv)

            ifelse (newseason$newteam, ratings_history <- merge(ratings_history, newseason$ratings, all=TRUE), ratings_history<-rbind(ratings_history, newseason$ratings, make.row.names=FALSE))

            if (meta) {
                m<-metaElo(ratings_history = ratings_history, calcDates = split_dates[[i]], teams=newseason$teams)
                meta_elo <- rbind(meta_elo, m, make.row.names=FALSE)
            }

            #Regress to mean. Teams not playing in a season are given NA
            if (regress_strength != 0){
                ratings_history[nrow(ratings_history)+1,]<-NA
                ratings_history[nrow(ratings_history), "Date"] <- (split_dates[[i+1]][1]-1)
                ratings_history[nrow(ratings_history), newseason$teams] <- (ratings_history[(nrow(ratings_history)-1), newseason$teams] * regress_strength + mean_value)/(regress_strength + 1)
            }

            setTxtProgressBar(pb,i)
        }
    }

    #One (last) season
    newseason<-.eloSeason(schedule=schedule, dates=split_dates[[length(split_dates)]], ratings = ratings_history[nrow(ratings_history),,drop=FALSE], new_teams = new_teams, k=k, home_adv = home_adv)
    ifelse (newseason$newteam, ratings_history <- merge(ratings_history, newseason$ratings, all=TRUE), ratings_history<-rbind(ratings_history, newseason$ratings, make.row.names=FALSE))

    if (meta) {
        meta_elo <- rbind(meta_elo, metaElo(ratings_history, split_dates[[length(split_dates)]]), make.row.names=FALSE)
    }

    rlist<-list("Ratings"=ratings_history, "Meta" = meta_elo)
    return(rlist)
}


#' Calculate the win chance percent for HomeTeam
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @param h_adv The home advantage (in ranking points). Default: 0
#'
#' @return A number between 0 and 1 corresponding to the win chances of the Home Team.
#'
predictEloResult <- function(home_rank, away_rank, h_adv=0) {
    return(1/(1 + (10^((away_rank - (home_rank+h_adv))/400))))
}

#' Calculate the new ranking of both teams after an interaction
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @param result The result of the game, either [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1] corresponding to an Away win, Away Win (OT), Away Win (SO) a draw, or a Home win (same adjustors).
#' @param k optional: The Elo K value. Default: 8
#' @param h_adv: The home advantage (in ranking points). Default: 0
#'
#' @return A vector with two new ratings for Home and Away team, respectively
#'
newRankings <- function(home_rank, away_rank, result, k = 8, h_adv=0) {
    # result is in set [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]
    p<-predictEloResult(as.numeric(home_rank), as.numeric(away_rank), h_adv)
    h_rank <- as.numeric(home_rank) + k * (as.numeric(result) - p)
    a_rank <- as.numeric(away_rank) + k * ((1 - as.numeric(result)) - (1 - p))
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
        game_dates <- sort(unique(game_dates$Date))
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

    # This removes null (unfilled) 'years' in the data. I'm looking at you, '2005'.
    split_dates <- split_dates[!sapply(split_dates, is.null)]

    return(split_dates)
}

#'Calculate 1 season worth of elo
#'
.eloSeason <- function(schedule, dates, ratings, new_teams, k, home_adv) {
    newteam<-FALSE
    teams<-character()
    for (i in c(1:length(unique(dates)))){
        s<-schedule[(schedule$Date == as.Date(dates[i])),]

        h<-make.names(s[,"HomeTeam"])
        v<-make.names(s[,"AwayTeam"])
        teams<-unique(c(teams,h,v))

        if (length(teams[!(teams %in% names(ratings))]) > 0){
            newteam<-TRUE
            ratings<-cbind(ratings, as.data.frame(setNames(replicate((length(teams[!(teams %in% names(ratings))])), new_teams, simplify=FALSE), teams[!(teams %in% names(ratings))])))
        }

        newelos<-ratings[nrow(ratings), (!names(ratings) %in% "Date")]

        #hack to replace new (formally dropped out teams) as a new team with score new_teams
        newelos[,c(h,v)][is.na(newelos[,c(h,v)])]<-new_teams
        newrank<-newRankings(home_rank = newelos[1,h], away_rank=newelos[1,v], result=s[,"Result"], k=k, h_adv = home_adv)

        ngames<-length(h)
        newelos[1,h]<-newrank[c(1:ngames)]
        newelos[1,v]<-newrank[c((ngames+1):(2*ngames))]
        newelos$Date<-dates[i]

        ratings<-rbind(ratings, newelos, make.row.names=FALSE)
    }
    seasonreturn<-list("ratings"=ratings, "newteam"=newteam, "teams"=teams)
    return(seasonreturn)
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

#' Calculate and return meta statistics
#' @param ratings_history The ratings history to calculate against
#' @param calcDates An optional list of dates through which to calculate the meta statistics. Default all
#'
#' @return a named list of all statistics calculated
metaElo <- function(ratings_history, calcDates = NULL, teams=NULL) {
    if (!is.null(calcDates)) {
        meta_hist <- ratings_history[ratings_history$Date %in% unique(calcDates), ]
    } else {
        meta_hist <- ratings_history
        calcDates <- meta_hist$Date
    }

    if (is.null(teams)){
        teams<-names(meta_hist[,!(names(meta_hist) %in% "Date")])
    }

    #each game is 0 sum, each row has same average.
    meta_mean <- mean(rowMeans(meta_hist[nrow(meta_hist), teams], na.rm = TRUE))

    meta_max_row <- as.vector(which.max(apply(meta_hist[, teams], 1, max)))[1]
    meta_max_col <- as.vector(which.max(apply(meta_hist[, teams], 2, max)))[1]
    meta_max_val <- meta_hist[, teams][meta_max_row, meta_max_col]
    meta_max_team <- names(meta_hist[, teams])[meta_max_col]
    meta_max_date <- meta_hist[meta_max_row, "Date"]

    meta_min_row <- as.vector(which.min(apply(meta_hist[, teams], 1, min)))[1]
    meta_min_col <- as.vector(which.min(apply(meta_hist[,teams], 2, min)))[1]
    meta_min_val <- meta_hist[, teams][meta_min_row, meta_min_col]
    meta_min_team <- names(meta_hist[, teams])[meta_min_col]
    meta_min_date <- meta_hist[meta_min_row, "Date"]

    meta_best_team <- names(which.max(colMeans(meta_hist[, teams])))[1]
    meta_best_team_avg <- max(colMeans(meta_hist[, teams]))[1]

    meta_worst_team <- names(which.min(colMeans(meta_hist[, teams])))[1]
    meta_worst_team_avg <- min(colMeans(meta_hist[, teams]))[1]

    metar <- data.frame("season.end" = as.Date(meta_hist[nrow(meta_hist), "Date"]), "mean" = meta_mean, "max.team" = meta_max_team, "max.date" = as.Date(meta_max_date), "max.val" = meta_max_val, "min.team" = meta_min_team, "min.date" = as.Date(meta_min_date),
        "min.val" = meta_min_val, "best.avg.team" = meta_best_team, "best.avg.team.avg" = meta_best_team_avg, "worst.avg.team" = meta_worst_team, "worst.avg.team.avg" = meta_worst_team_avg, stringsAsFactors = FALSE)
    return(metar)
}
