#' Calculate Elo Ratings for a series of games
#'
#' @param schedule A four column schedule, containing [Date, HomeTeam, AwayTeam, Result [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]].
#' @param ratings_history optional: A past history of team ranking, to be expanded Default: None
#' @param k optional: The Elo K value. Default: 8
#' @param mean_value optional: The mean value to regress results to at the end of the season. Default: 1500
#' @param new_teams optional: The ranking that new teams should have. Default: 1500
#' @param regress_strength optional: The amount by which to regress to mean. Set = 0 to turn off regression. Default: 3 (1/3 regression to mean.)
#' @param home_adv The home advantage, in rating points. Default: 35
#' @param k_var optional: Whether to use fixed k or variable k. If variable, k' is set by k param. Then requires goal diff values.
#' @param gammaK optional: gamma value for variable k.
#'
#' @return named list(ratings, meta) containg the Elo Rating for each team through time, and meta statistics if requested
#' @examples
#' calculateEloRatings(nhl20102011)
#' calculateEloRatings(nhl20152016, ratings_history=hist_elo, k=10, mean_value=1505, new_teams=1350)
calculateEloRatings <- function(schedule, ratings_history = NULL, k = 20, mean_value = 1500, new_teams = 1300, meta = TRUE, regress_strength=3, home_adv=35, k_var=FALSE, gammaK=1) {
    # Ensuring Opts are ok.
    if (!k_var){
        stopifnot(ncol(schedule) == 4, nrow(schedule) > 0)
        names(schedule) <- c("Date", "HomeTeam", "AwayTeam", "Result")
        schedule$Diff<-NA
    }
    else {
        stopifnot(ncol(schedule) == 5, nrow(schedule) > 0)
        names(schedule) <-c("Date", "HomeTeam", "AwayTeam", "Result", "Diff")
    }
    schedule$HomeTeam <- as.character(schedule$HomeTeam)
    schedule$AwayTeam <- as.character(schedule$AwayTeam)
    team_names = unique(c(schedule$HomeTeam, schedule$AwayTeam))
    nteams <- length(team_names)

    if (is.null(ratings_history)) {
        ratings_history <- data.frame("Date"=as.Date(schedule[1,"Date"] -1))
    }

    #stopifnot(is.numeric(k), k >= 0, k < 100)
    stopifnot(is.numeric(mean_value))
    stopifnot(is.numeric(new_teams))
    stopifnot(is.numeric(gammaK))

    # Massage Data & Extract Extras
    game_dates <- sort(unique(schedule$Date))
    split_dates <- splitDates(game_dates)

    stopifnot(length(splitDates) > 0)

    meta_elo<-NULL

    if(meta){
        meta_elo <- data.frame("season.end" = as.Date(character()), "mean" = numeric(), "max.team" = character(), "max.date" = as.Date(character()), "max.val" = numeric(), "min.team" = character(), "min.date" = as.Date(character()),
                            "min.val" = numeric(), "best.avg.team" = character(), "best.avg.team.avg" = numeric(), "worst.avg.team" = character(), "worst.avg.team.avg" = numeric())
    }

    if (length(split_dates) >= 2) {
        #For more than one season, use a progress bar. Can be a long calculation
        pb<-txtProgressBar(min = 0, max = length(split_dates), initial = 0)

        for (i in c(1:(length(split_dates)-1))) {


            newseason<-.eloSeason(schedule=schedule, dates=split_dates[[i]], ratings = ratings_history[nrow(ratings_history),,drop=FALSE], new_teams = new_teams, k=k, home_adv = home_adv, k_var = k_var, gammaK=gammaK)

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
    newseason<-.eloSeason(schedule=schedule, dates=split_dates[[length(split_dates)]], ratings = ratings_history[nrow(ratings_history),,drop=FALSE], new_teams = new_teams, k=k, home_adv = home_adv, k_var=k_var, gammaK=gammaK)
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
newRankings <- function(home_rank, away_rank, result, k = 8, h_adv=0, k_var=FALSE, gammaK=1, diff=0) {
    # result is in set [0, 0.25, 0.4, 0.50, 0.6, 0.75, 1]
    if (k_var){
        k<-variableK(diff = diff, k_prime = k, gammaK = gammaK)
    }
    p<-predictEloResult(as.numeric(home_rank), as.numeric(away_rank), h_adv)
    h_rank <- as.numeric(home_rank) + k * (as.numeric(result) - p)
    a_rank <- as.numeric(away_rank) + k * ((1 - as.numeric(result)) - (1 - p))
    return(c(h_rank, a_rank))
}

#'K can be a variable value, rewarding bigger wins more. k'*(1+diff)^gammaK
#'
#' @param diff Goal differential
#' @param k_prime Base k' value
#' @param gammaK Exponential reward
#'
#' @return numerical k value for a specified goal differential
variableK<-function(diff, k_prime=10, gammaK=1){
    diff<-abs(diff)
    return(k_prime*(1+diff)^gammaK)
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
.eloSeason <- function(schedule, dates, ratings, new_teams, k, home_adv, k_var, gammaK) {
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
        newrank<-newRankings(home_rank = newelos[1,h], away_rank=newelos[1,v], result=s[,"Result"], k=k, h_adv = home_adv, k_var=k_var, gammaK=gammaK, diff=s[,"Diff"])

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
#' @param hockeyData Raw data read from hockey-reference cache
#' @param cleanTeams Whether or not to remove conflicting & historical team names
#' @param identifyTies Whether or not to add a boolean df column for tie games
#' @param listWinnersLosers Whether or not to add df columns containing winning and losing team name
#' @param removeInternational Whether or not to remove international teams from game info
#' @param eloResults Whether to calculate Elo results [0, 0.25, 0.4, 0.5, 0.6, 0.75, 1]
#'
#' @return a cleaned data frame
prepareEloData <- function(hockeyData, cleanTeams = TRUE, identifyTies = TRUE, listWinnersLosers = TRUE, removeInternational = TRUE, eloResults=TRUE) {
    teamReplace <- list(list("Alberta Oilers", "Edmonton Oilers"), list("Mighty Ducks of Anaheim", "Anaheim Ducks"), list("Winnipeg Jets (historical)","Arizona Coyotes"), list("Phoenix Coyotes", "Arizona Coyotes"), list("Atlanta Flames", "Calgary Flames"), list("Atlanta Thrashers", "Winnipeg Jets"),
                        list("Toronto Toros", "Birmingham Bulls"), list("Ottawa Nationals", "Birmingham Bulls"), list("Quebec Athletic Club/Bulldogs", "Brooklyn Americans"),
                        list("Hamilton Tigers", "Brooklyn Americans"), list("New York Americans", "Brooklyn Americans"), list("Philadelphia Blazers", "Calgary Cowboys"),
                        list("Vancouver Blazers", "Calgary Cowboys"), list("Oakland Seals", "Cleveland Barons"), list("California Golden Seals", "Cleveland Barons"),
                        list("New England Whalers", "Carolina Hurricanes"), list("Hartford Whalers", "Carolina Hurricanes"), list("Chicago Black Hawks", "Chicago Blackhawks"),
                        list("Quebec Nordiques", "Colorado Avalanche"), list("Kansas City Scouts", "New Jersey Devils"), list("Colorado Rockies", "New Jersey Devils"),
                        list("Minnesota North Stars", "Dallas Stars"), list("Detroit Cougars", "Detroit Red Wings"), list("Detroit Falcons", "Detroit Red Wings"),
                        list("Los Angeles Sharks", "Michigan Stags/Baltimore Blades"), list("New York Raiders", "San Diego Mariners"), list("New York Golden Blades/New Jersey Knights",
                                                                                                                                            "San Diego Mariners"), list("Pittsburgh Pirates", "Philadelphia Quakers"), list("Toronto Arenas", "Toronto Maple Leafs"), list("Toronto St. Patricks",
                                                                                                                                                                                                                                                                           "Toronto Maple Leafs"), list("Ottawa Senators (historical)", "St. Louis Eagles"))

    # ReType frame
    message("retype frame")
    hockeyData <- unique(hockeyData)
    try(hockeyData <- subset(hockeyData, select = -LOG), silent=TRUE)
    try(hockeyData <- subset(hockeyData, select = -X), silent=TRUE)
    hockeyData$Date <- as.Date(hockeyData$Date)
    hockeyData <- hockeyData[order(hockeyData$Date, hockeyData$League),]
    if ('Att.' %in% names(hockeyData)){
        hockeyData$Att. <- as.integer(hockeyData$Att.)
    }
    names(hockeyData)[names(hockeyData) == "G"] <- "VisitorGoals"
    names(hockeyData)[names(hockeyData) == "G.1"] <- "HomeGoals"
    names(hockeyData)[names(hockeyData) == "X.1"] <- "OTStatus"
    hockeyData$OTStatus <- as.factor(hockeyData$OTStatus)
    hockeyData$League <- as.factor(hockeyData$League)

    if (identifyTies) {
        hockeyData$Tie <- FALSE
        hockeyData[hockeyData$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT", "SO"), ]$Tie <- TRUE
    }

    # Remove games against international teams
    if (removeInternational) {
        message("dropping international games")
        hockeyData <- hockeyData[!(hockeyData$Visitor %in% c("Soviet All-Stars", "Czechoslovakia","Finland")), ]
    }

    message("dropping unplayed games - future or past cancelled")
    hockeyData <- hockeyData[!is.na(hockeyData$VisitorGoals), ]

    if (cleanTeams) {
        # Special Casing out the various teams with repeat existances
        message("special cases")
        levels(hockeyData$Home) <- c(levels(hockeyData$Home), "Winnipeg Jets (historical)")
        levels(hockeyData$Visitor) <- c(levels(hockeyData$Visitor), "Winnipeg Jets (historical)")
        levels(hockeyData$Home) <- c(levels(hockeyData$Home), "Ottawa Senators (historical)")
        levels(hockeyData$Visitor) <- c(levels(hockeyData$Visitor), "Ottawa Senators (historical)")

        try(hockeyData[hockeyData$Visitor == "Winnipeg Jets" & hockeyData$Date < as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Winnipeg Jets (historical)", silent=TRUE)
        try(hockeyData[hockeyData$Home == "Winnipeg Jets" & hockeyData$Date < as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Home <- "Winnipeg Jets (historical)", silent=TRUE)
        try(hockeyData[hockeyData$Visitor == "Ottawa Senators" & hockeyData$Date < as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Ottawa Senators (historical)", silent=TRUE)
        try(hockeyData[hockeyData$Home == "Ottawa Senators" & hockeyData$Date < as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Home <- "Ottawa Senators (historical)", silent=TRUE)

        message("reguar replacements")
        for (t in teamReplace) {
            try(hockeyData[hockeyData$Visitor == t[1], ]$Visitor <- t[2], silent=TRUE)
            try(hockeyData[hockeyData$Home == t[1], ]$Home <- t[2], silent=TRUE)
        }

        try(hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & hockeyData$Visitor == "Minnesota Fighting Saints", ]$Visitor <- "Cleveland Crusaders", silent=TRUE)
        try(hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & hockeyData$Home == "Minnesota Fighting Saints", ]$Home <- "Cleveland Crusaders", silent=TRUE)

        hockeyData$Visitor <- droplevels(hockeyData$Visitor)
        hockeyData$Home <- droplevels(hockeyData$Home)
    }

    if (listWinnersLosers) {
        message("find winners and losers")
        hockeyData$Winner <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] > x[5], x[2], x[4])))
        hockeyData$Loser <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] <= x[5], x[2], x[4])))
    }

    if (eloResults) {
        message('calculating Elo results')
        hockeyData$Result <- apply(hockeyData, 1, function(x) tieSort(x))
        hockeyData$Diff <- abs(hockeyData$HomeGoals - hockeyData$VisitorGoals)
    }

    return(hockeyData)
}

#' Load data from directory, and pass to prepareEloData if requested. Returns a fully processed
#' data set ready for elo analysis
#'
#' @param data_dir Directory where data is stored
#' @param nhl_year_list NHL Years to import data. Will automatically skip missing seasons
#' @param wha_year_list WHA Years to import data.
#' @param playoffs Whether or not to import playoff games
#' @param last_playoffs Whether or not to import the playoffs for the latest (current?) season
#' @param ... Parameters for prepareEloData()
#'
#' @return a data frame containing processed NHL data for the selected parameters
loadEloData <- function(data_dir="./_data", nhl_year_list = c(1918:2017), wha_year_list = c(1973:1979), playoffs = TRUE, lastPlayoffs = FALSE, ...) {
    df_nhl <- data.frame(Date = NULL, Visitor = NULL, G = NULL, Home = NULL, G.1 = NULL, X.1 = NULL)
    df_wha <- df_nhl
    nhl_year_list<-nhl_year_list[nhl_year_list != 2005]
    message('reading NHL data')
    for (year in 1:length(nhl_year_list)) {
        df_nhl <- rbind(df_nhl, read.csv(paste("./_data/", nhl_year_list[year] - 1, nhl_year_list[year], ".csv", sep = ""))[2:7])
    }
    if (playoffs) {
        for (year in 1:(length(nhl_year_list) - 1)) {
            if (nhl_year_list[year] != 1920){df_nhl <- rbind(df_nhl, read.csv(paste("./_data/", nhl_year_list[year] - 1, nhl_year_list[year], "Playoffs.csv", sep = ""))[2:7])}
        }
        if (lastPlayoffs) {
            df_nhl <- rbind(df_nhl, read.csv(paste("./_data/", nhl_year_list[length(nhl_year_list)] - 1, nhl_year_list[length(nhl_year_list)], "Playoffs.csv", sep = ""))[2:7])
        }
    }

    df_nhl$League<-"NHL"

    message('reading WHA data')
    for (year in 1:length(wha_year_list)) {
        df_wha <- rbind(df_wha, read.csv(paste("./_data/wha", wha_year_list[year] - 1, wha_year_list[year], ".csv", sep = ""))[2:7])
    }
    if (playoffs) {
        for (year in 1:(length(wha_year_list))) {
            df_wha <- rbind(df_wha, read.csv(paste("./_data/wha", wha_year_list[year] - 1, wha_year_list[year], "Playoffs.csv", sep = ""))[2:7])
        }
    }

    df_wha$League<-"WHA"

    df<-rbind(df_nhl, df_wha)

    df <- prepareEloData(hockeyData = df, ...)
    return(df)
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

#' Helper function for incoming data
tieSort<-function(x) {
    if (as.numeric(x['VisitorGoals']) > as.numeric(x['HomeGoals'])){
        if (x['OTStatus'] %in% c("SO")){
            return(0.4)
        }
        else if (x['OTStatus'] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")){
            return(0.25)
        }
        else{
            return(0.0)
        }
    }
    else if (as.numeric(x['VisitorGoals']) < as.numeric(x['HomeGoals'])){
        if (x['OTStatus'] %in% c("SO")){
            return(0.6)
        }
        else if (x['OTStatus'] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")){
            return(0.75)
        }
        else{
            return(1.0)
        }
    }
    else if (x[3] == x[5]){
        return(0.5)
    }
}
