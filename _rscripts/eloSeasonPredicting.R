
predictOneGame <- function(elo_diff, pWin, pLoss) {
    w<-predict(pWin, data.frame("EloDiff"=elo_diff), type='response')
    l<-predict(pLoss, data.frame("EloDiff"=elo_diff), type='response')

    r<-runif(1)

    if(r <= w){  # Home Win
        return(1)
    }
    else if (r <= w+l){  # Visitor Win
        return (0)
    }
    else{
        pw<-(w-w*l)/(w+l-2*w*l)
        if (runif(1)>0.6){ #approximate percentage of shootout vs. OT
            ifelse(r <= pw, return(0.6), return(0.4))
        }
        else{
            ifelse(r <= pw, return(0.75), return(0.25))
        }
    }
}

makeStatsTable <- function(df) {
    tmpTable = data.frame(Team = sort(unique(df$Visitor)), GP = 0, W = 0, OTL = 0, L = 0, ROW = 0, HomeGames = 0, HomeWin = 0, HomeOTW = 0, HomeSOW = 0,
        HomeOTL = 0, HomeLoss = 0, VisitorGames = 0, VisitorWin = 0, VisitorOTW = 0, VisitorSOW = 0, VisitorOTL = 0, VisitorLoss = 0, P = 0, PPG = 0, OT.Win.Percent = 0)

    # Games Played
    tmpTable$HomeGames = as.numeric(table(df$Home))
    tmpTable$VisitorGames = as.numeric(table(df$Visitor))

    # Wins
    tmpTable$HomeWin = as.numeric(table(df$Home[df$Result == 1]))
    tmpTable$VisitorWin = as.numeric(table(df$Visitor[df$Result == 0]))

    # Losses
    tmpTable$HomeLoss = as.numeric(table(df$Home[df$Result == 0]))
    tmpTable$VisitorLoss = as.numeric(table(df$Visitor[df$Result == 1]))

    # OT Wins
    tmpTable$HomeOTW = as.numeric(table(df$Home[df$Result == 0.75]))
    tmpTable$HomeSOW = as.numeric(table(df$Home[df$Result == 0.60]))

    tmpTable$VisitorOTW = as.numeric(table(df$Visitor[df$Result == 0.25]))
    tmpTable$VisitorSOW = as.numeric(table(df$Visitor[df$Result == 0.40]))

    # OT Losses
    tmpTable$HomeOTL = as.numeric(table(df$Home[df$Result == 0.25]))
    tmpTable$VisitorOTL = as.numeric(table(df$Visitor[df$Result == 0.75]))

    # W/L/OTL/ROW
    tmpTable$GP = tmpTable$HomeGames + tmpTable$VisitorGames
    tmpTable$W = tmpTable$HomeWin + tmpTable$VisitorWin + tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$VisitorOTW + tmpTable$VisitorSOW
    tmpTable$OTL = tmpTable$HomeOTL + tmpTable$VisitorOTL
    tmpTable$L = tmpTable$HomeLoss + tmpTable$VisitorLoss
    tmpTable$ROW = tmpTable$W - (tmpTable$HomeSOW + tmpTable$VisitorSOW)

    # Additional Stats
    tmpTable$P = (2 * tmpTable$W) + tmpTable$OTL
    tmpTable$PPG = tmpTable$P/tmpTable$GP
    tmpTable$OT.Win.Percent = (tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$VisitorOTW + tmpTable$VisitorSOW)/(tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$VisitorOTW +
        tmpTable$VisitorOTL + tmpTable$OTL)
    tmpTable <- tmpTable[, c("Team", "GP", "W", "OTL", "L", "ROW", "P", "PPG", "OT.Win.Percent")]
    tmpTable <- tmpTable[order(-tmpTable$P, -tmpTable$PPG, -tmpTable$ROW), ]

    rownames(tmpTable) <- 1:nrow(tmpTable)

    return(tmpTable)
}


buildStandingsTable <- function(stats, standings = NULL) {
    if (is.null(standings)) {
        standings <- matrix(0, nrow = length(unique(stats$Team)), ncol = length(unique(stats$Team)))
        rownames(standings) <- sort(unique(stats$Team))
        colnames(standings) <- c(1:ncol(standings))
    }

    for (t in 1:nrow(standings)) {
        standings[stats[t, "Team"], t] <- standings[stats[t, "Team"], t] + 1
    }
    return(standings)
}

nhlFutureGames <- function(df, date=Sys.Date()) {
    df$Date <- as.Date(df$Date)
    df <- df[!(df$Date < date), ]
    colnames(df) <- c("Date", "Home", "Visitor", "Result")
    return(df)
}

#Schedule as Date, HomeTeam, VisitorTeam, Result
predictRemainderOfSeason <- function(schedule, eloHist, pWin, pLoss) {
    # Takes in a schedule of games and returns the schedule with scores filled out.
    for (game in 1:nrow(schedule)) {
        home <- make.names(as.character(schedule[game, "Home"]))
        visitor <- make.names(as.character(schedule[game, "Visitor"]))
        schedule[game, 'Result'] <- predictOneGame((tail(eloHist[,home],1)-tail(eloHist[,visitor],1)),pWin, pLoss)
    }
    return(schedule)
}

simulateSeason <- function(schedule, past_results, eloHist, pWin, pLoss, n = 10000) {
    # simulates the (remainder of the) season n times, returning a standings table with the times each team finished at each position
    standings <- matrix(0, nrow = length(unique(schedule$Home)), ncol = length(unique(schedule$Home)))
    rownames(standings) <- sort(unique(schedule$Home))
    colnames(standings) <- c(1:ncol(standings))
    for (i in 1:n) {
        scores <- predictRemainderOfSeason(schedule, eloHist, pWin, pLoss)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        standings <- buildStandingsTable(stats = stats_table, standings = standings)
    }
    return(standings)
}

nhl_divisions <- list(Atlantic = c("Boston Bruins", "Buffalo Sabres", "Detroit Red Wings", "Florida Panthers", "Montreal Canadiens", "Ottawa Senators",
    "Tampa Bay Lightning", "Toronto Maple Leafs"), Central = c("Colorado Avalanche", "Chicago Blackhawks", "Dallas Stars", "Minnesota Wild", "Nashville Predators",
    "St. Louis Blues", "Winnipeg Jets"), Metropolitan = c("Carolina Hurricanes", "Columbus Blue Jackets", "Philadelphia Flyers", "Pittsburgh Penguins",
    "New Jersey Devils", "New York Islanders", "New York Rangers", "Washington Capitals"), Pacific = c("Anaheim Ducks", "Arizona Coyotes", "Calgary Flames",
    "Edmonton Oilers", "Los Angeles Kings", "San Jose Sharks", "Vancouver Canucks"))

nhl_conferences <- list(East = c(unlist(nhl_divisions["Atlantic"]), unlist(nhl_divisions["Metropolitan"])), West = c(unlist(nhl_divisions["Central"]),
    unlist(nhl_divisions["Pacific"])))


getDivisionStats <- function(stats, division) {
    return(stats[stats$Team %in% unlist(nhl_divisions[division]), ])
}

getConferenceStats <- function(stats, conference) {
    if (conference == "East") {
        a <- getDivisionStats(stats, "Atlantic")
        b <- getDivisionStats(stats, "Metropolitan")
    } else {
        a <- getDivisionStats(stats, "Central")
        b <- getDivisionStats(stats, "Pacific")
    }
    top6 <- rbind(a[1:3], b[1:3])
    top6 <- top6[order(-top6$P, -top6$PP, -top6$ROW, -top6$DIFF), ]
    remainder <- rbind(a[4:nrow(a), ], b[4:nrow(b), ])
    remainder <- remainder[order(-remainder$P, -remainder$PP, -remainder$ROW, -remainder$DIFF)]

    return(rbind(top6, remainder))
}

getConferenceStandings <- function(standings, conference) {
    # Change this to sort by MAX of standings
    standings <- as.data.frame(standings)
    standings$Team <- rownames(standings)
    if (conference == "East") {
        a <- getDivisionStats(standings, "Atlantic")
        b <- getDivisionStats(standings, "Metropolitan")
    } else {
        a <- getDivisionStats(standings, "Central")
        b <- getDivisionStats(standings, "Pacific")
    }
    standings <- rbind(a, b)
    standings <- subset(standings, select = -Team)
    standings <- standings[do.call(order, c(as.list(standings), decreasing = TRUE)), ]
    return(as.matrix(standings))
}

new_stdev <- function(stdev, mean, pop, new_val) {
    n_m = new_mean(mean, pop, new_val)
    return(sqrt((((pop - 2) * stdev^2) + (new_val - n_m) * (new_val - mean))/(pop - 1)))
}

v_new_stdev <- Vectorize(new_stdev, c("stdev", "mean", "new_val"))

new_mean <- function(mean, pop, new_val) {
    return((mean * pop + new_val)/(pop + 1))
}

v_new_mean <- Vectorize(new_mean, c("mean", "new_val"))

point_predict <- function(res, schedule, stats, past_results, n = 10000, maxgoal = 8, m = NULL) {
    pp <- matrix(0, nrow = length(unique(stats$Team)), ncol = 6)
    rownames(pp) <- sort(unique(stats$Team))
    colnames(pp) <- c("Points", "Points_StDev", "Playoffs", "Playoffs_StDev", "Presidents", "Presidents_StDev")

    scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
    stats_table <- makeStatsTable(rbind(past_results, scores))
    standings <- buildStandingsTable(stats_table)
    pp[, "Points"] <- stats_table[order(stats_table$Team), ]$P
    playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, ]), rownames(getConferenceStandings(standings, "West")[1:8, ]))
    pp[playoff_list, "Playoffs"] <- 1
    pp[names(which(standings[, "1"] == 1, arr.ind = TRUE)), "Presidents"] <- 1

    if (n == 2) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        stgs <- buildStandingsTable(stats_table)
        standings <- buildStandingsTable(stats = stats_table, standings = standings)
        playoff_list <- c(rownames(getConferenceStandings(stgs, "East")[1:8, ]), rownames(getConferenceStandings(stgs, "West")[1:8, ]))

        pp[, "Points_StDev"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), ]$P), 1, sd)
        pp[, "Points"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), ]$P), 1, mean)
        pp[, "Playoffs_StDev"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% playoff_list), 1, sd)
        pp[, "Playoffs"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% playoff_list), 1, mean)
        pp[, "Presidents_StDev"] <- apply(cbind(pp[, "Presidents"], stgs[, "1"]), 1, sd)
        pp[, "Presidents"] <- apply(cbind(pp[, "Presidents"], stgs[, "1"]), 1, mean)
    } else if (n > 2) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        stgs <- buildStandingsTable(stats_table)
        standings <- buildStandingsTable(stats = stats_table, standings = standings)
        playoff_list <- c(rownames(getConferenceStandings(stgs, "East")[1:8, ]), rownames(getConferenceStandings(stgs, "West")[1:8, ]))

        scores2 <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
        stats_table2 <- makeStatsTable(rbind(past_results, scores2))
        stgs2 <- buildStandingsTable(stats_table)
        standings <- buildStandingsTable(stats = stats_table, standings = standings)
        playoff_list2 <- c(rownames(getConferenceStandings(stgs2, "East")[1:8, ]), rownames(getConferenceStandings(stgs2, "West")[1:8, ]))

        pp[, "Points_StDev"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), ]$P, stats_table2[order(stats_table2$Team), ]$P),
            1, sd)
        pp[, "Points"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), ]$P, stats_table2[order(stats_table2$Team), ]$P), 1, mean)
        pp[, "Playoffs_StDev"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% playoff_list, rownames(pp) %in% playoff_list2), 1, sd)
        pp[, "Playoffs"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% playoff_list, rownames(pp) %in% playoff_list2), 1, mean)
        pp[, "Presidents_StDev"] <- apply(cbind(pp[, "Presidents"], stgs[, "1"], stgs2[, "1"]), 1, sd)
        pp[, "Presidents"] <- apply(cbind(pp[, "Presidents"], stgs[, "1"], stgs2[, "1"]), 1, mean)
        if (n > 3) {
            for (i in 4:n) {
                scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
                stats_table <- makeStatsTable(rbind(past_results, scores))
                standings <- buildStandingsTable(stats = stats_table, standings = standings)
                stgs <- buildStandingsTable(stats_table)
                playoff_list <- c(rownames(getConferenceStandings(stgs, "East")[1:8, ]), rownames(getConferenceStandings(stgs, "West")[1:8, ]))

                pp[, "Points_StDev"] <- v_new_stdev(pp[, "Points_StDev"], pp[, "Points"], i - 1, stats_table[order(stats_table$Team), ]$P)
                pp[, "Points"] <- v_new_mean(pp[, "Points"], i - 1, stats_table[order(stats_table$Team), ]$P)
                pp[, "Playoffs_StDev"] <- v_new_stdev(pp[, "Playoffs_StDev"], pp[, "Playoffs"], i - 1, rownames(pp) %in% playoff_list)
                pp[, "Playoffs"] <- v_new_mean(pp[, "Playoffs"], i - 1, rownames(pp) %in% playoff_list)
                pp[, "Presidents_StDev"] <- v_new_stdev(pp[, "Presidents_StDev"], pp[, "Presidents"], i - 1, stgs[, "1"])
                pp[, "Presidents"] <- v_new_mean(pp[, "Presidents"], i - 1, stgs[, "1"])
            }
        }
    }
    return(list(standings, pp))
}

