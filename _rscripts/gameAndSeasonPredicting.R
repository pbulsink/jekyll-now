log5OTPredictor <- function(stats, home, away) {
    # reurns chances that home team wins in OT
    pa <- stats[stats$Team == home, ]$OT.Win.Percent
    pb <- stats[stats$Team == away, ]$OT.Win.Percent

    # PA or PB = 0 gives a NAN/div0 error
    if (pa == 0) {
        pa = 0.001
    }
    if (pb == 0) {
        pb == 0.001
    }

    log5 <- (pa - (pa * pb))/(pa + pb - (2 * pa * pb))
    if (!is.na(log5)) {
        return(log5)
    } else {
        return(0.5)
    }
}

buildScoreMatrix <- function(res, home, away, maxgoal = 8, m = NULL) {
    if (!is.null(m)) {
        # Expected goals home
        lambda <- predict(m, data.frame(Home = 1, Team = home, Opponent = away), type = "response")

        # Expected goals away
        mu <- predict(m, data.frame(Home = 0, Team = away, Opponent = home), type = "response")

        # rho
        rho <- res$par
    } else {
        attack.home <- paste("Attack", home, sep = ".")
        attack.away <- paste("Attack", away, sep = ".")
        defence.home <- paste("Defence", home, sep = ".")
        defence.away <- paste("Defence", away, sep = ".")

        # Expected goals home
        lambda <- exp(res$par["HOME"] + res$par[attack.home] + res$par[defence.away])
        # Expected goals away
        mu <- exp(res$par[attack.away] + res$par[defence.home])
        rho <- res$par["RHO"]
    }
    probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))

    scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, rho), nrow = 2)
    probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

    pmatrix <- matrix(nrow = nrow(probability_matrix), ncol = ncol(probability_matrix))
    current_p <- 0
    for (i in 1:ncol(pmatrix)) {
        for (j in 1:nrow(pmatrix)) {
            pmatrix[j, i] <- current_p
            current_p <- current_p + probability_matrix[j, i]
        }
    }
    return(pmatrix)
}

predictOneGame <- function(pmatrix, stats, home, away) {
    random <- runif(1)
    # Ensure random isn't higher than matrix sum (calculation at less than indexed).
    while (random > pmatrix[nrow(pmatrix), ncol(pmatrix)]) {
        random <- runif(1)
    }
    score <- as.vector(which(pmatrix > random, arr.ind = T)[1, ])
    # scores is matrix c(home away) as INDEXES (eg. 0-0 score is 1,1)
    score <- score - 1
    score[3] <- NA
    if (score[1] == score[2]) {
        log5 <- log5OTPredictor(stats, home, away)
        if (log5 > runif(1)) {
            score[1] <- score[1] + 1
        } else {
            score[2] <- score[2] + 1
        }
        if (runif(1) > 0.5) {
            score[3] <- "OT"
        } else {
            score[3] <- "SO"
        }
    }
    return(score)
}

makeStatsTable <- function(df) {
    tmpTable = data.frame(Team = sort(unique(df$AwayTeam)), GP = 0, W = 0, OTL = 0, L = 0, ROW = 0, HomeGames = 0, HomeWin = 0, HomeOTW = 0, HomeSOW = 0,
        HomeOTL = 0, HomeLoss = 0, AwayGames = 0, AwayWin = 0, AwayOTW = 0, AwaySOW = 0, AwayOTL = 0, AwayLoss = 0, P = 0, HomeFor = 0, HomeAgainst = 0,
        AwayFor = 0, AwayAgainst = 0, GF = 0, GA = 0, DIFF = 0, PPG = 0, OT.Win.Percent = 0)

    # Games Played
    tmpTable$HomeGames = as.numeric(table(df$HomeTeam))
    tmpTable$AwayGames = as.numeric(table(df$AwayTeam))

    # Wins
    tmpTable$HomeWin = as.numeric(table(df$HomeTeam[df$HG > df$AG]))
    tmpTable$AwayWin = as.numeric(table(df$AwayTeam[df$AG > df$HG]))

    # Losses
    tmpTable$HomeLoss = as.numeric(table(df$HomeTeam[df$AG > df$HG]))
    tmpTable$AwayLoss = as.numeric(table(df$AwayTeam[df$HG > df$AG]))

    # OT Wins
    tmpTable$HomeOTW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == "OT")]))
    tmpTable$HomeSOW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == "SO")]))

    tmpTable$AwayOTW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == "OT")]))
    tmpTable$AwaySOW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == "SO")]))

    # OT Losses
    tmpTable$HomeOTL = as.numeric(table(df$HomeTeam[(df$OT.Win == "V")]))
    tmpTable$AwayOTL = as.numeric(table(df$AwayTeam[(df$OT.Win == "H")]))

    # W/L/OTL/ROW
    tmpTable$GP = tmpTable$HomeGames + tmpTable$AwayGames
    tmpTable$W = tmpTable$HomeWin + tmpTable$AwayWin + tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$OTL = tmpTable$HomeOTL + tmpTable$AwayOTL
    tmpTable$L = tmpTable$HomeLoss + tmpTable$AwayLoss
    tmpTable$ROW = tmpTable$W - (tmpTable$HomeSOW + tmpTable$AwaySOW)

    # Goal Diffs (includes OT scores)
    tmpTable$HomeFor = as.numeric(tapply(df$HG, df$HomeTeam, sum, na.rm = TRUE)) + tmpTable$HomeOTW + tmpTable$HomeSOW
    tmpTable$HomeAgainst = as.numeric(tapply(df$AG, df$HomeTeam, sum, na.rm = TRUE)) + tmpTable$HomeOTL

    tmpTable$AwayFor = as.numeric(tapply(df$AG, df$AwayTeam, sum, na.rm = TRUE)) + tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$AwayAgainst = as.numeric(tapply(df$HG, df$AwayTeam, sum, na.rm = TRUE)) + tmpTable$AwayOTL


    tmpTable$GF = ifelse(is.na(tmpTable$HomeFor), 0, tmpTable$HomeFor) + ifelse(is.na(tmpTable$AwayFor), 0, tmpTable$AwayFor)
    tmpTable$GA = ifelse(is.na(tmpTable$HomeAgainst), 0, tmpTable$HomeAgainst) + ifelse(is.na(tmpTable$AwayAgainst), 0, tmpTable$AwayAgainst)

    tmpTable$DIFF = tmpTable$GF - tmpTable$GA

    # Additional Stats
    tmpTable$P = (2 * tmpTable$W) + tmpTable$OTL
    tmpTable$PPG = tmpTable$P/tmpTable$GP
    tmpTable$OT.Win.Percent = (tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + tmpTable$AwaySOW)/(tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW +
        tmpTable$AwayOTL + tmpTable$OTL)
    tmpTable <- tmpTable[, c("Team", "GP", "W", "OTL", "L", "ROW", "P", "GF", "GA", "DIFF", "PPG", "OT.Win.Percent")]
    tmpTable <- tmpTable[order(-tmpTable$P, -tmpTable$PPG, -tmpTable$ROW, -tmpTable$DIFF), ]

    rownames(tmpTable) <- 1:nrow(tmpTable)

    return(tmpTable)
}

# stats2015<-makeStatsTable(nhl2015) stats_all<-makeStatsTable(nhl_all)

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

predictScore <- function(res, stats, home, away, maxgoal = 8, m = NULL) {
    return(predictOneGame(buildScoreMatrix(res, home, away, maxgoal = maxgoal, m = m), stats = stats, home, away))
}

nhlFutureGames <- function(df) {
    df <- df[, c("Date", "Visitor", "G", "Home", "G.1")]
    df$Date <- as.Date(df$Date)
    df <- df[!(df$Date < Sys.Date()), ]
    df$OT.SO <- ""
    df$OT.Win <- ""
    colnames(df) <- c("Date", "AwayTeam", "AG", "HomeTeam", "HG", "OT.SO", "OT.Win")
    return(df)
}

# nhl_2016_standings<-buildStandingsTable(nhl_2016_stats)

predictRemainderOfSeason <- function(res, schedule, stats, maxgoal = 8, m = NULL) {
    # Takes in a schedule of games and returns the schedule with scores filled out.
    for (game in 1:nrow(schedule)) {
        home <- as.character(schedule[game, "HomeTeam"])
        away <- as.character(schedule[game, "AwayTeam"])
        score <- predictScore(res, stats, home, away, maxgoal = maxgoal, m = m)
        if (!is.na(score[3])) {
            schedule[game, "OT.SO"] <- score[3]
            if (score[1] > score[2]) {
                schedule[game, "HG"] <- score[2]
                schedule[game, "AG"] <- score[2]
                schedule[game, "OT.Win"] <- "H"
            } else {
                schedule[game, "HG"] <- score[1]
                schedule[game, "AG"] <- score[1]
                schedule[game, "OT.Win"] <- "V"
            }
        } else {
            schedule[game, "HG"] <- score[1]
            schedule[game, "AG"] <- score[2]
        }
    }
    schedule$HG <- as.integer(schedule$HG)
    schedule$AG <- as.integer(schedule$AG)
    return(schedule)
}

simulateSeason <- function(res, schedule, stats, past_results, n = 10000, maxgoal = 8, m = NULL) {
    # simulates the (remainder of the) season n times, returning a standings table with the times each team finished at each position
    standings <- matrix(0, nrow = length(unique(stats$Team)), ncol = length(unique(stats$Team)))
    rownames(standings) <- sort(unique(stats$Team))
    colnames(standings) <- c(1:ncol(standings))
    for (i in 1:n) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, maxgoal = maxgoal, m = m)
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
