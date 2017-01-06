require(RCurl)
#require(XML)
require(stringr)
require(plyr)

#' Get a list of Players
#' Download the full list of active and past players from Hockey-Reference.com
#'
#' @param sleep The sleep time between scrape requests
#'
#' @return a data.frame containing:
#' \item{Complete}{The complete line that data was scraped from}
#' \item{BlnNHL}{Whether the player played any time in the NHL}
#' \item{URL}{Player page URL}
#' \item{Name}{Player Name}
#' \item{Active}{Whether the player is currently active}

getPlayerList <- function(sleep = 30) {
  pattern <- "<p class=\"([a-z\\_]+)\">(?:<strong>)*<a href=\"(\\/players\\/[a-z]+\\/[a-zA-Z0-9]+\\.html)\">([a-zA-Z ]+)<\\/a>(?:<\\/strong>)*\\s*\\(([0-9-]+)*"
  player_list <- data.frame(Complete = character(), BlnNHL = character(),
    URL = character(), Name = character(), Active = character())
  for (letter in letters) {
    message(letter)
    url <- paste0("http://www.hockey-reference.com/players/",
      letter, "/")
    raw_player_list <- getURLInternal(url)
    pl <- str_match_all(raw_player_list, pattern)
    pl <- as.data.frame(pl[1], stringsAsFactors = FALSE)
    colnames(pl) <- c("Complete", "BlnNHL", "URL", "Name",
      "Active")
    player_list <- rbind(player_list, pl)
    Sys.sleep(sleep)
  }
  player_list[player_list$BlnNHL == "nhl", "BlnNHL"] <- TRUE
  player_list[player_list$BlnNHL == "non_nhl", "BlnNHL"] <- FALSE
  player_list$BlnNHL <- as.factor(player_list$BlnNHL)
  return(player_list)
}

#' Extra wrapped getURL
#' \code{\link[RCurl]{getURL}} wrapped in useragent sampling and try/retry, with referer.
#'
#' @param url The URL to try download
#' @param referer A referer source
#'
#' @return raw url contents output from getURL as the HTTP reply from the server
getURLInternal <- function(url, referer = "hockey-reference.com") {
  agents <- c("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
  htmlpage <- try(getURL(url, header = FALSE, .opts = curlOptions(referer = referer,
    header = TRUE, followLocation = TRUE, useragent = agents[sample(1:8,
      1)])))

  if (class(htmlpage) == "try-error") {
    message(paste0("HTML Try Error on: ", url))
    htmlpage <- getURL(url, header = FALSE, .opts = curlOptions(referer = referer,
      header = TRUE, followLocation = TRUE, useragent = agents[sample(1:8,
        1)]))
  }
  return(htmlpage)
}

#' Get player info from a page
#' A function to manage scraping all of the info from Hockey-Reference.com player pages.
#' Also perform collection player meta info (name, birth, etc)
#'
#' @param url The player URL
#'
#' @return a list of data.frames containing
#' \item{Tables}{Player Statistics Tables from HTML Page}
#' \item{Metas}{Player Meta information (Name, Birth, Height, Weight, Shot, etc.)}
getPlayerInfo <- function(url) {
  htmlpage <- getURLInternal(url)
  htmlpage <- gsub(htmlpage, pattern = "<!--", replacement = "")
  htmlpage <- gsub(htmlpage, pattern = "-->", replacement = "")

  # Read in Tables
  tables <- readHTMLTable(htmlpage)

  m1 <- "<p><strong>Position<\\/strong>:\\s*([A-Z\\/]+)\\&*"
  meta_pos <- str_match(htmlpage, m1)[, 2]
  names(meta_pos) <- "Position"

  m1b <- "<strong>(?:Shoots|Catches)<\\/strong>:\\s*([A-Za-z\\/]+)\\s*"
  meta_hand <- str_match(htmlpage, m1b)[, 2]
  names(meta_hand) <- "Handed"

  m2 <- "<p><span itemprop=\"height\">([0-9-]+)<\\/span>.+itemprop=\"weight\">([0-9]+)lb.+\\(([0-9]+)cm,.+;([0-9]+)kg\\).<\\/p>"
  meta_h_w <- str_match(htmlpage, m2)[, c(2:5)]
  names(meta_h_w) <- c("HeightImp", "WeightImp", "HeightMetric",
    "WeightMetric")

  m3 <- "data-birth=\"([0-9-]*)\"+>.+\"birthPlace\">\\s*in\\&nbsp;([A-Za-z\\.(?:\\&nbsp;)]*),.+country=([A-Za-z\\.(?:\\&nbsp;)]*).+province=([A-Za-z\\.(?:\\&nbsp;)]*).+state=([A-Za-z\\.(?:\\&nbsp;)]*)\""
  meta_birth <- str_match(htmlpage, m3)[2:6]
  names(meta_birth) <- c("Birthdate", "BirthPlace", "Country",
    "Province", "State")

  m4 <- "data-death=\"([0-9-]*)\""
  meta_death <- str_match(htmlpage, m4)[[1]][2]
  names(meta_death) <- c("Deathdate")

  m5 <- "draft.html\">([A-Za-z]+)<\\/a>,\\s*([0-9A-Za-z]+)\\s*round\\s*\\(([0-9]+)[a-z]{2}\\&nbsp;overall\\), <[a-zA-Z\\s\\/=\"+_0-9]+\\.html\">([0-9]{4})"
  meta_draft <- str_match_all(htmlpage, m5)[[1]][, c(2:5)]
  if (is.null(nrow(meta_draft)) || nrow(meta_draft) > 0) {
    # handles redrafted players (about 30 players)
    if (class(meta_draft) == "matrix") {
      m = character()
      for (i in 1:nrow(meta_draft)) m <- c(m, meta_draft[i,
        ])
      meta_draft <- m
    }
    names(meta_draft) <- c("DraftTeam", "DraftRound", "DraftOverall",
      "DraftYear", rep(c("ReDraftTeam", "ReDraftRound",
        "ReDraftOverall", "ReDraftYear"), times = ((length(meta_draft) -
        4)/4)))

    if (length(meta_draft) > 8) {
      for (i in 9:length(meta_draft)) {
        names(meta_draft)[i] <- paste0(names(meta_draft)[i],
          (i - 4)%/%4)
      }
    }
  }

  # Stitch it together
  metas <- unlist(list(meta_pos, meta_hand, meta_h_w, meta_birth,
    meta_death, meta_draft))

  return(list(Tables = tables, Metas = metas))
}

#' Scrape & compile a player's info
#' Read each table on hockey-reference.com and flatten to single useful tables.
#' Avoids tables such as last5, awards, etc.
#' Melts all NHL (regular season and playoff) and non-NHL, recording league and playoff status
#'
#' @param tables Tables output from readHTMLTables of the player's page
#'
#' @return single combined stats Table as data.frame
flattenTables <- function(tables) {
  stats_nhl <- data.frame()
  if ("stats_basic_plus_nhl" %in% names(tables)) {
    stats_nhl <- tables$stats_basic_plus_nhl
    stats_nhl$Playoffs = FALSE
    # Sometimes there's '' named columns. Messes up rbind.fill
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in%
        "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  } else if ("stats_basic_nhl" %in% names(tables)) {
    stats_nhl <- tables$stats_basic_nhl
    stats_nhl$Playoffs = FALSE
    # Sometimes there's '' named columns. Messes up rbind.fill
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in%
        "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  }
  colnames(stats_nhl) <- make.unique(colnames(stats_nhl))
  colnames(stats_nhl)[colnames(stats_nhl) == "EV.1"] <- "EV.Assists"
  colnames(stats_nhl)[colnames(stats_nhl) == "PP.1"] <- "PP.Assists"
  colnames(stats_nhl)[colnames(stats_nhl) == "SH.1"] <- "SH.Assists"

  if ("skaters_advanced" %in% names(tables)) {
    advnhl <- tables$skaters_advanced
    stats_nhl <- merge(stats_nhl, advnhl, by = c("Season",
      "Team"), all = TRUE)
    if ("Age.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -Age.y)
    if ("Lg.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -Lg.y)
    if ("GP.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -GP.y)
    if ("TOI.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -TOI.y)

    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in%
        "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Age.x"] <- "Age"
    colnames(stats_nhl)[colnames(stats_nhl) == "Lg.x"] <- "Lg"
    colnames(stats_nhl)[colnames(stats_nhl) == "GP.x"] <- "GP"
    colnames(stats_nhl)[colnames(stats_nhl) == "TOI.x"] <- "TOI"
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  }

  if ("stats_misc_nhl" %in% names(tables)) {
    stmisc <- tables$stats_misc_nhl
    colnames(stmisc)[colnames(stmisc) == "Tm"] <- "Team"
    stmisc <- subset(stmisc, select = -c(7:12))
    stmisc <- subset(stmisc, select = c(Season, Age, Team,
      Lg, GC, G, A, PTS, GC.1, OPS, DPS, PS))
    colnames(stmisc)[colnames(stmisc) == "G"] <- "Adj.G"
    colnames(stmisc)[colnames(stmisc) == "A"] <- "Adj.A"
    colnames(stmisc)[colnames(stmisc) == "PTS"] <- "Adj.PTS"
    colnames(stmisc)[colnames(stmisc) == "GC.1"] <- "Adj.GC"

    stats_nhl <- merge(stats_nhl, stmisc, by = c("Season",
      "Team"), all = TRUE)
    colnames(stats_nhl)[colnames(stats_nhl) == "Age.x"] <- "Age"
    colnames(stats_nhl)[colnames(stats_nhl) == "Lg.x"] <- "Lg"
    colnames(stats_nhl)[colnames(stats_nhl) == "GP.x"] <- "GP"
    colnames(stats_nhl)[colnames(stats_nhl) == "TOI.x"] <- "TOI"
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in%
        "")]
    if ("Age.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -Age.y)
    if ("Lg.y" %in% colnames(stats_nhl))
      stats_nhl <- subset(stats_nhl, select = -Lg.y)
  }

  playoffs_nhl <- data.frame()
  if ("stats_playoffs_nhl" %in% names(tables)) {
    playoffs_nhl <- tables$stats_playoffs_nhl
    playoffs_nhl$Playoffs = TRUE
    colnames(playoffs_nhl)[colnames(playoffs_nhl) == "Tm"] <- "Team"
    # Sometimes there's '' named columns. Messes up rbind.fill
    if ("" %in% colnames(playoffs_nhl))
      playoffs_nhl <- playoffs_nhl[, -which(names(playoffs_nhl) %in%
        "")]
  }

  stats_other <- data.frame()
  if ("stats_basic_other" %in% names(tables)) {
    stats_other <- tables$stats_basic_other
    stats_other$Playoffs = FALSE
    # Sometimes there's '' named columns. Messes up rbind.fill
    if ("" %in% colnames(stats_other))
      stats_other <- stats_other[, -which(names(stats_other) %in%
        "")]
    colnames(stats_other)[colnames(stats_other) == "Tm"] <- "Team"
  }

  playoffs_other <- data.frame()
  if ("stats_playoffs_other" %in% names(tables)) {
    playoffs_other <- tables$stats_playoffs_other
    playoffs_other$Playoffs = TRUE
    # Sometimes there's '' named columns. Messes up rbind.fill
    if ("" %in% colnames(playoffs_other))
      playoffs_other <- playoffs_other[, -which(names(playoffs_other) %in%
        c(""))]
    colnames(playoffs_other)[colnames(playoffs_other) ==
      "Tm"] <- "Team"
  }

  stats <- rbind.fill(stats_nhl, playoffs_nhl, stats_other,
    playoffs_other)
  colnames(stats)[colnames(stats) == "CF% rel"] <- "CF%rel"
  colnames(stats)[colnames(stats) == "FF% rel"] <- "FF%rel"
  colnames(stats)[colnames(stats) == "EV"] <- "EV.Goals"
  colnames(stats)[colnames(stats) == "PP"] <- "PP.Goals"
  colnames(stats)[colnames(stats) == "SH"] <- "SH.Goals"

  return(stats)
}

#' Get stats for a list of players
#' A function to get all player stats from a list of players (in the form provided by \code{\link{getPlayerList}})
#'
#' @param player_list A player list from \code{\link{getPlayerList}}
#' @param sleep Time to sleep between player scrapings
#'
#' @return a list of three data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}

getPlayerStats <- function(player_list, sleep = 30) {
  player_stats_tables <- data.frame()
  goalie_stats_tables <- data.frame()
  player_meta_tables <- data.frame()
  plist <- player_list[player_list$BlnNHL == TRUE, ]
  if (length(plist) == 0)
    return(NULL)
  pb <- txtProgressBar(min = 0, max = nrow(plist), initial = 0)
  for (player in c(1:nrow(plist))) {
    # prep HTML
    url <- paste0("http://www.hockey-reference.com", plist[player,
      "URL"])

    pname <- plist[player, "Name"]
    if (grepl("02.html", url, fixed = TRUE)) {
      pname <- paste(pname, "02")
    } else if (grepl("03.html", url, fixed = TRUE)) {
      pname <- paste(pname, "03")
    }

    scrape <- getPlayerInfo(url)
    # Add to record

    tables <- flattenTables(scrape[[1]])
    tables$Name <- pname

    if ("G" %in% scrape[[2]]["Position"]) {
      goalie_stats_tables <- rbind.fill(goalie_stats_tables,
        tables)
    } else {
      player_stats_tables <- rbind.fill(player_stats_tables,
        tables)
    }
    player_meta_tables <- rbind.fill(player_meta_tables,
      data.frame(Name = pname, Active = plist[player, "Active"],
        t(unlist(scrape[[2]]))))

    setTxtProgressBar(pb, player)
    Sys.sleep(sleep)
  }
  return(list(PlayerStats = player_stats_tables, GoalieStats = goalie_stats_tables,
    PlayerMeta = player_meta_tables))
}

#' Scrape players by Alphabet
#' A function to scrape and save player tables by last name, breaking up the scraping
#' into each chunk to prevent progress loss by scraping error (HTML error)
#'
#' @param player_list a player list of the type created by \code{\link{getPlayerNames}}
#' @param letters_to_scrape the letters of last names to scrape (default all letters)
#' @param letter_sleep The length of time to sleep between letters
#' @param combine Whether to combine all player data.frames (a-z) after downloading
#' @param directory Where to store data files
#' @param ... Additional params for getPlayerStats
#'
#' @return True, if successful
scrapeByAlphabet <- function(player_list, letters_to_scrape = letters,
  letter_sleep = 120, combine = TRUE, directory = "./_data/players/",
  ...) {
  for (letter in letters_to_scrape) {
    message(paste0("Getting Players with last name of ",
      toupper(letter), "."))
    ps <- getPlayerStats(player_list[startsWith(player_list$URL,
      paste0("/players/", letter)), ], ...)
    saveRDS(ps, paste0(directory, "players_", letter, ".RDS"))
    Sys.sleep(letter_sleep)
  }
  gc(verbose = FALSE)
  message("All player data downloaded")
  if (combine == TRUE) {
    return(combinePlayerDataFrames(directory))
  }
  return(TRUE)
}

#' Combine single letter player data frames
#' Easily combine player data.frames into one single RDS file.
#'
#' @param directory The directory where data files are stored. Default './_data/players/'
#'
#' @return TRUE, if successful
combinePlayerDataFrames <- function(directory = "./_data/players/") {
  message("Combining all player data to one object")
  ldf <- list()
  meta <- list()
  players <- list()
  goalies <- list()
  for (letter in letters) {
    tryCatch({
      ldf[[letter]] <- readRDS(paste0(directory, "players_",
        letter, ".RDS"))
      meta[[letter]] <- ldf[[letter]][[3]]
      goalies[[letter]] <- ldf[[letter]][[2]]
      players[[letter]] <- ldf[[letter]][[1]]
    }, error = function(e) message("Error reading file players_",
      letter, ".RDS: ", e, "Continuing..."))
  }
  all_players <- rbind.fill(players)
  all_goalies <- rbind.fill(goalies)
  all_meta <- rbind.fill(meta)
  all_df <- list(PlayerStats = all_players, GoalieStats = all_goalies,
    PlayerMeta = all_meta)
  saveRDS(all_df, paste0(directory, "allPlayers.RDS"))
  return(TRUE)
}

#' Clean Player Data
#' This function will process player data, returning clean data frames as a list
#'
#' @param player_data The player_data to clean up
#' @param drop_awards Whether to drop awards column.
#'
#' @return a list of three cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}

processPlayerData <- function(player_data, drop_awards = TRUE) {
  players <- player_data[[1]]
  goalies <- player_data[[2]]
  meta <- player_data[[3]]

  # Undo factors
  numeric_columns <- c("Age", "GP", "G", "A", "PTS", "+/-",
    "PIM", "EV.Goals", "PP.Goals", "SH.Goals", "GW", "EV.Assists",
    "PP.Assists", "SH.Assists", "S", "S%", "TOI", "GC", "Adj.G",
    "Adj.A", "Adj.PTS", "Adj.GC", "TSA", "OPS", "DPS", "PS",
    "FOW", "FOL", "FO%", "HIT", "BLK", "TK", "GV", "CF",
    "CA", "CF%", "CF%rel", "FF", "FA", "FF%", "FF%rel", "oiSH%",
    "oiSV%", "PDO", "oZS%", "dZS%", "GS", "W", "L", "T/O",
    "GA", "SA", "SV", "SV%", "GAA", "SO", "MIN", "QS", "QS%",
    "RBS", "GA%-", "GSAA", "GPS")
  pnames <- colnames(players)
  players <- data.frame(lapply(players, as.character), stringsAsFactors = FALSE)
  colnames(players) <- pnames

  gnames <- colnames(goalies)
  goalies <- data.frame(lapply(goalies, as.character), stringsAsFactors = FALSE)
  colnames(goalies) <- gnames

  mnames <- colnames(meta)
  meta <- data.frame(lapply(meta, as.character), stringsAsFactors = FALSE)
  colnames(meta) <- mnames

  players[, colnames(players) %in% numeric_columns] <- as.numeric(unlist(players[,
    colnames(players) %in% numeric_columns]))
  goalies[, colnames(goalies) %in% numeric_columns] <- as.numeric(unlist(goalies[,
    colnames(goalies) %in% numeric_columns]))

  # Fix Team vs. Tm
  if ("Tm" %in% colnames(players)) {
    players[is.na(players$Team), ]$Team <- players[is.na(players$Team),
      ]$Tm
    players <- subset(players, select = -Tm)
  }

  if ("Tm" %in% colnames(goalies)) {
    goalies[is.na(goalies$Team), ]$Team <- goalies[is.na(goalies$Team),
      ]$Tm
    goalies <- subset(goalies, select = -Tm)
  }

  # Remove double or more teams sums
  for (i in c(2:5)) {
    players <- subset(players, Team != paste0(i, " Teams"))
    goalies <- subset(goalies, Team != paste0(i, " Teams"))
  }

  # Average Time On Ice
  toi <- players$ATOI
  toi[toi == ""] <- "0:0"
  toi[is.na(toi)] <- "0:0"
  players$ATOI <- unlist(lapply(toi, function(x) as.numeric(unlist(strsplit(x,
    ":")))[1] + as.numeric(unlist(strsplit(x, ":"))[2])/60))
  toi <- goalies$ATOI
  toi[toi == ""] <- "0:0"
  toi[is.na(toi)] <- "0:0"
  goalies$ATOI <- unlist(lapply(toi, function(x) as.numeric(unlist(strsplit(x,
    ":")))[1] + as.numeric(unlist(strsplit(x, ":"))[2])/60))

  # Drop Awards
  if (drop_awards) {
    players <- subset(players, select = -Awards)
    goalies <- subset(goalies, select = -Awards)
  }

  # meta Cleanup
  meta[!is.na(meta$Birthdate), "Birthdate"] <- as.Date(meta[!is.na(meta$Birthdate),
    "Birthdate"])
  meta[!is.na(meta$Deathdate), "Deathdate"] <- as.Date(meta[!is.na(meta$Deathdate),
    "Deathdate"])

  imp <- meta$HeightImp
  imp[imp == ""] <- "0-0"
  imp[is.na(imp)] <- "0-0"
  meta$HeightImp <- unlist(lapply(imp, function(x) as.numeric(unlist(strsplit(x,
    "-")))[1] * 12 + as.numeric(unlist(strsplit(x, "-"))[2])))

  active <- meta$Active
  active[active == ""] <- "0-0"
  active[is.na(active)] <- "0-0"
  meta$ActiveStart <- unlist(lapply(active, function(x) as.numeric(unlist(strsplit(x,
    "-")))[1]))
  meta$ActiveEnd <- unlist(lapply(active, function(x) as.numeric(unlist(strsplit(x,
    "-")))[2]))
  meta <- subset(meta, select = -Active)

  mnumeric <- c("HeightImp", "WeightImp", "HeightMetric", "WeightMetric",
    "DraftRound", "DraftOverall", "DraftYear", "ReDraftRound",
    "ReDraftOverall", "ReDraftYear", "ActiveStart", "ActiveEnd")
  meta[, colnames(meta) %in% mnumeric] <- as.numeric(unlist(meta[,
    colnames(meta) %in% mnumeric]))

  # Order data.frame
  players <- players[with(players, order(Name, Age, Lg, Team,
    Playoffs)), ]
  goalies <- goalies[with(goalies, order(Name, Age, Lg, Team,
    Playoffs)), ]
  meta <- meta[with(meta, order(Name, Birthdate)), ]

  # Refactor Select Columns
  meta$Name <- factor(meta$Name)
  meta$Country <- gsub("&amp", "", meta$Country)
  meta$Country <- factor(meta$Country)
  meta$Province <- gsub("&amp", "", meta$Province)
  meta$Province <- factor(meta$Province)
  meta$State <- gsub("&amp", "", meta$State)
  meta$State <- factor(meta$State)
  meta$BirthPlace <- gsub("&amp", "", meta$BirthPlace)
  meta$BirthPlace <- gsub("&nbsp;", " ", meta$BirthPlace)
  meta$BirthPlace <- factor(meta$BirthPlace)
  meta$DraftTeam <- factor(meta$DraftTeam)
  meta$ReDraftTeam <- factor(meta$ReDraftTeam, levels = levels(meta$DraftTeam))

  players$Season <- factor(players$Season, ordered = TRUE)
  players$Team <- factor(players$Team)
  players$Lg <- factor(players$Lg)
  players$Name <- factor(players$Name, levels = levels(meta$Name))

  goalies$Season <- factor(goalies$Season, ordered = TRUE)
  goalies$Team <- factor(goalies$Team, levels = levels(players$Team))
  goalies$Lg <- factor(goalies$Lg, levels = levels(players$Lg))
  goalies$Name <- factor(goalies$Name, levels = levels(meta$Name))

  return(list(PlayerStats = players, GoalieStats = goalies,
    PlayerMeta = meta))
}

#' Get Current estimated rosters
#'
#' @param sleep The amount of time to sleep between requests
#'
#' @return a long data frame with three columns:
#' \item{Team}{The team}
#' \item{Player}{The name of the Forward, Defence or Goalie playing}
#' \item{updateDate}{The date the Team's page was updated}
getCurrentRosters<-function(sleep=30){
    teamUrlList<-data.frame(URL = c("http://www2.dailyfaceoff.com/teams/lines/13/anaheim-ducks/",
                              "http://www2.dailyfaceoff.com/teams/lines/35/arizona-coyotes",
                              "http://www2.dailyfaceoff.com/teams/lines/15/boston-bruins/",
                              "http://www2.dailyfaceoff.com/teams/lines/16/buffalo-sabres/",
                              "http://www2.dailyfaceoff.com/teams/lines/17/calgary-flames",
                              "http://www2.dailyfaceoff.com/teams/lines/18/carolina-hurricanes",
                              "http://www2.dailyfaceoff.com/teams/lines/19/chicago-blackhawks",
                              "http://www2.dailyfaceoff.com/teams/lines/20/colorado-avalanche",
                              "http://www2.dailyfaceoff.com/teams/lines/21/columbus-blue-jackets",
                              "http://www2.dailyfaceoff.com/teams/lines/22/dallas-stars",
                              "http://www2.dailyfaceoff.com/teams/lines/23/detroit-red-wings",
                              "http://www2.dailyfaceoff.com/teams/lines/24/edmonton-oilers",
                              "http://www2.dailyfaceoff.com/teams/lines/25/florida-panthers",
                              "http://www2.dailyfaceoff.com/teams/lines/26/los-angeles-kings",
                              "http://www2.dailyfaceoff.com/teams/lines/27/minnesota-wild",
                              "http://www2.dailyfaceoff.com/teams/lines/28/montreal-canadiens",
                              "http://www2.dailyfaceoff.com/teams/lines/29/nashville-predators",
                              "http://www2.dailyfaceoff.com/teams/lines/30/new-jersey-devils",
                              "http://www2.dailyfaceoff.com/teams/lines/31/new-york-islanders",
                              "http://www2.dailyfaceoff.com/teams/lines/32/new-york-rangers",
                              "http://www2.dailyfaceoff.com/teams/lines/33/ottawa-senators",
                              "http://www2.dailyfaceoff.com/teams/lines/34/philadelphia-flyers",
                              "http://www2.dailyfaceoff.com/teams/lines/36/pittsburgh-penguins",
                              "http://www2.dailyfaceoff.com/teams/lines/37/san-jose-sharks",
                              "http://www2.dailyfaceoff.com/teams/lines/38/st-louis-blues",
                              "http://www2.dailyfaceoff.com/teams/lines/39/tampa-bay-lightning",
                              "http://www2.dailyfaceoff.com/teams/lines/40/toronto-maple-leafs",
                              "http://www2.dailyfaceoff.com/teams/lines/41/vancouver-canucks",
                              "http://www2.dailyfaceoff.com/teams/lines/42/washington-capitals",
                              "http://www2.dailyfaceoff.com/teams/lines/14/winnipeg-jets"),
                      Team = c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins",
                               "Buffalo Sabres", "Calgary Flames", "Carolina Hurricanes",
                               "Chicago Blackhawks", "Colorado Avalanche", "Columbus Blue Jackets",
                               "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers", "Florida Panthers",
                               "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens",
                               "Nashville Predators", "New Jersey Devils", "New York Islanders",
                               "New York Rangers", "Ottawa Senators", "Philadelphia Flyers",
                               "Pittsburgh Penguins", "San Jose Sharks", "St. Louis Blues",
                               "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks",
                               "Washington Capitals", "Winnipeg Jets"),
                      stringsAsFactors = FALSE)
    rosters<-data.frame('Team'=character(), 'Players'=character(), 'updateDate'=character())

    pb <- txtProgressBar(min = 0, max = nrow(teamUrlList), initial = 0)

    for(i in 1:nrow(teamUrlList)){
        htmlpage<-getURLInternal(teamUrlList[i,1])
        tabs<-readHTMLTable(htmlpage, header=FALSE)
        pattern<-"Last update: ([A-Za-z0-9\\., ]+)<\\/div>"
        dt<-as.character(as.Date(gsub('\\.', '', str_match(htmlpage, pattern)[1,2]), format='%b%t%e,%t%Y'))
        r<-data.frame(Team=rep(teamUrlList[i,2]), Players=c(levels(unlist(tabs$forwards)), levels(unlist(tabs$defense)), levels(unlist(tabs$goalie_list))), updateDate=rep(dt), stringsAsFactors = FALSE)
        rosters<-rbind(rosters, r)
        setTxtProgressBar(pb, i)
        Sys.sleep(sleep)
    }

    rosters$Team <- factor(rosters$Team)
    rosters$updateDate <- as.Date(rosters$updateDate)
    return(rosters)
}
