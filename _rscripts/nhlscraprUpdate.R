### Patch for full.game.database and for player.summary
### for A.C. Thomas's nhlscrapr package version 1.8 ( act@acthomas.ca )
### by Jack Davis 2016-04-18 ( jackd@sfu.ca )

###Further patches by Philip Bulsink ( bulsinkp@gmail.com )
require(nhlscrapr)
### Now works for extra seasons
full.game.database = function (extra.seasons = 0) 
{
    game.roster <- NULL
    seasons <- c("20022003", "20032004", "20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012", "20122013", "20132014", "20142015", "20152016")
    if (extra.seasons > 0) 
	{
        seasons <- c(seasons, paste(2015 + 1:extra.seasons, 2016 + 1:extra.seasons, sep = ""))  ## Patched: Extra seasons now start at 20162017 - PB
	}
	
	### Patched: bad.game.list now include many NULLs for additional seasons.
	### 		Reason: R doesn't allow adding NULL elements to a list after the line has been instantiated. - JD
    games <- c(rep(1230, 9), 720, 1230, 1230, 1230, rep(1230, extra.seasons))
    bad.game.list <- list(c(1:127, 134, 135, 582, 598, 872), 
        c(10, 251, 453, 456, 482, 802, 1205), c(18, 140, 127, 
            234, 298, 458, 974), c(1024), c(1178), c(259, 409, 
            1077), c(81, 827, 836, 857, 863, 874, 885), c(124, 
            429), c(259), c(), c(), c(), c(), c(), c(), c(), c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c(),
			c(), c(), c(), c(), c(), c(), c(), c(), c(),c(), c())
			
			
	bad.game.list = bad.game.list[1:length(seasons)] ### Patched: Removes extraneous elements rather than adding on NULLs - JD
	
    playoff.series <- c("11", "12", "13", "14", "15", "16", "17", 
        "18", "21", "22", "23", "24", "31", "32", "41")
    gnum <- paste0("0", c(t(outer(playoff.series, 1:7, paste0))))
    for (ss in 1:length(seasons)) 
	{
        gn1 <- as.character(1:games[ss])
        while (any(nchar(gn1) < 4)) gn1[nchar(gn1) < 4] <- paste0("0", 
            gn1[nchar(gn1) < 4])
        df1 <- data.frame(season = seasons[ss], session = c(rep("Regular", 
            games[ss]), rep("Playoffs", length(gnum))), gamenumber = c(gn1, 
            gnum), gcode = "", status = 1, valid = c(!(1:games[ss] %in% 
            bad.game.list[[ss]]), rep(TRUE, length(gnum))), awayteam = "", 
            hometeam = "", awayscore = "", homescore = "", date = "", 
            game.start = "", game.end = "", periods = 0, stringsAsFactors = FALSE)
        game.roster <- rbind(game.roster, df1)
    }
	
	
    game.roster[, 1] <- as.character(game.roster[, 1])
    game.roster[, 2] <- as.character(game.roster[, 2])
    game.roster$gcode <- paste0(2 + 1 * (game.roster$session == 
        "Playoffs"), game.roster$gamenumber)
    game.roster$status[!game.roster$valid] <- 0
    game.roster <- game.roster[, colnames(game.roster) != "valid"]
    playoff.series.lengths <- c(
        4, 4, 6, 7, 7, 7, 6, 4,  5, 7, 7, 5,  6, 6,  6, #"20022003"
        6, 5, 5, 6, 7, 6, 4, 7,  7, 6, 5, 6,  7, 7,  4, #"20032004"
        7, 5, 7, 5, 7, 6, 5, 7,  5, 4, 6, 6,  6, 7,  6, #"20052006"
        7, 5, 6, 4, 6, 6, 5, 7,  5, 5, 5, 6,  4, 7,  5, #"20062007"
        7, 5, 6, 4, 5, 6, 5, 7,  5, 6, 5, 6,  5, 5,  6, #"20072008"
        5, 7, 4, 7, 5, 6, 7, 6,  6, 5, 5, 4,  6, 5,  6, #"20082009"
        6, 4, 7, 7, 6, 6, 6, 7,  7, 7, 5, 6,  5, 4,  6, #"20092010"
        5, 7, 7, 7, 7, 6, 4, 6,  4, 4, 6, 7,  7, 5,  7, #"20102011"
        7, 7, 7, 6, 5, 5, 6, 5,  7, 5, 4, 5,  6, 5,  6, #"20112012"
        6, 5, 7, 7, 5, 7, 4, 6,  5, 5, 7, 7,  4, 5,  6, #"20122013"
        5, 4, 6, 7, 7, 6, 6, 7,  7, 7, 6, 7,  6, 7,  5, #"20132014"
        6, 7, 5, 7, 6, 6, 4, 6,  6, 7, 4, 5,  7, 7,  6, #"20142015" - PB
        6, 5, 6, 5, 6, 7, 7, 5,  5, 6, 7, 7,  7, 6,  6, #"20152016" - PB
        rep(7, 15 * (extra.seasons)))
    sequence.seven <- function(nn) c(rep(1, nn), rep(0, 7 - nn))
    playoff.status <- c(sapply(playoff.series.lengths, sequence.seven))
    game.roster$status[game.roster$session == "Playoffs"] <- playoff.status
    bad.playoff <- matrix(c("20032004", "30134", "20052006", 
        "30233"), nrow = 2)
    for (kk in 1:dim(bad.playoff)[2]) game.roster$status[game.roster$season == 
        bad.playoff[1, kk] & game.roster$gcode == bad.playoff[2, 
        kk]] <- 0
    gamecols <- match(paste0("20142015", as.character(nhlscrapr::date201415$gcode)), 
        paste0(game.roster$season, game.roster$gcode))
    game.roster$awayteam[gamecols] <- as.character(nhlscrapr::date201415$awayteam)
    game.roster$hometeam[gamecols] <- as.character(nhlscrapr::date201415$hometeam)
    unplayed <- which(game.roster$game.start[gamecols] == "")
    game.roster$game.start[gamecols[unplayed]] <- paste(as.character(nhlscrapr::date201415$StartET[unplayed]), 
        "ET")
    game.roster$date[gamecols[unplayed]] <- as.character(nhlscrapr::date201415$GameDate[unplayed])
    return(game.roster)
}



### Fixed some indexing crashes, and some crashes for players with 0 events
 player.summary = function (grand.data, roster.unique) 
{
    events <- c("PENL", "SHOT", "GOAL", "MISS", "BLOCK")
    columns <- 3 + c(5:16, 18:20, 28:29)
    involved.players <- NULL
    for (cc in columns) involved.players <- unique(c(involved.players, 
        grand.data[, cc]))
    involved.players <- sort(involved.players)
    output <- array(0, c(length(involved.players), length(events), 
        5))
    for (ee in events) {
        message(paste("Matching", ee))
        little.data <- grand.data[grand.data$etype == ee, ]
        if (dim(little.data)[1] > 0) {
            for (cc in which(names(grand.data) %in% c("ev.player.1", "ev.player.2", "ev.player.3")))  ## patched this line, cc is an index now - JD
			{
                evs <- table(little.data[, cc])
                rws <- match(as.numeric(names(evs)), involved.players)
                output[rws, which(ee == events), cc - 20] <- output[rws, which(ee == events), cc - 20] + evs
            }
            for (cc in which(names(grand.data) %in% c("a1", "a2", "a3", "a4", "a5", "a6", "away.G"))) ## patched this line, cc is an index now - JD
			{
                evs <- table(little.data[little.data$ev.team ==  little.data$hometeam, cc])
                rws <- match(as.numeric(names(evs)), involved.players)
				evs <- evs[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
				rws <- rws[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
                output[rws, which(ee == events), 5] <- output[rws, which(ee == events), 5] + evs
				
				
                evs <- table(little.data[little.data$ev.team == little.data$awayteam, cc])
                rws <- match(as.numeric(names(evs)), involved.players)
				evs <- evs[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
				rws <- rws[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
			
                output[rws, which(ee == events), 4] <- output[rws, which(ee == events), 4] + evs
            }
            for (cc in which(names(grand.data) %in%  c("h1", "h2", "h3", "h4", "h5", "h6",  "home.G")))  ## patched this line, cc is an index now - JD
			{
                evs <- table(little.data[little.data$ev.team == little.data$hometeam, cc])
                rws <- match(as.numeric(names(evs)), involved.players)
                evs <- evs[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
				rws <- rws[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
                output[rws, which(ee == events), 4] <- output[rws,  which(ee == events), 4] + evs
                
				evs <- table(little.data[little.data$ev.team == little.data$awayteam, cc])
                rws <- match(as.numeric(names(evs)), involved.players)
                evs <- evs[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
				rws <- rws[!is.na(rws)] ### added this line, this removes player that were never involved in that situation (NAs in match() )
                output[rws, which(ee == events), 5] <- output[rws, which(ee == events), 5] + evs
            }
        }
    }
    output <- output[involved.players > 0, , ]
    involved.players <- involved.players[involved.players > 0]
    rownames(output) <- roster.unique$firstlast[involved.players]
    colnames(output) <- events
    return(output)
}



require(plyr)

aggregate_roster_by_name = function(roster)
{

	roster_name = ddply(roster, .(firstlast), summarize,
				pos = pos[1],
				last = last[1],
				first = first[1],
				numfirstlast = numfirstlast[1],
				firstlast = firstlast[1],
				index = index[1],
				player.id = player.id[1],
				pC = sum(pC),
				pL = sum(pL),
				pR = sum(pR),
				pD = sum(pD),
				pG = sum(pG)	
				)
	
	roster_name = roster_name[order(roster_name$player.id),]
	return(roster_name)
}

compile.all.games<-function (rdata.folder = "nhlr-data", output.folder = "source-data", 
                             new.game.table = NULL, seasons = NULL, verbose = FALSE, override.days.back = NULL, 
                             date.check = FALSE, extra.seasons = 0, ...) 
{
  
  if (extra.seasons > 0) 
  {
    seasons <- c(seasons, paste(2015 + 1:extra.seasons, 2016 + 1:extra.seasons, sep = ""))  ## Patched: Extra seasons now start at 20162017 - PB
  }
  
  suppressWarnings(dir.create(output.folder))
  if (file.exists(paste0(output.folder, "/nhlscrapr-core.RData"))) {
    message("Loading game and player data.")
    load(paste0(output.folder, "/nhlscrapr-core.RData"))
  }
  else {
    message("Creating game table and player data.")
    games <- full.game.database()
    grand.data <- NULL
    roster.master <- NULL
    distance.adjust <- scoring.models <- shot.tables <- NULL
    blanks <- which(is.na(games$date))
    for (kk in blanks[blanks > 1 & blanks < nrow(games)]) if (!is.na(games$date[kk - 
                                                                                1]) && !is.na(games$date[kk + 1]) && games$date[kk - 
                                                                                                                                1] == games$date[kk + 1]) 
      games$date[kk] <- games$date[kk - 1]
  }
  repl <- grep("^[0-9]+$", games$date)
  games$date[repl] <- as.character(as.Date("1970-01-01") + 
                                     as.numeric(games$date[repl]))
  if (!is.null(new.game.table)) {
    games <- new.game.table
  }
  if (!is.null(seasons)) {
    message("Overriding existing game table to create one with specified seasons.")
    eligible.seasons <- c("20022003", "20032004", "20052006", 
                          "20062007", "20072008", "20082009", "20092010", "20102011", 
                          "20112012", "20122013", "20132014", "20142015", "20152016")
    if (extra.seasons > 0) 
    {
      eligible.seasons <- c(seasons, paste(2015 + 1:extra.seasons, 2016 + 1:extra.seasons, sep = ""))  ## Patched: Extra seasons now start at 20162017 - PB
    }
    if (!all(seasons %in% eligible.seasons)) 
      stop("Specified seasons must be within ", paste(eligible.seasons, 
                                                      collapse = ", "))
    games <- full.game.database()
    games <- games[games$season %in% seasons, ]
  }
  today.now <- format(as.POSIXct(Sys.time(), tz = "America/Los_Angeles"), 
                      tz = "America/Los_Angeles", usetz = TRUE)
  today <- as.Date(today.now)
  override.dates <- as.character(today - override.days.back)
  override.rows <- which(games$date %in% override.dates)
  games$status[which(games$date %in% override.dates)] <- 2
  for (this.season in unique(games$season)) {
    new.pbp <- new.roster <- list()
    cons.failures <- 0
    replace.rows <- which(games$season == this.season & games$status %in% 
                            c(1, 2))
    sub.games <- games[replace.rows, ]
    if (length(replace.rows) == 0) {
      message(this.season, ": no games need updating.")
      next
    }
    for (kk in 1:nrow(sub.games)[1]) {
      if (kk%%500 == 0) 
        message(paste0("Event assembly: ", this.season, 
                       " game", kk))
      if (verbose) 
        message("Trying game ", sub.games$season[kk], 
                sub.games$gcode[kk], " ", sub.games$date[kk], 
                " because it's ", today.now)
      if (grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}", sub.games$date[kk]) | 
          !date.check) {
        if (sub.games$date[kk] > today.now) {
          if (verbose) 
            message("Skipping Game ", sub.games$season[kk], 
                    sub.games$gcode[kk], " ", sub.games$date[kk], 
                    " because it's ", today.now)
          next
        }
      }
      else next
      tryme <- try({
        game.info <- retrieve.game(sub.games$season[kk], 
                                   sub.games$gcode[kk], rdata.folder, force = FALSE)
        doit <- FALSE
        if (is.null(game.info)) 
          doit <- TRUE
        else if (game.info$status %in% 1:2) 
          doit <- TRUE
        if (doit) 
          game.info <- process.single.game(sub.games$season[kk], 
                                           sub.games$gcode[kk], rdata.folder = rdata.folder, 
                                           override.download = TRUE, ...)
        sub.games$status[kk] <- game.info$status
        sub.games$awayteam[kk] <- game.info$teams[1]
        sub.games$hometeam[kk] <- game.info$teams[2]
        sub.games$awayscore[kk] <- game.info$score[2]
        sub.games$homescore[kk] <- game.info$score[1]
        sub.games$date[kk] <- as.character(as.Date(paste(game.info$date, 
                                                         collapse = " "), format = "%A %B %d %Y"))
        sub.games$game.start[kk] <- game.info$game.times[1]
        sub.games$game.end[kk] <- game.info$game.times[2]
        sub.games$periods[kk] <- max(game.info$playbyplay$period)
        new.pbp[[kk]] <- game.info
        new.roster[[kk]] <- game.info$players
      }, TRUE)
      if (class(tryme) == "try-error") 
        cons.failures <- cons.failures + 1
      else cons.failures <- 0
      if (cons.failures >= 20) {
        message("20 consecutive failed attempts; stopping file retrieval.")
        break
      }
    }
    games[replace.rows, ] <- sub.games
    if (length(new.roster) > 0) {
      message(this.season, " -- updating rosters on each game file.")
      roster.master <- construct.rosters.from.list(new.roster, 
                                                   roster.master)
      new.pbp.2 <- lapply(new.pbp, function(game.info) {
        out <- try(augment.game(game.info, roster.master), 
                   TRUE)
        if (class(out) == "try-error") 
          out <- NULL
        return(out)
      })
      saveRDS(new.pbp.2, file = "new.pbp.2.RDS")
      saveRDS(new.pbp, file="new.pbp.RDS")
      secondary.data <- fold.frames(new.pbp.2)
      secondary.data$adjusted.distance <- NA
      secondary.data$shot.prob.distance <- NA
      secondary.data$prob.goal.if.ongoal <- NA
      message("Adding event location sections.")
      coords <- secondary.data[, c("xcoord", "ycoord")]
      flip <- which(coords[, 1] < 0)
      coords[flip, 1] <- -coords[flip, 1]
      coords[flip, 2] <- -coords[flip, 2]
      secondary.data$loc.section <- pick.section(coords)
      secondary.data$new.loc.section <- secondary.data$loc.section
      secondary.data$newxc <- secondary.data$xcoord
      secondary.data$newyc <- secondary.data$ycoord
    }
    else secondary.data <- NULL
    if (!is.null(secondary.data)) {
      if (file.exists(paste0(output.folder, "/nhlscrapr-", 
                             this.season, ".RData"))) {
        load(paste0(output.folder, "/nhlscrapr-", this.season, 
                    ".RData"))
      }
      else grand.data <- secondary.data[secondary.data$season == 
                                          "0000", ]
      grand.data <- rbind(grand.data[!(grand.data$gcode %in% 
                                         unique(secondary.data$gcode)), ], secondary.data)
      grand.data$gcode <- as.character(grand.data$gcode)
      save(grand.data, file = paste0(output.folder, "/nhlscrapr-", 
                                     this.season, ".RData"))
    }
  }
  message("Saving to output file")
  roster.unique <- manual.patches(roster.master[match(1:max(roster.master$player.id), 
                                                      roster.master$player.id), ])
  save(roster.master, roster.unique, games, file = paste0(output.folder, 
                                                          "/nhlscrapr-core.RData"))
  return(TRUE)
}

construct.rosters.from.list <- function (roster.collection,  #raw list
                                         roster.master=NULL) {
  #roster.collection=new.roster;  roster.master=NULL
  
  blanky <- data.frame (pos="", last="", first="", numfirstlast="", firstlast="",
                        index=1, player.id=1,
                        pC=0, pL=0, pR=0, pD=0, pG=0,
                        stringsAsFactors=FALSE)
  if (is.null(roster.master)) roster.master <- blanky
  
  for (kk in 1:length(roster.collection)) if (!is.null(roster.collection[[kk]])) if (nrow(roster.collection[[kk]])>0) { # ) {    #
    
    if (kk %% 500 == 0) message(paste("Roster merger: game",kk,"of",length(roster.collection)))
    
    this.roster <- fix.names.manually(roster.collection[[kk]][,c("number","pos","last","first","numfirstlast")])
    
    match1 <- match(this.roster$numfirstlast,
                    roster.master$numfirstlast)
    if (any(is.na(match1))) {
      rows <- which(is.na(match1))
      newrecs <- data.frame (pos=this.roster$pos[rows],
                             last=this.roster$last[rows],
                             first=this.roster$first[rows],
                             numfirstlast=this.roster$numfirstlast[rows],
                             firstlast="",  index=nrow(roster.master) + 1:length(rows),
                             player.id=NA,
                             pC=0, pL=0, pR=0, pD=0, pG=0,
                             stringsAsFactors=FALSE)
      
      newrecs$firstlast <- paste(newrecs$first, newrecs$last)
      
      m2 <- match(newrecs$firstlast, roster.master$firstlast)
      if (any(!is.na(m2))) newrecs$player.id[!is.na(m2)] <- roster.master$player.id[m2[!is.na(m2)]]
      if (any(is.na(m2))) newrecs$player.id[is.na(m2)] <- max(roster.master$player.id) + 1:sum(is.na(m2))
      
      roster.master <- rbind(roster.master, newrecs)
      
      #zeroes <- rep(0, length(rows))
      #positions <- rbind(positions, data.frame(pC=zeroes, pL=zeroes, pR=zeroes, pD=zeroes, pG=zeroes))
    }
    
    r1.match <- match(this.roster$numfirstlast,
                      roster.master$numfirstlast)
    roster.master$pC[r1.match[this.roster$pos=="C"]] <-
      roster.master$pC[r1.match[this.roster$pos=="C"]] + 1
    roster.master$pL[r1.match[this.roster$pos=="L"]] <-
      roster.master$pL[r1.match[this.roster$pos=="L"]] + 1
    roster.master$pR[r1.match[this.roster$pos=="R"]] <-
      roster.master$pR[r1.match[this.roster$pos=="R"]] + 1
    roster.master$pD[r1.match[this.roster$pos=="D"]] <-
      roster.master$pD[r1.match[this.roster$pos=="D"]] + 1
    roster.master$pG[r1.match[this.roster$pos=="G"]] <-
      roster.master$pG[r1.match[this.roster$pos=="G"]] + 1
    
  }
  
  return(roster.master)
  
}

fix.names.manually <- function (master.list) {
  #  master.list=roster.collection[[kk]][,c("number","pos","last","first","numfirstlast")]
  
  #one name, two players.
  master.list$first[which(master.list$last=="PICARD" & master.list$first=="ALEXANDRE" & master.list$pos == "D")] <- "ALEXANDRE R."
  master.list$first[which(master.list$last=="GREEN" & master.list$first=="MIKE" & master.list$pos == "C")] <- "MICHAEL G."
  
  #manual fixes.
  master.list$last[which(master.list$last=="ANDERSSON" & master.list$first=="CRAIG")] <- "ANDERSON"
  master.list$first[which(master.list$last=="ANTROPOV" & master.list$first=="NIKOLAI")] <- "NIK"
  master.list$first[which(master.list$last=="AULD" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$last=="AXELSSON" & master.list$first=="PER JOHAN")] <- "P.J."
  master.list$first[grep("P\\. *J\\. *",master.list$first)] <- "P.J."
  master.list$first[which(master.list$last=="BAILEY" & master.list$first=="JOSHUA")] <- "JOSH"
  master.list$first[which(master.list$last=="BARCH" & master.list$first=="KRYSTOFER")] <- "KRYS"
  master.list$first[which(master.list$last=="BARKER" & master.list$first=="CAMERON")] <- "CAM"
  master.list$first[which(master.list$last=="BERGFORS" & master.list$first=="NICKLAS")] <- "NICLAS"
  master.list$first[which(master.list$last=="BLACKBURN" & master.list$first=="DANIEL")] <- "DAN"
  master.list$first[which(master.list$last=="BLAKE" & master.list$first=="ROBERT")] <- "ROB"
  master.list$first[which(master.list$last=="BLUNDEN" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="BOURQUE" & master.list$first=="CHRISTOPHER")] <- "CHRIS"
  master.list$first[which(master.list$last=="BOYNTON" & master.list$first=="NICHOLAS")] <- "NICK"
  master.list$first[which(master.list$last=="BRIERE" & master.list$first=="DANNY")] <- "DANIEL"
  master.list$first[which(master.list$last=="BRYZGALOV" & master.list$first=="ILJA")] <- "ILYA"
  master.list$first[which(master.list$last=="BURROWS" & master.list$first=="ALEXANDRE")] <- "ALEX"
  master.list$first[which(master.list$last=="CAMMALLERI" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="CARCILLO" & master.list$first=="DANIEL")] <- "DAN"
  master.list$first[which(master.list$last=="CARLE" & master.list$first=="MATTHEW")] <- "MATT"
  master.list$first[which(master.list$last=="CLEARY" & master.list$first=="DAN")] <- "DANIEL"
  master.list$first[which(master.list$last=="CLEARY" & master.list$first=="DANNY")] <- "DANIEL"
  master.list$first[which(master.list$last=="CORVO" & master.list$first=="JOSEPH")] <- "JOE"
  master.list$first[which(master.list$last=="CRABB" & master.list$first=="JOSEPH")] <- "JOEY"
  master.list$first[which(master.list$last=="CROMBEEN" & master.list$first=="BJ")] <- "B.J."
  master.list$first[which(master.list$last=="CROMBEEN" & master.list$first=="BRANDON")] <- "B.J."
  master.list$first[which(master.list$last=="DADONOV" & master.list$first=="EVGENII")] <- "EVGENY"
  
  master.list$first[which(master.list$last=="DOWD" & master.list$first=="JAMES")] <- "JIM"
  master.list$first[which(master.list$last=="DOWELL" & master.list$first=="JACOB")] <- "JAKE"
  master.list$first[which(master.list$last=="DRAZENOVIC" & master.list$first=="NICHOLAS")] <- "NICK"
  
  master.list$first[which(master.list$last=="DUMBA" & master.list$first=="MATHEW")] <- "MATT"
  
  master.list$first[which(master.list$last=="DUMONT" & master.list$first=="J P")] <- "JEAN-PIERRE"
  master.list$first[which(master.list$last=="DUMONT" & master.list$first=="J-P")] <- "JEAN-PIERRE"
  master.list$first[which(master.list$last=="EARL" & master.list$first=="ROBBIE")] <- "ROBERT"
  
  master.list$first[which(master.list$last=="ENSTROM" & master.list$first=="TOBY")] <- "TOBIAS"  ## 2014-10-19
  
  master.list$first[which(master.list$last=="FERNANDEZ" & master.list$first=="EMMANUEL")] <- "MANNY"
  master.list$first[which(master.list$last=="FROLOV" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$first=="TJ")] <- "T.J."
  master.list$first[which(master.list$last=="GAUTHIER" & master.list$first=="DENIS JR.")] <- "DENIS"
  master.list$first[which(master.list$last=="GIGUERE" & master.list$first=="J")] <- "JEAN-SEBASTIEN"
  master.list$first[which(master.list$last=="GIRARDI" & master.list$first=="DAN")] <- "DANIEL"
  master.list$first[which(master.list$last=="GREENE" & master.list$first=="ANDY")] <- "ANDREW"
  master.list$first[which(master.list$last=="GREER" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="GROSSMAN" & master.list$first=="NIKLAS")] <- "NICKLAS"
  master.list$last[which(master.list$last=="GROSSMAN" & master.list$first=="NICKLAS")] <- "GROSSMANN"
  master.list$first[which(master.list$last=="GUENIN" & master.list$first=="NATE")] <- "NATHAN"
  master.list$first[which(master.list$last=="HALKO" & master.list$first=="STEVE")] <- "STEVEN"
  master.list$first[which(master.list$last=="HIGGINS" & master.list$first=="CHRISTOPHER")] <- "CHRIS"
  
  master.list$first[which(master.list$last=="HAVLAT" & master.list$first=="MARTY")] <- "MARTIN"
  
  master.list$first[which(master.list$last=="HERR" & master.list$first=="MATTHEW")] <- "MATT"
  
  
  master.list$last[which(master.list$last=="HILLEN III" & master.list$first=="JOHN")] <- "HILLEN"
  master.list$first[which(master.list$last=="HILLEN" & master.list$first=="JOHN")] <- "JACK"
  master.list$first[which(master.list$last=="HOLIK" & master.list$first=="ROBERT")] <- "BOBBY"
  master.list$first[which(master.list$last=="HOWARD" & master.list$first=="JAMES")] <- "JIMMY"
  
  master.list$first[which(master.list$last=="IRWIN" & master.list$first=="MATTHEW")] <- "MATT"
  master.list$first[which(master.list$last=="JACKMAN" & master.list$first=="RICHARD")] <- "RIC"
  master.list$first[which(master.list$last=="JACQUES" & master.list$first=="J-F")] <- "JEAN-FRANCOIS"
  master.list$first[which(master.list$last=="JOHANSSON" & master.list$first=="MATTIAS")] <- "MATHIAS"
  master.list$first[which(master.list$last=="KALINSKI" & master.list$first=="JONATHON")] <- "JON"
  
  master.list$last[which(master.list$last=="KASTSITSYN")] <- "KOSTITSYN"
  master.list$first[which(master.list$last=="KOSTITSYN" & master.list$first=="SIARHEI")] <- "SERGEI"
  
  master.list$first[which(master.list$last=="KILLORN" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$first[which(master.list$last=="KING" & master.list$first=="DWAYNE")] <- "D.J."
  
  master.list$first[which(master.list$first=="DJ")] <- "D.J."
  master.list$first[which(master.list$last=="KNUBLE" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="KOLANOS" & master.list$first=="KRYSTOFER")] <- "KRYS"
  master.list$first[which(master.list$last=="KOMISAREK" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="KONDRATIEV" & master.list$first=="MAX")] <- "MAXIM"
  master.list$first[which(master.list$last=="KOVALEV" & master.list$first=="ALEXEI")] <- "ALEX"
  master.list$last[which(master.list$last=="KRONVALL")] <- "KRONWALL"
  master.list$first[which(master.list$last=="LEGACE" & master.list$first=="EMMANUEL")] <- "MANNY"
  master.list$first[which(master.list$last=="LETANG" & master.list$first=="KRISTOPHER")] <- "KRIS"
  
  master.list$first[which(master.list$last=="MACIAS" & master.list$first=="RAYMOND")] <- "RAY"
  
  master.list$first[which(master.list$last=="MACLEAN" & master.list$first=="DONALD")] <- "DON"
  master.list$last[which(master.list$last=="MAGNAN-GRENIER")] <- "MAGNAN"
  master.list$first[which(master.list$last=="MAYOROV" & master.list$first=="MAXIM")] <- "MAKSIM"
  master.list$first[which(master.list$last=="MCCOLLUM" & master.list$first=="TOM")] <- "THOMAS"
  master.list$first[which(master.list$last=="MCGILLIS" & master.list$first=="DAN")] <- "DANIEL"
  master.list$last[which(master.list$last=="MEYER IV")] <- "MEYER"
  master.list$first[which(master.list$last=="MEYER" & master.list$first=="FREDDY")] <- "FREDERICK"
  master.list$first[which(master.list$last=="MILLER" & master.list$first=="ANDREW")] <- "DREW"
  
  master.list$first[which(master.list$last=="MILLS" & master.list$first=="BRADLEY")] <- "BRAD"
  
  master.list$first[which(master.list$last=="MODANO" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="MODIN" & master.list$first=="FREDDY")] <- "FREDRIK"
  master.list$first[which(master.list$last=="NEIL" & master.list$first=="CHRISTOPHER")] <- "CHRIS"
  
  master.list$first[which(master.list$last=="NIETO" & master.list$first=="MATTHEW")] <- "MATT"
  
  master.list$first[which(master.list$last=="ODUYA" & master.list$first=="DAVID JOHNNY")] <- "JOHNNY"
  master.list$first[which(master.list$last=="ODUYA" & master.list$first=="JOHN")] <- "JOHNNY"
  master.list$last[which(master.list$last=="ORTMYER" & master.list$first=="JED")] <- "ORTMEYER"
  master.list$first[which(master.list$last=="OVECHKIN" & master.list$first=="ALEXANDER")] <- "ALEX"
  
  master.list$first[which(master.list$last=="PARENTEAU" & master.list$first=="PIERRE")] <- "P.A."
  master.list$first[which(master.list$last=="PARENTEAU" & master.list$first=="PA")] <- "P.A."
  master.list$first[which(master.list$last=="PELLEY" & master.list$first=="RODNEY")] <- "ROD"
  master.list$first[which(master.list$last=="PEVERLEY" & master.list$first=="JOHN")] <- "RICH"
  
  master.list$first[which(master.list$last=="POULIOT" & master.list$first=="MARC")] <- "MARC-ANTOINE"
  
  master.list$first[which(master.list$last=="PROSPAL" & master.list$first=="VINNY")] <- "VACLAV"
  master.list$first[which(master.list$last=="PURCELL" & master.list$first=="EDWARD")] <- "TEDDY"
  
  master.list$last[which(master.list$last=="PUSHKAREV" & master.list$first=="KONSTANTIN")] <- "PUSHKARYOV"
  
  master.list$first[which(master.list$last=="REINHART" & master.list$first=="MAXWELL")] <- "MAX"
  
  
  master.list$first[which(master.list$last=="REINPRECHT" & master.list$first=="STEVE")] <- "STEVEN"
  master.list$first[which(master.list$last=="RISSMILLER" & master.list$first=="PAT")] <- "PATRICK"
  master.list$first[which(master.list$last=="RUPP" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="SANTORELLI" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="SCUDERI" & master.list$first=="ROBERT")] <- "ROB"
  
  master.list$first[which(master.list$last=="SESTITO" & master.list$first=="TOMMY")] <- "TOM"
  master.list$last[which(master.list$last=="SHISKANOV" & master.list$first=="TIMOFEI")] <- "SHISHKANOV"
  master.list$first[which(master.list$last=="SILLINGER" & master.list$first=="MICHAEL")] <- "MIKE"
  
  master.list$first[which(master.list$last=="SIM" & master.list$first=="JON")] <- "JONATHAN"
  master.list$first[which(master.list$last=="SIMON" & master.list$first=="BEN")] <- "BENJAMIN"
  master.list$first[which(master.list$last=="STAJAN" & master.list$first=="MATTHEW")] <- "MATT"
  
  master.list$first[which(master.list$last=="STEEN" & master.list$first=="ALEXANDER")] <- "ALEX"
  master.list$last[which(master.list$last=="ST LOUIS" & master.list$first=="MARTIN")] <- "ST. LOUIS"
  master.list$first[which(master.list$last=="STORTINI" & master.list$first=="ZACHERY")] <- "ZACK"
  master.list$last[which(master.list$last=="ST PIERRE" & master.list$first=="MARTIN")] <- "ST. PIERRE"
  master.list$last[which(master.list$last=="STREBAK" & master.list$first=="MARTIN")] <- "STRBAK"
  master.list$first[which(master.list$first=="PK")] <- "P.K."
  
  master.list$first[which(master.list$last=="TAYLOR" & master.list$first=="TIMOTHY")] <- "TIM"
  master.list$first[which(master.list$last=="THOMAS" & master.list$first=="TIMOTHY JR.")] <- "TIM"
  master.list$first[which(master.list$last=="THOMAS" & master.list$first=="WILLIAM")] <- "BILL"
  
  
  
  master.list$first[which(master.list$first=="RJ")] <- "R.J."
  master.list$first[which(master.list$last=="VALICEVIC" & master.list$first=="ROBERT")] <- "ROB"
  master.list$first[which(master.list$last=="VALIQUETTE" & master.list$first=="STEVE")] <- "STEPHEN"
  master.list$first[which(master.list$last=="VANDERMEER" & master.list$first=="JAMES")] <- "JIM"
  master.list$first[which(master.list$last=="VARLAMOV" & master.list$first=="SIMEON")] <- "SEMYON"
  master.list$last[which(master.list$last=="VANDE VELDE" & master.list$first=="CHRIS")] <- "VANDEVELDE"
  
  master.list$first[which(master.list$last=="WHITE" & master.list$first=="COLIN (JOHN)")] <- "COLIN"
  
  
  master.list$first[which(master.list$last=="WOZNIEWSKI" & master.list$first=="ANDREW")] <- "ANDY"
  master.list$first[which(master.list$last=="WYMAN" & master.list$first=="JT")] <- "JAMES"
  
  master.list$first[which(master.list$last=="YORK" & master.list$first=="MICHAEL")] <- "MIKE"
  master.list$first[which(master.list$last=="ZHERDEV" & master.list$first=="NIKOLAY")] <- "NIKOLAI"
  master.list$first[which(master.list$last=="ZOLNIERCZYK" & master.list$first=="HARRISON")] <- "HARRY"
  
  master.list <- master.list[order(master.list$last,
                                   master.list$first),]
  #master.list$numfirstlast <- with(master.list, paste(num, first, last))
  
  
  return(master.list)
}

.simpleCap <- function(x, ch=" ", ch2=ch) {
  s <- strsplit(x, ch)[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = ch)
}

manual.patches <- function (roster.unique) {
  
  roster.unique$first <- sapply(roster.unique$first, .simpleCap)
  roster.unique$first <- sapply(roster.unique$first, .simpleCap, "-")
  #roster.unique$first <- sapply(roster.unique$first, .simpleCap, "\\.")
  
  roster.unique$last <- sapply(roster.unique$last, .simpleCap)
  roster.unique$last <- sapply(roster.unique$last, .simpleCap, "-")
  #roster.unique$last <- sapply(roster.unique$last, .simpleCap, "\\.")
  
  substr(roster.unique$last[grep("^Mac", roster.unique$last)], 4,4) <-
    toupper(substr(roster.unique$last[grep("^Mac", roster.unique$last)], 4,4))
  substr(roster.unique$last[grep("^Mc", roster.unique$last)], 3,3) <-
    toupper(substr(roster.unique$last[grep("^Mc", roster.unique$last)], 3,3))
  roster.unique$last <- gsub("^Van ", "van ", roster.unique$last)
  roster.unique$firstlast <- paste(roster.unique$first, roster.unique$last)
  return(roster.unique)
  
}

fold.frames <- function(frame.list) {
  #frame.list = new.pbp.2
  
  if (length(frame.list) > 1) repeat {
    if (length(frame.list) == 1) break
    hold.list <- list()
    for (kk in 1:floor(length(frame.list)/2))
      hold.list[[kk]] <- tryCatch(
        rbind(frame.list[[2*kk-1]], frame.list[[2*kk]]),
        warning = function(war) message(paste(kk, war)),
        error = function(err) message(paste(kk, err)),
        finally = {})
    if (length(frame.list) %% 2 == 1)
      if (length(hold.list) > 0)
        hold.list[[length(hold.list)]] <- rbind(hold.list[[length(hold.list)]],
                                                frame.list[[2*kk+1]]) else hold.list <- frame.list[2*kk+1]
                                                
                                                frame.list <- hold.list
                                                rm(hold.list)
                                                message ("Folding data frames. Total: ",length(frame.list))
                                                if (length(frame.list) == 1) break
  }
  
  return(frame.list[[1]])
}

augment.game <- function (game.info, player.list) {
  #game.info=sample.game; player.list=roster; season=""; gcode=""
  
  playbyplay <- game.info$playbyplay; teams <- game.info$teams
  if (length(playbyplay) == 0) stop ("Play-by-play table does not exist.")
  if (length(player.list) == 0) stop ("Player roster does not exist.")
  
  
  #replace players with ID numbers.
  for (cc in c(paste0("a",1:6), paste0("h",1:6), "away.G", "home.G", "ev.player.1", "ev.player.2", "ev.player.3")) {
    replacement <- player.list$player.id[match(playbyplay[,cc], player.list$numfirstlast)]
    if (is.null(replacement)) replacement <- rep(NA, dim(playbyplay)[1])
    playbyplay[,cc] <- replacement
    playbyplay[is.na(playbyplay[,cc]),cc] <- 1
  }
  
  #playbyplay <- patch.for.shottypes(playbyplay)
  return(playbyplay)
}

pick.section <- function (xy.points) {
  
  in.1 <- apply(nhlscrapr::quadsarray[1:3,,], 3, in.tri.rev, xy.points)
  in.2 <- apply(nhlscrapr::quadsarray[c(1,3,4),,], 3, in.tri.rev, xy.points)
  picks <- in.1 | in.2
  picks[is.na(picks)] <- FALSE
  
  picker <- function (row) if (sum(row)>0) min(which(row)) else 0
  sections <- apply (picks, 1, picker)
  return(sections)
}

in.triangle <- function (xyp, tr) {
  area <- (-tr[5]*tr[3] + tr[4]*(tr[3]-tr[2]) + tr[1]*(-tr[6]+tr[5]) + tr[2]*tr[6])/2
  ss <- 1/2/area * (tr[4]*tr[3] - tr[1]*tr[6] + (tr[6]-tr[4])*xyp[,1] + (tr[1]-tr[3])*xyp[,2])
  tt <- 1/2/area * (tr[1]*tr[5] - tr[4]*tr[2] + (tr[4]-tr[5])*xyp[,1] + (tr[2]-tr[1])*xyp[,2])
  output <- (ss > 0 & tt > 0 & 1 > ss+tt)
  return(output)
}
in.tri.rev <- function (tr=matrix(c(0,0.5,1, 0,1,0), nrow=3), xy.points) in.triangle (xy.points, tr)