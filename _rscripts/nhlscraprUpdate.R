### Patch for full.game.database and for player.summary
### for A.C. Thomas's nhlscrapr package version 1.8 ( act@acthomas.ca )
### by Jack Davis 2016-04-18 ( jackd@sfu.ca )

### Now works for extra seasons
full.game.database = function (extra.seasons = 0) 
{
    game.roster <- NULL
    seasons <- c("20022003", "20032004", "20052006", "20062007", "20072008", "20082009", "20092010", "20102011", "20112012", "20122013", "20132014", "20142015")
    if (extra.seasons > 0) 
	{
        seasons <- c(seasons, paste(2014 + 1:extra.seasons, 2015 + 1:extra.seasons, sep = ""))  ## Patched: Extra seasons now start at 20152016 - JD
	}
	
	### Patched: bad.game.list now include many NULLs for additional seasons.
	### 		Reason: R doesn't allow adding NULL elements to a list after the line has been instantiated. - JD
    games <- c(rep(1230, 9), 720, 1230, 1230, rep(1230, extra.seasons))
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
    playoff.series.lengths <- c(5, 5, 6, 7, 6, 4, 7, 7, 6, 5, 
        6, 7, 7, 4, 7, 5, 7, 5, 7, 6, 5, 7, 5, 4, 6, 6, 6, 7, 
        6, 7, 5, 6, 4, 6, 6, 5, 7, 5, 5, 5, 6, 4, 7, 5, 7, 5, 
        6, 4, 5, 6, 5, 7, 5, 6, 5, 6, 5, 5, 6, 5, 7, 4, 7, 5, 
        6, 7, 6, 6, 5, 5, 4, 6, 5, 6, 6, 4, 7, 7, 6, 6, 4, 4, 
        6, 7, 7, 7, 6, 4, 5, 7, 7, 5, 6, 6, 6, 6, 6, 7, 7, 7, 
        5, 6, 5, 4, 6, 5, 7, 7, 7, 7, 6, 4, 6, 4, 4, 6, 7, 7, 
        5, 7, 7, 7, 7, 6, 5, 5, 6, 5, 7, 5, 4, 5, 6, 5, 6, 6, 
        5, 7, 7, 5, 7, 4, 6, 5, 5, 7, 7, 4, 5, 6, 5, 4, 6, 7, 
        7, 6, 6, 7, 7, 7, 6, 7, 6, 7, 5, rep(7, 15), rep(7, 15 * 
            (extra.seasons)))
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


