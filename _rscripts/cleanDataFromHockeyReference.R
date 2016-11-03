cleanHockeyData <- function(hockeyData, cleanTeams = TRUE, identifyTies = TRUE, listWinnersLosers = TRUE, removeInternational = TRUE, eloResults=TRUE) {
    teamReplace <- list(list("Alberta Oilers", "Edmonton Oilers"), list("Mighty Ducks of Anaheim", "Anaheim Ducks"), list("Winnipeg Jets (historical)",
        "Arizona Coyotes"), list("Phoenix Coyotes", "Arizona Coyotes"), list("Atlanta Flames", "Calgary Flames"), list("Atlanta Thrashers", "Winnipeg Jets"),
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
    if ('Att.' %in% names(hockeyData)){
        hockeyData$Att. <- as.integer(hockeyData$Att.)
    }
    names(hockeyData)[names(hockeyData) == "G"] <- "VisitorGoals"
    names(hockeyData)[names(hockeyData) == "G.1"] <- "HomeGoals"
    names(hockeyData)[names(hockeyData) == "X.1"] <- "OTStatus"
    hockeyData$OTStatus <- as.character(hockeyData$OTStatus)

    if (identifyTies) {
        hockeyData$Tie <- FALSE
        hockeyData[allHockeyData$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT", "SO"), ]$Tie <- TRUE
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
        hockeyData$Result <- apply(hockeyData, 1, function(x) tieSort(x))
    }

    return(hockeyData)
}

tieSort<-function(x) {
    if (x[3] > x[5]){
        if (x[6] %in% c("SO")){
            return(0.4)
        }
        else if (x[6] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")){
            return(0.25)
        }
        else{
            return(0.0)
        }
    }
    else if (x[3] < x[5]){
        if (x[6] %in% c("SO")){
            return(0.6)
        }
        else if (x[6] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")){
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
