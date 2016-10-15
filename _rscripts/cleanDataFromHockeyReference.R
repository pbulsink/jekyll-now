cleanHockeyData <- function(hockeyData, cleanTeams = TRUE, identifyTies = TRUE, listWinnersLosers = TRUE, removeInternational = TRUE) {
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
    hockeyData <- subset(hockeyData, select = -LOG)
    hockeyData <- subset(hockeyData, select = -X)
    hockeyData$Date <- as.Date(hockeyData$Date)
    hockeyData$Att. <- as.integer(hockeyData$Att.)
    names(hockeyData)[names(hockeyData) == "G"] <- "VisitorGoals"
    names(hockeyData)[names(hockeyData) == "G.1"] <- "HomeGoals"
    names(hockeyData)[names(hockeyData) == "X.1"] <- "OTStatus"
    
    if (identifyTies) {
        hockeyData$Tie <- FALSE
        hockeyData[allHockeyData$X.1 %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT", "SO"), ]$Tie <- TRUE
    }
    
    # Remove games against international teams
    if (removeInternational) {
        message("dropping international games")
        hockeyData <- hockeyData[hockeyData$Visitor != "Soviet All-Stars", ]
        hockeyData <- hockeyData[hockeyData$Visitor != "Czechoslovakia", ]
        hockeyData <- hockeyData[hockeyData$Visitor != "Finland", ]
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
        
        hockeyData[hockeyData$Visitor == "Winnipeg Jets" & hockeyData$Date < as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Winnipeg Jets (historical)"
        hockeyData[hockeyData$Home == "Winnipeg Jets" & hockeyData$Date < as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Home <- "Winnipeg Jets (historical)"
        hockeyData[hockeyData$Visitor == "Ottawa Senators" & hockeyData$Date < as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Ottawa Senators (historical)"
        hockeyData[hockeyData$Home == "Ottawa Senators" & hockeyData$Date < as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Home <- "Ottawa Senators (historical)"
        
        message("reguar replacements")
        for (t in teamReplace) {
            hockeyData[hockeyData$Visitor == t[1], ]$Visitor <- t[2]
            hockeyData[hockeyData$Home == t[1], ]$Home <- t[2]
        }
        
        hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & hockeyData$Visitor == "Minnesota Fighting Saints", ]$Visitor <- "Cleveland Crusaders"
        hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & hockeyData$Home == "Minnesota Fighting Saints", ]$Home <- "Cleveland Crusaders"
        
        hockeyData$Visitor <- droplevels(hockeyData$Visitor)
        hockeyData$Home <- droplevels(hockeyData$Home)
    }
    
    if (listWinnersLosers) {
        message("find winners and losers")
        hockeyData$Winner <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] > x[5], x[2], x[4])))
        # <= works because for ELO a winner & loser don't matter in tie.
        hockeyData$Loser <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] <= x[5], x[2], x[4])))
    }
    
    return(hockeyData)
}
