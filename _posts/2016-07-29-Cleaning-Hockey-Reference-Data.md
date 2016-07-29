---
title: "Cleaning Hockey-Reference Data"
author: "Philip Bulsink"
date: "July 28, 2016"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey scraping cleaning
---
 

 
Having downloaded data from Hockey-Reference.com in the [last post]({{ site.baseurl }}/blog/2016-07-28/Getting-Data-Part-One.html), we'll now want to prepare it for analysis. This will involve combining all of the files into one dataset, and doing some cleaning. Depending on our planned usage, we may wish to alter team names to provide continuity for moved teams (think Quebec Nordiques to Colorado Avalanche), or to isolate teams that have existed a few times (think about the Winnipeg Jets, or the Ottawa Senators).
 
<!--more-->
 
We'll start with combining all the data to one frame. I'm using data from both the NHL and WHA, but fortunately, the data arrives quite uniform. We can actually just merge the data frames we get from `read.csv` together.
 
Assuming the data is in the `./_data/` folder (that's where mine is), a short set of code will do it all. While there's lots of ways to do this, one that doesn't use any additional libraries is here:
 

{% highlight r %}
fileMerge <- function(pathToFiles = ".") {
    f <- list.files(path = pathToFiles, full.names = TRUE, pattern = "*.csv")
    d <- lapply(f, function(x) read.csv(file = x))
    unifiedData <- do.call(rbind, d)
    return(unifiedData)
}

allHockeyData <- fileMerge("./_data/")
{% endhighlight %}
 
In this function, the files are discovered by the `list.files` call to the provided or default path. The files are read sequentially by `lapply` call on `read.csv`, producing a list of `data.frame`s The `do.call` function is a built-in that iterates through the provided list `d`, and applies `rbind` across each item, resulting in one `data.frame`.
 

{% highlight r %}
head(allHockeyData)
{% endhighlight %}



{% highlight text %}
##   X       Date            Visitor  G               Home G.1 X.1 Att.  LOG
## 1 1 1917-12-19     Toronto Arenas  9 Montreal Wanderers  10     <NA> <NA>
## 2 2 1917-12-19 Montreal Canadiens  7    Ottawa Senators   4     <NA> <NA>
## 3 3 1917-12-22 Montreal Canadiens 11 Montreal Wanderers   2     <NA> <NA>
## 4 4 1917-12-22    Ottawa Senators  4     Toronto Arenas  11     <NA> <NA>
## 5 5 1917-12-26    Ottawa Senators  6 Montreal Wanderers   3     <NA> <NA>
## 6 6 1917-12-26 Montreal Canadiens  5     Toronto Arenas   7     <NA> <NA>
##   Notes
## 1      
## 2      
## 3      
## 4      
## 5      
## 6
{% endhighlight %}
 
Looking at the head of the data, we see there's some columns that need better names, and a deeper look at the data says that there are a few that likely can be dropped. There's a mix up in types too, Date should be dates, instead of factors, and some integer lines are as strings. 
 
A few times in the past teams have moved, merged, changed names, or otherwise changed identities in the way the score was kept. Searching through the unique names and deep wikipedia diving have helped me create the following 'team key':
 
```
Teams Movement (Alphabetical by first appearance)
 
  [1] Alberta Oilers --> Edmonton Oilers 
  [2] Mighty Ducks of Anaheim --> Anaheim Ducks 
* [3] Winnipeg Jets (1972-1996) --> Phoenix Coyotes --> Arizona Coyotes 
  [4] Atlanta Flames --> Calgary Flames 
  [5] Atlanta Thrashers --> Winnipeg Jets 
  [6] Toronto Toros --> Ottawa Nationals --> Birmingham Bulls
  [7] Boston Bruins 
  [8] Quebec Athletic Club/Bulldogs --> Hamilton Tigers --> New York Americans --> Brooklyn Americans
  [9] Buffalo Sabres
 [10] Philadelphia Blazers --> Vancouver Blazers --> Calgary Cowboys
 [11] Oakland Seals --> California Golden Seals --> Cleveland Barons (merged with Minnesota North Stars in 1978)
 [12] New England Whalers --> Hartford Whalers --> Carolina Hurricanes
 [13] Chicago Black Hawks --> Chicago Blackhawks 
 [14] Chicago Cougars
 [15] Cincinnati Stingers
*[16] Cleveland Crusaders --> Minnesota Fighting Saints (1976-1977)
 [17] Quebec Nordiques --> Colorado Avalanche 
 [18] Kansas City Scouts --> Colorado Rockies --> New Jersey Devils 
 [19] Columbus Blue Jackets 
*[20] Minnesota North Stars (merged with Cleveland Barons in 1978) --> Dallas Stars 
 [21] Denver Spurs/Ottawa Civics               
 [22] Detroit Cougars --> Detroit Falcons --> Detroit Red Wings 
 [23] Houston Aeros                            
 [24] Indianapolis Racers
 [25] Los Angeles Kings 
 [26] Los Angeles Sharks --> Michigan Stags/Baltimore Blades
*[27] Minnesota Fighting Saints (1972-1976)                
 [28] Minnesota Wild 
 [29] Montreal Canadiens                       
 [30] Montreal Maroons                         
 [31] Montreal Wanderers                       
 [32] Nashville Predators 
 [33] New York Raiders --> New York Golden Blades/New Jersey Knights --> San Diego Mariners
 [34] New York Islanders                       
 [35] New York Rangers                         
*[36] Ottawa Senators (historical 1883-1934) --> St. Louis Eagles
 [37] Ottawa Senators 
 [38] Philadelphia Flyers 
 [39] Pittsburgh Pirates --> Philadelphia Quakers 
 [40] Phoenix Roadrunners                      
 [41] Pittsburgh Penguins 
 [42] San Jose Sharks 
 [43] St. Louis Blues 
 [44] Tampa Bay Lightning 
 [45] Toronto Arenas --> Toronto St. Patricks --> Toronto Maple Leafs 
 [46] Vancouver Canucks 
 [47] Washington Capitals 
```
 
If you look carefully, you'll find a few international teams making appearances in the league games:
 

{% highlight r %}
tail(unique(allHockeyData$Visitor), 3)
{% endhighlight %}



{% highlight text %}
## [1] Czechoslovakia   Soviet All-Stars Finland         
## 80 Levels: Montreal Canadiens Montreal Wanderers ... Finland
{% endhighlight %}
 
And, looking carfully, you'll find games cancelled or not yet played:
 

{% highlight r %}
head(allHockeyData[is.na(allHockeyData$G.1), ])
{% endhighlight %}



{% highlight text %}
##         X       Date               Visitor  G                Home G.1  X.1
## 53856 648 2014-01-07   Carolina Hurricanes NA      Buffalo Sabres  NA     
## 53962 754 2014-01-21   Carolina Hurricanes NA Philadelphia Flyers  NA     
## 53978 770 2014-01-24       Ottawa Senators NA Carolina Hurricanes  NA     
## 54182 974 2014-03-10 Columbus Blue Jackets NA        Dallas Stars  NA     
## 57176   1 2016-10-12       St. Louis Blues NA  Chicago Blackhawks  NA <NA>
## 57177   2 2016-10-12        Calgary Flames NA     Edmonton Oilers  NA <NA>
##       Att.  LOG
## 53856 <NA> <NA>
## 53962 <NA> <NA>
## 53978 <NA> <NA>
## 54182 <NA> <NA>
## 57176 <NA> <NA>
## 57177 <NA> <NA>
##                                                              Notes
## 53856                   Postponed due to blizzard until 2014-02-25
## 53962                  Postponed due to snowstorm until 2014-01-22
## 53978 Rescheduled to 2014-01-25 due to rescheduled 2014-01-22 game
## 54182          Game postponed until 2014-04-09 (Medical Emergency)
## 57176                                                         <NA>
## 57177                                                         <NA>
{% endhighlight %}
 
There are some usages where explicit Winner or Loser columns are ideal, or a boolean 'Tie' flag. For both of these I'm thinking of the [`EloRating`](https://cran.r-project.org/web/packages/EloRating/index.html) package, which I'll talk about later.
 
Putting all of our requirements into one function leaves us with this. I've chosen to place each team substitution in a vector as a pair, then iterate over the data frame to make the substitution. There's a few manual switches with date filters made to avoid collisions between the old and new versions of teams.
 

{% highlight r %}
cleanHockeyData <- function(hockeyData, cleanTeams = TRUE, identifyTies = TRUE, 
    listWinnersLosers = TRUE, removeInternational = TRUE) {
    teamReplace <- list(list("Alberta Oilers", "Edmonton Oilers"), list("Mighty Ducks of Anaheim", 
        "Anaheim Ducks"), list("Winnipeg Jets (historical)", "Arizona Coyotes"), 
        list("Phoenix Coyotes", "Arizona Coyotes"), list("Atlanta Flames", "Calgary Flames"), 
        list("Atlanta Thrashers", "Winnipeg Jets"), list("Toronto Toros", "Birmingham Bulls"), 
        list("Ottawa Nationals", "Birmingham Bulls"), list("Quebec Athletic Club/Bulldogs", 
            "Brooklyn Americans"), list("Hamilton Tigers", "Brooklyn Americans"), 
        list("New York Americans", "Brooklyn Americans"), list("Philadelphia Blazers", 
            "Calgary Cowboys"), list("Vancouver Blazers", "Calgary Cowboys"), 
        list("Oakland Seals", "Cleveland Barons"), list("California Golden Seals", 
            "Cleveland Barons"), list("New England Whalers", "Carolina Hurricanes"), 
        list("Hartford Whalers", "Carolina Hurricanes"), list("Chicago Black Hawks", 
            "Chicago Blackhawks"), list("Quebec Nordiques", "Colorado Avalanche"), 
        list("Kansas City Scouts", "New Jersey Devils"), list("Colorado Rockies", 
            "New Jersey Devils"), list("Minnesota North Stars", "Dallas Stars"), 
        list("Detroit Cougars", "Detroit Red Wings"), list("Detroit Falcons", 
            "Detroit Red Wings"), list("Los Angeles Sharks", "Michigan Stags/Baltimore Blades"), 
        list("New York Raiders", "San Diego Mariners"), list("New York Golden Blades/New Jersey Knights", 
            "San Diego Mariners"), list("Pittsburgh Pirates", "Philadelphia Quakers"), 
        list("Toronto Arenas", "Toronto Maple Leafs"), list("Toronto St. Patricks", 
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
        hockeyData[allHockeyData$X.1 %in% c("2OT", "3OT", "4OT", "5OT", "6OT", 
            "OT", "SO"), ]$Tie <- TRUE
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
        
        hockeyData[hockeyData$Visitor == "Winnipeg Jets" & hockeyData$Date < 
            as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Winnipeg Jets (historical)"
        hockeyData[hockeyData$Home == "Winnipeg Jets" & hockeyData$Date < as.Date("1997-01-01", 
            format = "%Y-%m-%d"), ]$Home <- "Winnipeg Jets (historical)"
        hockeyData[hockeyData$Visitor == "Ottawa Senators" & hockeyData$Date < 
            as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Ottawa Senators (historical)"
        hockeyData[hockeyData$Home == "Ottawa Senators" & hockeyData$Date < 
            as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Home <- "Ottawa Senators (historical)"
        
        message("reguar replacements")
        for (t in teamReplace) {
            hockeyData[hockeyData$Visitor == t[1], ]$Visitor <- t[2]
            hockeyData[hockeyData$Home == t[1], ]$Home <- t[2]
        }
        
        hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & 
            hockeyData$Visitor == "Minnesota Fighting Saints", ]$Visitor <- "Cleveland Crusaders"
        hockeyData[hockeyData$Date > as.Date("1976-09-01", format = "%Y-%m-%d") & 
            hockeyData$Home == "Minnesota Fighting Saints", ]$Home <- "Cleveland Crusaders"
        
        hockeyData$Visitor <- droplevels(hockeyData$Visitor)
        hockeyData$Home <- droplevels(hockeyData$Home)
    }
    
    if (listWinnersLosers) {
        message("find winners and losers")
        hockeyData$Winner <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] > 
            x[5], x[2], x[4])))
        # <= works because for ELO a winner & loser don't matter in tie.
        hockeyData$Loser <- as.factor(apply(hockeyData, 1, function(x) ifelse(x[3] <= 
            x[5], x[2], x[4])))
    }
    
    return(hockeyData)
}
{% endhighlight %}
 
Using this function, we see the new output:

{% highlight r %}
cleanData <- cleanHockeyData(allHockeyData)
{% endhighlight %}



{% highlight text %}
## retype frame
{% endhighlight %}



{% highlight text %}
## Warning in cleanHockeyData(allHockeyData): NAs introduced by coercion
{% endhighlight %}



{% highlight text %}
## dropping international games
{% endhighlight %}



{% highlight text %}
## dropping unplayed games - future or past cancelled
{% endhighlight %}



{% highlight text %}
## special cases
{% endhighlight %}



{% highlight text %}
## reguar replacements
{% endhighlight %}



{% highlight text %}
## find winners and losers
{% endhighlight %}



{% highlight r %}
head(cleanData)
{% endhighlight %}



{% highlight text %}
##         Date             Visitor VisitorGoals                Home
## 1 1917-12-19 Toronto Maple Leafs            9  Montreal Wanderers
## 2 1917-12-19  Montreal Canadiens            7    St. Louis Eagles
## 3 1917-12-22  Montreal Canadiens           11  Montreal Wanderers
## 4 1917-12-22    St. Louis Eagles            4 Toronto Maple Leafs
## 5 1917-12-26    St. Louis Eagles            6  Montreal Wanderers
## 6 1917-12-26  Montreal Canadiens            5 Toronto Maple Leafs
##   HomeGoals OTStatus Att. Notes   Tie              Winner
## 1        10            NA       FALSE  Montreal Wanderers
## 2         4            NA       FALSE  Montreal Canadiens
## 3         2            NA       FALSE  Montreal Canadiens
## 4        11            NA       FALSE Toronto Maple Leafs
## 5         3            NA       FALSE    St. Louis Eagles
## 6         7            NA       FALSE Toronto Maple Leafs
##                 Loser
## 1 Toronto Maple Leafs
## 2    St. Louis Eagles
## 3  Montreal Wanderers
## 4    St. Louis Eagles
## 5  Montreal Wanderers
## 6  Montreal Canadiens
{% endhighlight %}
 
We'll stash this cleaned data frame back in the `./_data/` folder, to make it easier to use in the future. 
 

{% highlight r %}
saveRDS(cleanData, "./_data/hockeyData.Rds")
{% endhighlight %}
 
We'll dig into this data at a later point.
