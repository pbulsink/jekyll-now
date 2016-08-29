---
title: "nhlscrapr and Play-By-Play Data"
author: "Philip Bulsink"
date: "August 31, 2016"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey play-by-play nhlscrapr
---
 
As interesting as it is to predict how well teams will do on a team-by-team basis, based on their past performance, it would be great to get better granularity and be able to dig into what happened each game. This data is available, online, from the NHL website. Manually downloading it all would be horrendous (there are 1230 games each year, plus playoffs). Fortunately, a package exists in CRAN to help with this.
 
<!--more-->
 

{% highlight r %}
library(nhlscrapr)
{% endhighlight %}



{% highlight text %}
## nhlscrapr v 1.8
{% endhighlight %}
 
(un?)Fortunately, the authors of nhlscrapr got jobs with NHL teams, and gave up working on the project. Others have written update patches, such as Jack Davis at the [Musings on Statistics et.al.](https://factotumjack.blogspot.ca/2016/04/nhlscrapr-revisited.html) blog. You can download the patch from his site, and apply it as directed. I've made a few extra adjustments to get 2015-2016 to properly process, and this can be found on [my GitHub](https://github.com/pbulsink/pbulsink.github.io/raw/master/_rscripts/nhlscraprUpdate.R).  
 

{% highlight r %}
source("./_rscripts/nhlscraprUpdate.R")
{% endhighlight %}



{% highlight text %}
## Loading required package: plyr
{% endhighlight %}
 
With the library and patch loaded, we can start getting all of the data from the NHL site. 
 
We start by defining the database of all the games available:
 

{% highlight r %}
all_games<-full.game.database()
{% endhighlight %}
 
We use the `extra.seasons` parameter because the original code was written to cover up to 2014-2015. By calling in some extra.seasons, we can get the game data for later seasons. 
 
Because the data is quite large, and downloading takes a long time (even with a short delay between games), it's common to work on one season at a time.
 

{% highlight r %}
download_season = "20152016"
game_ids = subset(all_games, season == download_season)
 
dummy = download.games(games = game_ids, wait = 5)
process.games(games=game_ids,override.download=FALSE)
gc()
compile.all.games(output.file="./_data/nhlscrapr-20152016.RData")
{% endhighlight %}
 
Once this has completed running (about 2 hours), there's two files. Our nhlscrapr-20152016.RData file, as well as nhlscrapr-core.RData. The first stores the play by play data, and the second is a legend for player identifications. 
 
We'll load the data into the environment and play with them.
 

{% highlight r %}
play_by_play<-get(load("./_data/nhlscrapr-20152016.RData"))
roster<-get(load("./_data/nhlscrapr-core.RData"))
{% endhighlight %}
 
Let's look at the data to see what we have:

{% highlight r %}
head(roster)
{% endhighlight %}



{% highlight text %}
##   pos      last    first        numfirstlast        firstlast index
## 1                                                                 1
## 2   R ARCOBELLO     MARK   33 MARK ARCOBELLO   MARK ARCOBELLO     2
## 3   D  BEAULIEU   NATHAN  28 NATHAN BEAULIEU  NATHAN BEAULIEU     3
## 4   G   BERNIER JONATHAN 45 JONATHAN BERNIER JONATHAN BERNIER     4
## 5   R     BOYES     BRAD       28 BRAD BOYES       BRAD BOYES     5
## 6   C     BOZAK    TYLER      42 TYLER BOZAK      TYLER BOZAK     6
##   player.id pC pL pR pD pG
## 1         1  0  0  0  0  0
## 2         2  0  0 20  0  0
## 3         3  0  0  0 64  0
## 4         4  0  0  0  0 74
## 5         5  0  0 60  0  0
## 6         6 57  0  0  0  0
{% endhighlight %}
 
Each player in the roster is listed, with an indexing number (index of 1 is a 'no player' cheat index). Each player's numbers of appearance in each position is given too (pC, pL, pR, pD, pG). 
 
The index makes an appearance in the `play_by_play` data as well.

{% highlight r %}
head(play_by_play)
{% endhighlight %}



{% highlight text %}
##     season gcode refdate event period seconds  etype a1 a2 a3 a4 a5 a6 h1
## 1 20152016 20001    5027     1      1     0.0    FAC 31 14 27 38 24  1 25
## 2 20152016 20001    5027     3      1    14.0    FAC 31 14 27 38 24  1 25
## 3 20152016 20001    5027     4      1    27.0 CHANGE 31 14 27 38 24  1 25
## 4 20152016 20001    5027     5      1    40.0    HIT  8 14 11 10 38  1 25
## 5 20152016 20001    5027     6      1    45.5 CHANGE  8 14 11 10 38  1 25
## 6 20152016 20001    5027     7      1    51.0   SHOT  8 40 11 29 10  1 20
##   h2 h3 h4 h5 h6 ev.team ev.player.1 ev.player.2 ev.player.3 distance
## 1  6 22 19 30  1     MTL          31           6           1       NA
## 2  6 22 19 30  1     MTL          31           6           1       NA
## 3  6 22 19 30  1                   1           1           1       NA
## 4 20  5 19 30  1     MTL          14          19           1       NA
## 5 20  5 19 30  1                   1           1           1       NA
## 6  5 39 17 15  1     TOR           5           1           1       35
##    type homezone xcoord ycoord awayteam hometeam home.score away.score
## 1            Neu     NA     NA      MTL      TOR          0          0
## 2            Neu     NA     NA      MTL      TOR          0          0
## 3            Neu     NA     NA      MTL      TOR          0          0
## 4            Def     94    -34      MTL      TOR          0          0
## 5            Neu     NA     NA      MTL      TOR          0          0
## 6 Wrist      Off    -55      6      MTL      TOR          0          0
##   event.length away.G home.G home.skaters away.skaters adjusted.distance
## 1          0.0     32      4            6            6                NA
## 2         14.0     32      4            6            6                NA
## 3         13.0     32      4            6            6                NA
## 4         13.0     32      4            6            6                NA
## 5          5.5     32      4            6            6                NA
## 6          5.5     32      4            6            6                NA
##   shot.prob.distance prob.goal.if.ongoal loc.section new.loc.section newxc
## 1                 NA                  NA           0               0    NA
## 2                 NA                  NA           0               0    NA
## 3                 NA                  NA           0               0    NA
## 4                 NA                  NA          15              15    94
## 5                 NA                  NA           0               0    NA
## 6                 NA                  NA           6               6   -55
##   newyc
## 1    NA
## 2    NA
## 3    NA
## 4   -34
## 5    NA
## 6     6
{% endhighlight %}
 
There's lots of info here. When it happened, what kind of event, who was on (player index), which team 'won' the event, which players it happened to, distance to the net, location on the ice, the score at that time, who was in net, how many players on the ice... 
 
Soon we'll go throught some real data analysis as we try to get an idea of what we can all pull from this.
