---
title: "Scoring ELO Over a Season"
author: "Philip Bulsink"
date: "January 26, 2017"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey elo prediction scoring
---
 

 
On Twitter, there are many excellent hockey analytics folks. One in particular, [@IneffectiveMath](https://twitter.com/ineffectivemath), is worth following. Amongst other things, such as great visualizations of data, he's running a contest this year that is rating models on their season-long predictions, using the following scoring scheme: 
 
<!--more-->
 
> The score associated to a given estimate will be computed as the probability of drawing the actual result from a normal distribution with mean given by the estimated point total and standard deviation givin by the estimated uncertainty. The maximum possible score for a single team is 1, which can only be obtained by specifying the team's point total exactly with an uncertainty of 0 -- in this case you will score 0 for even the slightest deviation from your estimate. Specifying a larger uncertainty will help you capture some points even when your estimate is poor; specifying a smaller uncertianty will give you a larger score when your estimate is good. The overall score is the sum of the thirty team scores, so the maximum theoretical score is thirty. 
 
Thus, the way to score a prediction is, at the end of the season, to sum up the `dnorm(x=ActualPoints, mean = Points, sd=PointsSD)`. This is vectorized, you can sub in a column of a data.frame and then get the `sum()`. 
 
So, let's try it. 
 
Having prepared a 2014-2015 schedule, and recorded the 2014-2015 season points for each team, we can run our Elo rating predictions through that method to see how they do. 
 
We'll simulate a season using some more `R` type programming than past season predictors. We sample from the list `[0, 0.4, 0.6, 1]`, corresponding to an away win, away OT win, home OT win, and home win, with the probabilities based on pWin (of the home team winning) of `[1-pWin, 0.1-0.1*pWin, 0.1*pWin, pWin]`. This gives us about a 10% chance of going into OT, with the chances of a team in OT the same as in regular time. While this sometimes adds up to more than 1 (eg: `pWin = 1 --> odds = [0, 0, 0.1, 1]`), the `R` function `sample()` can scale probabilites if needed. 
 
We'll vectorize the function to sample to make it easy to feed in a list of pWins, and get a matrix of samples back, easily repeated the number of times we want to simulate a season. 

{% highlight r %}
predictEloWins.vec<-Vectorize(FUN = function(pWin, n_sims){
    return(sample(c(0,0.4, 0.6, 1), 
                  size = n_sims, 
                  replace = TRUE, 
                  prob=c(1-pWin, 0.1-0.1*pWin, 0.1*pWin, pWin)))},
    vectorize.args = 'pWin')
{% endhighlight %}
 
Similarly, we'll vectorize a formula to give us the odds of a home team win given the Elo rating difference between two teams: 

{% highlight r %}
predictEloResults.vec<-Vectorize(FUN = function(elo_diff, h_adv=0){
    return(1/(1 + (10^(((-1*elo_diff)+h_adv)/400))))},
    vectorize.args = 'elo_diff')
{% endhighlight %}
 
Finally, those two functions get put together with some data munging and compilation, and we get a predicted finish for each team after a certain number of sims.
 

{% highlight r %}
predictSeasonElo<-function(elo_data, schedule, n_sims=10){
    nhl_teams<-unique(c(as.character(schedule$Home), as.character(schedule$Visitor)))
    start_date<-as.Date(head(schedule$Date, 1))
    elo<-tail(elo_data[elo_data$Date < start_date,],1)
    elo_long<-melt(elo, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)
 
    schedule$EloDiff<-apply(schedule, 1, function(x) elo_long[elo_long$Team == make.names(x[2]),"Rating"]-elo_long[elo_long$Team == make.names(x[3]),"Rating"])
 
    schedule$PHome<-predictEloResults.vec(schedule$EloDiff, h_adv = 0)
 
    results <- matrix(predictEloWins.vec(schedule$PHome, n_sims), ncol=n_sims, byrow = TRUE)
 
    team_performance<-matrix(NA, nrow=length(nhl_teams), ncol=n_sims * 3)
    rownames(team_performance)<-make.names(nhl_teams)
    for(i in 1:n_sims){
        for(team in nhl_teams){
            team_performance[make.names(team), i]<-sum(results[schedule$Home == team,i]>0.5) + sum(results[schedule$Visitor == team, i]<0.5)
            team_performance[make.names(team), i+n_sims]<-sum(results[schedule$Home == team, i]==0.4)+sum(results[schedule$Visitor == team, i]==0.4)
            team_performance[make.names(team), i+2*n_sims]<-2 * team_performance[make.names(team), i] + team_performance[make.names(team), i+n_sims]
 
        }
    }
    team_results<-data.frame("Team"=nhl_teams)
    team_results$WinsMax<-apply(team_performance[,1:n_sims], 1, max)
    team_results$WinsMin<-apply(team_performance[,1:n_sims], 1, min)
    team_results$Wins<-apply(team_performance[,1:n_sims], 1, mean)
    team_results$WinsSD<-apply(team_performance[,1:n_sims], 1, sd)
    team_results$OTLoss<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossMax<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossMin<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$OTLossSD<-apply(team_performance[,(n_sims+1):(2*n_sims)], 1, mean)
    team_results$PointsMax<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, max)
    team_results$PointsMin<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, min)
    team_results$Points<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, mean)
    team_results$PointsSD<-apply(team_performance[,(2*n_sims+1):(3*n_sims)], 1, sd)
 
    return(team_results)
}
{% endhighlight %}
 
 
Now, when this runs, we get a table of expected Wins, OT Losses, and Points for each team (mean, max, min, and sd). This gives us the information we need to be able to calculate a score for the season. 
 

{% highlight r %}
results <- predictSeasonElo(elo_data, nhl15, 100)
results
{% endhighlight %}



{% highlight text %}
##                     Team WinsMax WinsMin  Wins   WinsSD OTLoss OTLossMax
## 1          Boston Bruins      59      41 50.16 4.244355   4.13      4.13
## 2         Calgary Flames      45      24 35.68 4.572381   3.73      3.73
## 3      Los Angeles Kings      58      39 49.22 4.340507   3.79      3.79
## 4    Toronto Maple Leafs      47      24 34.40 4.261645   3.62      3.62
## 5        Arizona Coyotes      49      28 37.93 4.522123   3.76      3.76
## 6         Buffalo Sabres      37      17 26.69 4.096549   3.64      3.64
## 7           Dallas Stars      52      29 41.31 4.471447   3.78      3.78
## 8      Detroit Red Wings      54      31 42.05 4.808190   3.94      3.94
## 9        Edmonton Oilers      48      23 34.27 4.732981   3.56      3.56
## 10        Minnesota Wild      56      32 43.58 4.714431   3.75      3.75
## 11   Nashville Predators      54      30 42.97 4.648134   3.89      3.89
## 12   Philadelphia Flyers      58      34 44.77 4.331830   3.73      3.73
## 13   Pittsburgh Penguins      55      33 45.56 4.513325   3.59      3.59
## 14       St. Louis Blues      51      33 41.94 4.182262   3.78      3.78
## 15   Tampa Bay Lightning      49      31 39.21 4.243343   3.72      3.72
## 16   Washington Capitals      53      30 40.94 4.842614   3.91      3.91
## 17   Carolina Hurricanes      49      26 38.46 4.684856   3.72      3.72
## 18 Columbus Blue Jackets      56      30 41.58 4.680024   3.72      3.72
## 19    Chicago Blackhawks      55      33 45.36 4.475568   3.50      3.50
## 20    Colorado Avalanche      56      36 44.60 4.007569   3.98      3.98
## 21      Florida Panthers      40      21 30.18 4.390946   3.60      3.60
## 22    New York Islanders      48      25 37.54 4.276834   3.64      3.64
## 23       San Jose Sharks      54      30 44.32 4.317875   3.53      3.53
## 24     Vancouver Canucks      45      26 35.22 4.289004   3.82      3.82
## 25      New York Rangers      57      37 47.26 4.108675   3.29      3.29
## 26    Montreal Canadiens      58      34 47.91 4.876443   3.74      3.74
## 27       Ottawa Senators      50      31 40.16 3.732792   4.00      4.00
## 28         Anaheim Ducks      60      38 48.50 4.702245   3.62      3.62
## 29         Winnipeg Jets      50      29 37.73 4.345798   3.61      3.61
## 30     New Jersey Devils      53      28 40.50 4.628633   3.75      3.75
##    OTLossMin OTLossSD PointsMax PointsMin Points PointsSD
## 1       4.13     4.13       122        86 104.45 8.582782
## 2       3.73     3.73        96        50  75.09 9.523066
## 3       3.79     3.79       122        80 102.23 9.057454
## 4       3.62     3.62        97        51  72.42 8.590787
## 5       3.76     3.76       101        58  79.62 9.098540
## 6       3.64     3.64        76        37  57.02 8.179773
## 7       3.78     3.78       109        60  86.40 9.049806
## 8       3.94     3.94       110        67  88.04 9.795814
## 9       3.56     3.56       101        49  72.10 9.683371
## 10      3.75     3.75       116        66  90.91 9.830991
## 11      3.89     3.89       111        63  89.83 9.180628
## 12      3.73     3.73       121        76  93.27 8.827019
## 13      3.59     3.59       116        70  94.71 9.315237
## 14      3.78     3.78       105        70  87.66 8.317221
## 15      3.72     3.72       102        65  82.14 8.561153
## 16      3.91     3.91       110        66  85.79 9.964883
## 17      3.72     3.72       102        55  80.64 9.491644
## 18      3.72     3.72       118        63  86.88 9.841450
## 19      3.50     3.50       115        67  94.22 9.257899
## 20      3.98     3.98       116        72  93.18 8.026950
## 21      3.60     3.60        87        45  63.96 9.201361
## 22      3.64     3.64        98        54  78.72 8.893398
## 23      3.53     3.53       112        64  92.17 8.710907
## 24      3.82     3.82        96        57  74.26 8.816445
## 25      3.29     3.29       120        76  97.81 8.835986
## 26      3.74     3.74       122        69  99.56 9.896760
## 27      4.00     4.00       102        64  84.32 7.470994
## 28      3.62     3.62       124        78 100.62 9.530154
## 29      3.61     3.61       103        59  79.07 8.891325
## 30      3.75     3.75       110        59  84.75 9.339149
{% endhighlight %}
 
We can score it simply this way:
 

{% highlight r %}
results<-merge(results, nhl15actual)
sum(dnorm(x=results$ActualPoints, mean = results$Points, sd=results$PointsSD))
{% endhighlight %}



{% highlight text %}
## [1] 0.6446211
{% endhighlight %}
 
Next time we'll optimize to this instead of by-game metrics, and see how we do.
 
