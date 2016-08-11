---
title: "Dixon-Coles Prediction of a Single Hockey Game"
author: "Philip Bulsink"
date: "August 08, 2016"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey cleaning predicting Dixon-Coles
---


 
*Note: This is earlier work I did (last winter/spring) so some info may seem dated at time of posting. I've used data files current to then.*
 
In [section 1]({{ site.baseurl }}/blog/2016-08-04/data_preparation.html), we prepared historical hockey data for analysis. In [section 2]({{ site.baseurl }}/blog/2016-08-05/dixon_coles_and_hockey_data.html), we set up some functions do prepare the Dixon-Coles parameters. Now, we can use them to predict a game. 
 
<!--more-->
 
Poisson analysis is good at predicting, based on an average, the proportion of each integer events happening. So, if we expect 3.1 goals to be scored by a certain team as an average, we know that the proportion of that is going to be (in R code) `dpois(3,3.1)`, equalling 0.2236768. Similarly, the chance that 1 and 2 goals is scored by that team to be `dpois(c(1,2), 3.1)` to be 0.1396525 and 0.2164614 respectively. 
 
The Dixon-Coles optimization gave us 'attack' and 'defence' parameters, along with a home field advantage factor `home` and a fudge fator for low-scoring games, 'rho' (&rho;). For two teams, Home and Away, we can calculate their expected goals socred by adding their attack, plus the opposing teams' defence, plus (for home) the home field advantage. This will give us a 'lambda' (&lambda;) and 'mu' (&mu;) which are the home and away goals expected. For example, let's use Toronto Maple Leafs vs. Montreal Canadiens:
 

{% highlight r %}
home <- "Montreal Canadiens"
away <- "Toronto Maple Leafs"
{% endhighlight %}
 
Montreal is a stronger team this year, we expect a higher attack and lower defence factor. For the 2015 season only, they are, in fact, 1.0892421 and -1.8038952. Similarly, Toronto's factors are 1.087436 and -1.7822052 respectively. 
 
We'll do the addition to get &lambda; and &mu;:

{% highlight r %}
# Expected goals home
lambda <- exp(res_2015$par["HOME"] + res_2015$par["Attack.Montreal Canadiens"] + 
    res_2015$par["Defence.Toronto Maple Leafs"])
# Expected goals away
mu <- exp(res_2015$par["Attack.Toronto Maple Leafs"] + res_2015$par["Defence.Montreal Canadiens"])
{% endhighlight %}
 
Thus, we expect Montreal to score 0.5932014 goals and Toronto to score 0.4884788 goals. 
 
Using this knowledge, we can create a probability matrix of Home and Away goals' Poisson probability. Using &lambda; and &mu;, `probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))`, and a maximum number of goals per team of 8:
 

| &nbsp;  |   0    |   1    |   2    |   3    |   4   |   5   |  6  |  7  |  8  |
|:-------:|:------:|:------:|:------:|:------:|:-----:|:-----:|:---:|:---:|:---:|
|  **0**  | 0.339  | 0.1656 | 0.0404 | 0.0066 | 8e-04 | 1e-04 |  0  |  0  |  0  |
|  **1**  | 0.2011 | 0.0982 | 0.024  | 0.0039 | 5e-04 |   0   |  0  |  0  |  0  |
|  **2**  | 0.0596 | 0.0291 | 0.0071 | 0.0012 | 1e-04 |   0   |  0  |  0  |  0  |
|  **3**  | 0.0118 | 0.0058 | 0.0014 | 2e-04  |   0   |   0   |  0  |  0  |  0  |
|  **4**  | 0.0017 | 9e-04  | 2e-04  |   0    |   0   |   0   |  0  |  0  |  0  |
|  **5**  | 2e-04  | 1e-04  |   0    |   0    |   0   |   0   |  0  |  0  |  0  |
|  **6**  |   0    |   0    |   0    |   0    |   0   |   0   |  0  |  0  |  0  |
|  **7**  |   0    |   0    |   0    |   0    |   0   |   0   |  0  |  0  |  0  |
|  **8**  |   0    |   0    |   0    |   0    |   0   |   0   |  0  |  0  |  0  |

Table: Probability of specific score matrix
 
This has the away score on the top (as columns), and the home score along the side (as rows). Recall that we need to apply the &tau; function to this matrix, to account for low scoring games. Once that is done, we can sum the diagonal of the matrix to find the probability of a tie. The sum of the upper triangle is the probability of an away win, and the sum of the lower triangle is the home win.
 
Putting it all together we get this function:
 

{% highlight r %}
predictResult <- function(res, home, away, maxgoal = 8) {
    attack.home <- paste("Attack", home, sep = ".")
    attack.away <- paste("Attack", away, sep = ".")
    defence.home <- paste("Defence", home, sep = ".")
    defence.away <- paste("Defence", away, sep = ".")
    
    # Expected goals home
    lambda <- exp(res$par["HOME"] + res$par[attack.home] + res$par[defence.away])
    # Expected goals away
    mu <- exp(res$par[attack.away] + res$par[defence.home])
    
    probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
    
    scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, res$par["RHO"]), 
        nrow = 2)
    probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
    
    HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
    DrawProbability <- sum(diag(probability_matrix))
    AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])
    
    return(c(HomeWinProbability, DrawProbability, AwayWinProbability))
}

probs_2015 <- predictResult(res_2015, home, away)
probs_all <- predictResult(res_all, home, away)
probs_2015_time <- predictResult(time_res_2015, home, away)
probs_all_time <- predictResult(time_res_all, home, away)
{% endhighlight %}
 
Now we can look at one of the results (say, 2015 not time weighted) and see the probability of a home win (Montreal), draw, or away win (Toronto): 0.3136673, 0.4414421, 0.2448906. In fact, we can plot the probabilities for the four fits we have, to show the effect of each fit type.
 
![plot of chunk hda_mtl_plot](/images/hda_mtl_plot-1.png)
 
While the odds of a draw haven't changed much, the odds of a Montreal or Toronto win have slightly. Note that over the past 10 years, Montreal has performed better, on average, than Toronto, so this is expected. As well, Monteal benefits from the home ice advantage. We can calculate this for a Toronto home game too.
 
![plot of chunk hda_tor_plot](/images/hda_tor_plot-1.png)
 
Why the difference in these two results? That's because the home advantage factor ranges not insignificantly. While not a huge range, it does impact the probabilities enough to make a noticeable difference.
 
We can predict scores for our Toronto at Montreal game with some random number work. First, we'll modify the probability matrix to contain the `sum` of the probabilities to that point. Then we can index goals based on the probability correllating to a random number.
 

{% highlight r %}
pmatrix <- matrix(nrow = nrow(probability_matrix), ncol = ncol(probability_matrix))
current_p <- 0
for (i in 1:ncol(pmatrix)) {
    for (j in 1:nrow(pmatrix)) {
        pmatrix[j, i] <- current_p
        current_p <- current_p + probability_matrix[j, i]
    }
}

predictOneGame <- function(pmatrix, stats, home, away) {
    random <- runif(1)
    
    # This ensures that there's no issue with random number larger than the
    # largest pmatrix probability
    while (random > pmatrix[nrow(pmatrix), ncol(pmatrix)]) {
        random <- runif(1)
    }
    
    score <- as.vector(which(pmatrix > random, arr.ind = T)[1, ])
    # scores is matrix c(home away), returning INDEX (ie 0-0 is 1,1)
    score <- score - 1
    return(score)
}
{% endhighlight %}
 
So, we can predict the score for a game with `predictOneGame`, and get a result of `home away`:

{% highlight r %}
predictOneGame(pmatrix, home, away)
{% endhighlight %}



{% highlight text %}
## [1] 1 0
{% endhighlight %}
 
Lets' do that a few times, to see the different results we get.

{% highlight r %}
t(replicate(10, predictOneGame(pmatrix, home, away)))
{% endhighlight %}



{% highlight text %}
##       [,1] [,2]
##  [1,]    2    0
##  [2,]    3    0
##  [3,]    4    1
##  [4,]    1    0
##  [5,]    3    1
##  [6,]    1    2
##  [7,]    1    1
##  [8,]    1    1
##  [9,]    1    0
## [10,]    1    0
{% endhighlight %}
 
But, recall that the NHL doesn't allow draws. We cold solve that by randomly choosing a winner, but that does a disservice to teams who excel at those scenarios. A simple way of adding OT is to use the `log5` method, invented by [Bill James](https://en.wikipedia.org/wiki/Bill_James), which applies the following formula, (from [Wikipedia](https://en.wikipedia.org/wiki/Log5)) The Log5 estimate for the probability of A defeating B is $p_{A,B} = \frac{p_A-p_A\times p_B}{p_A+p_B-2\times p_A\times p_B}$, where $p_A$ is the proportion of A wins, and $p_B$ is the proportion of B wins. For now, we'll plug in even values (we'll use `pa=pb=0.5`, so `log5 = (pa-(pa*pb))/(pa+pb-(2*pa*pb))` will equal 0.5 as well), but later we can re-evaluate performance of each team.
 
We'll also go 50/50 for Shootout and Overtime, but can adjust those odds later too.
 
Re-writing the score prediction formula will give us the chance to simulate draw handling.

{% highlight r %}
predictOneGame <- function(pmatrix, stats, home, away) {
    random <- runif(1)
    # This ensures that there's no issue with random number larger than the
    # largest pmatrix probability
    while (random > pmatrix[nrow(pmatrix), ncol(pmatrix)]) {
        random <- runif(1)
    }
    
    score <- as.vector(which(pmatrix > random, arr.ind = T)[1, ])
    # scores is matrix c(home away), returning INDEX (ie 0-0 is 1,1)
    score <- score - 1
    score[3] <- NA
    if (score[1] == score[2]) {
        if (runif(1) > 0.5) {
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
{% endhighlight %}
 
Trying this score simulation again:

{% highlight r %}
t(replicate(10, predictOneGame(pmatrix, home, away)))
{% endhighlight %}



{% highlight text %}
##       [,1] [,2] [,3]
##  [1,] "1"  "0"  NA  
##  [2,] "1"  "2"  "OT"
##  [3,] "2"  "0"  NA  
##  [4,] "2"  "1"  "SO"
##  [5,] "2"  "1"  "SO"
##  [6,] "1"  "2"  "SO"
##  [7,] "2"  "0"  NA  
##  [8,] "1"  "0"  NA  
##  [9,] "2"  "0"  NA  
## [10,] "2"  "1"  NA
{% endhighlight %}
 
To predict the winner of a game in OT, we'll use the win percentages of each team in the most recent season. Let's grab the results from 2015 and figure out each team's OT and Shootout results. We have to look at each game, so we'll take this opportunity to collect the rest of the stats available to make a stats table.
 

{% highlight r %}
makeStatsTable <- function(df) {
    tmpTable = data.frame(Team = sort(unique(df$AwayTeam)), GP = 0, W = 0, OTL = 0, 
        L = 0, ROW = 0, HomeGames = 0, HomeWin = 0, HomeOTW = 0, HomeSOW = 0, 
        HomeOTL = 0, HomeLoss = 0, AwayGames = 0, AwayWin = 0, AwayOTW = 0, 
        AwaySOW = 0, AwayOTL = 0, AwayLoss = 0, P = 0, HomeFor = 0, HomeAgainst = 0, 
        AwayFor = 0, AwayAgainst = 0, GF = 0, GA = 0, DIFF = 0, PP = 0, OT.Win.Percent = 0)
    
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
    tmpTable$HomeOTW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == 
        "OT")]))
    tmpTable$HomeSOW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == 
        "SO")]))
    
    tmpTable$AwayOTW = as.numeric(table(df$AwayTeam[(df$OT.Win == "A") & (df$OT.SO == 
        "OT")]))
    tmpTable$AwaySOW = as.numeric(table(df$AwayTeam[(df$OT.Win == "A") & (df$OT.SO == 
        "SO")]))
    
    # OT Losses
    tmpTable$HomeOTL = as.numeric(table(df$HomeTeam[(df$OT.Win == "A")]))
    tmpTable$AwayOTL = as.numeric(table(df$AwayTeam[(df$OT.Win == "H")]))
    
    # W/L/OTL/ROW
    tmpTable$GP = tmpTable$HomeGames + tmpTable$AwayGames
    tmpTable$W = tmpTable$HomeWin + tmpTable$AwayWin + tmpTable$HomeOTW + tmpTable$HomeSOW + 
        tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$OTL = tmpTable$HomeOTL + tmpTable$AwayOTL
    tmpTable$L = tmpTable$HomeLoss + tmpTable$AwayLoss
    tmpTable$ROW = tmpTable$W - (tmpTable$HomeSOW + tmpTable$AwaySOW)
    
    # Goal Diffs (includes OT scores)
    tmpTable$HomeFor = as.numeric(tapply(df$HG, df$HomeTeam, sum, na.rm = TRUE)) + 
        tmpTable$HomeOTW + tmpTable$HomeSOW
    tmpTable$HomeAgainst = as.numeric(tapply(df$AG, df$HomeTeam, sum, na.rm = TRUE)) + 
        tmpTable$HomeOTL
    
    tmpTable$AwayFor = as.numeric(tapply(df$AG, df$AwayTeam, sum, na.rm = TRUE)) + 
        tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$AwayAgainst = as.numeric(tapply(df$HG, df$AwayTeam, sum, na.rm = TRUE)) + 
        tmpTable$AwayOTL
    
    
    tmpTable$GF = ifelse(is.na(tmpTable$HomeFor), 0, tmpTable$HomeFor) + ifelse(is.na(tmpTable$AwayFor), 
        0, tmpTable$AwayFor)
    tmpTable$GA = ifelse(is.na(tmpTable$HomeAgainst), 0, tmpTable$HomeAgainst) + 
        ifelse(is.na(tmpTable$AwayAgainst), 0, tmpTable$AwayAgainst)
    
    tmpTable$DIFF = tmpTable$GF - tmpTable$GA
    
    # Additional Stats
    tmpTable$P = 2 * tmpTable$W + tmpTable$OTL
    tmpTable$PP = tmpTable$P/tmpTable$GP
    tmpTable$OT.Win.Percent = (tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + 
        tmpTable$AwaySOW)/(tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + 
        tmpTable$AwayOTL + tmpTable$OTL)
    tmpTable <- tmpTable[, c("Team", "GP", "W", "OTL", "L", "ROW", "P", "GF", 
        "GA", "DIFF", "PP", "OT.Win.Percent")]
    tmpTable <- tmpTable[order(-tmpTable$P, -tmpTable$PP, -tmpTable$ROW, -tmpTable$DIFF), 
        ]
    
    rownames(tmpTable) <- 1:nrow(tmpTable)
    
    return(tmpTable)
}
{% endhighlight %}
 
We use it by calling `makeStatsTable` with the input being the season data.

|         Team          |  GP  |  W  |  OTL  |  L  |  ROW  |  P  |  GF  |  GA  |  OT.Win.Percent  |
|:---------------------:|:----:|:---:|:-----:|:---:|:-----:|:---:|:----:|:----:|:----------------:|
|   New York Rangers    |  82  | 53  |   7   | 22  |  49   | 113 | 252  | 192  |      0.5882      |
|  Montreal Canadiens   |  82  | 50  |  10   | 22  |  43   | 110 | 221  | 189  |      0.619       |
|     Anaheim Ducks     |  82  | 51  |   7   | 24  |  43   | 109 | 236  | 226  |      0.6667      |
|    St. Louis Blues    |  82  | 51  |   7   | 24  |  42   | 109 | 248  | 201  |      0.6667      |
|  Tampa Bay Lightning  |  82  | 50  |   8   | 24  |  47   | 108 | 262  | 211  |      0.3333      |
|  Nashville Predators  |  82  | 47  |  10   | 25  |  41   | 104 | 232  | 208  |       0.5        |
|  Chicago Blackhawks   |  82  | 48  |   6   | 28  |  39   | 102 | 229  | 189  |       0.8        |
|   Vancouver Canucks   |  82  | 48  |   5   | 29  |  42   | 101 | 242  | 222  |      0.7059      |
|  Washington Capitals  |  82  | 45  |  11   | 26  |  40   | 101 | 242  | 203  |       0.4        |
|  New York Islanders   |  82  | 47  |   7   | 28  |  40   | 101 | 252  | 230  |      0.619       |
|    Minnesota Wild     |  82  | 46  |   8   | 28  |  42   | 100 | 231  | 201  |       0.5        |
|   Detroit Red Wings   |  82  | 43  |  14   | 25  |  39   | 100 | 235  | 221  |      0.4074      |
|    Ottawa Senators    |  82  | 43  |  13   | 26  |  37   | 99  | 238  | 215  |      0.4333      |
|     Winnipeg Jets     |  82  | 43  |  13   | 26  |  36   | 99  | 230  | 210  |      0.3929      |
|  Pittsburgh Penguins  |  82  | 43  |  12   | 27  |  39   | 98  | 221  | 210  |      0.3571      |
|    Calgary Flames     |  82  | 45  |   7   | 30  |  41   | 97  | 241  | 216  |       0.65       |
|     Boston Bruins     |  82  | 41  |  14   | 27  |  37   | 96  | 213  | 211  |      0.4062      |
|   Los Angeles Kings   |  82  | 40  |  15   | 27  |  38   | 95  | 220  | 205  |      0.1154      |
|     Dallas Stars      |  82  | 41  |  10   | 31  |  37   | 92  | 261  | 260  |       0.5        |
|   Florida Panthers    |  82  | 38  |  15   | 29  |  30   | 91  | 206  | 223  |      0.3214      |
|  Colorado Avalanche   |  82  | 39  |  12   | 31  |  29   | 90  | 219  | 227  |      0.4138      |
|    San Jose Sharks    |  82  | 40  |   9   | 33  |  36   | 89  | 228  | 232  |      0.375       |
| Columbus Blue Jackets |  82  | 42  |   5   | 35  |  33   | 89  | 236  | 250  |      0.8235      |
|  Philadelphia Flyers  |  82  | 33  |  18   | 31  |  30   | 84  | 215  | 234  |      0.2162      |
|   New Jersey Devils   |  82  | 32  |  14   | 36  |  27   | 78  | 181  | 216  |      0.2308      |
|  Carolina Hurricanes  |  82  | 30  |  11   | 41  |  25   | 71  | 188  | 226  |      0.3158      |
|  Toronto Maple Leafs  |  82  | 30  |   8   | 44  |  25   | 68  | 211  | 262  |       0.4        |
|    Edmonton Oilers    |  82  | 24  |  14   | 44  |  19   | 62  | 198  | 283  |      0.2414      |
|    Arizona Coyotes    |  82  | 24  |   8   | 50  |  19   | 56  | 170  | 272  |      0.5556      |
|    Buffalo Sabres     |  82  | 23  |   8   | 51  |  15   | 54  | 161  | 274  |      0.5625      |

Table: Table of statistics in 2015-2016 season to today.
 
We'll feed it into a log5 function, and use that instead of a factor of 0.5 to determine OT winners. As well, it's trivial to determine that on average, the number of games ending in shootout is slightly higher than the number ending in overtime.
 

{% highlight r %}
log5.OT.predictor <- function(stats, home, away) {
    # reurns chances that home team wins in OT
    pa <- stats[stats$Team == home, ]$OT.Win.Percent
    pb <- stats[stats$Team == away, ]$OT.Win.Percent
    
    log5 <- (pa - (pa * pb))/(pa + pb - (2 * pa * pb))
    
    return(log5)
}

predict_one_game <- function(pmatrix, stats, home, away) {
    random <- runif(1)
    while (random > pmatrix[nrow(pmatrix), ncol(pmatrix)]) {
        random <- runif(1)
    }
    score <- as.vector(which(pmatrix > random, arr.ind = T)[1, ])
    # scores is matrix c(home away)
    score <- score - 1
    score[3] <- NA
    if (score[1] == score[2]) {
        if (runif(1) < log5.OT.predictor(stats, home, away)) {
            # Home Win
            score[1] <- score[1] + 1
            
        } else {
            score[2] <- score[2] + 1
        }
        if (runif(1) < 0.56) {
            score[3] <- "SO"
        } else {
            score[3] <- "OT"
        }
    }
    return(score)
}
{% endhighlight %}
 
Finally, one more round of predictions:

{% highlight r %}
t(replicate(10, predictOneGame(pmatrix, home, away)))
{% endhighlight %}



{% highlight text %}
##       [,1] [,2] [,3]
##  [1,] "2"  "0"  NA  
##  [2,] "2"  "0"  NA  
##  [3,] "3"  "0"  NA  
##  [4,] "2"  "0"  NA  
##  [5,] "1"  "0"  NA  
##  [6,] "2"  "1"  NA  
##  [7,] "2"  "1"  "SO"
##  [8,] "1"  "2"  "OT"
##  [9,] "2"  "1"  "OT"
## [10,] "2"  "0"  NA
{% endhighlight %}
 
Next time we'll look at predicting a whole sesason.
