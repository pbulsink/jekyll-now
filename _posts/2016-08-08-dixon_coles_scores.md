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


{% highlight text %}
## Error in optim(par = par.inits, fn = DCoptimFn, DCm = dcm, xi = xi, method = "BFGS"): non-finite finite-difference value [2]
{% endhighlight %}
 
*Note: This is earlier work I did (last winter/spring) so some info may seem dated at time of posting. I've used data files current to then.*
 
In [section 1]({{ site.baseurl }}/blog/2016-08-04/data_preparation.html), we prepared historical hockey data for analysis. In [section 2]({{ site.baseurl }}/blog/2016-08-05/dixon_coles_and_hockey_data.html), we set up some functions do prepare the Dixon-Coles parameters. Now, we can use them to predict a game. 
 
<!--more-->
 
Poisson analysis is good at predicting, based on an average, the proportion of each integer events happening. So, if we expect 3.1 goals to be scored by a certain team as an average, we know that the proportion of that is going to be (in R code) `dpois(3,3.1)`, equalling 0.2236768. Similarly, the chance that 1 and 2 goals is scored by that team to be `dpois(c(1,2), 3.1)` to be 0.1396525 and 0.2164614 respectively. 
 
The Dixon-Coles optimization gave us 'attack' and 'defence' parameters, along with a home field advantage factor `home` and a fudge fator for low-scoring games, 'rho' ($\rho$). For two teams, Home and Away, we can calculate their expected goals socred by adding their attack, plus the opposing teams' defence, plus (for home) the home field advantage. This will give us a 'lambda' ($\lambda$) and 'mu' ($\mu$) which are the home and away goals expected. For example, let's use Toronto Maple Leafs vs. Montreal Canadiens:
 

{% highlight r %}
home <- "Montreal Canadiens"
away <- "Toronto Maple Leafs"
{% endhighlight %}
 
Montreal is a stronger team this year, we expect a higher attack and lower defence factor. For the 2015 season only, they are, in fact, 0.9780729 and -0.2676516. Similarly, Toronto's factors are 0.9656608 and 0.0833694 respectively. 
 
We'll do the addition to get $\lambda$ and $\mu$:

{% highlight r %}
# Expected goals home
lambda <- exp(res_2015$par["HOME"] + res_2015$par["Attack.Montreal Canadiens"] + 
    res_2015$par["Defence.Toronto Maple Leafs"])
# Expected goals away
mu <- exp(res_2015$par["Attack.Toronto Maple Leafs"] + res_2015$par["Defence.Montreal Canadiens"])
{% endhighlight %}
 
Thus, we expect Montreal to score 3.1555739 goals and Toronto to score 2.0097478 goals. 
 
Using this knowledge, we can create a probability matrix of Home and Away goals' Poisson probability. Using $\lambda$ and $\mu$, `probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))`, and a maximum number of goals per team of 8:
 

|   |        0|        1|        2|        3|        4|        5|        6|        7|        8|
|:--|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|
|0  | 0.005711| 0.011478| 0.011534| 0.007727| 0.003882| 0.001560| 0.000523| 0.000150| 0.000038|
|1  | 0.018022| 0.036220| 0.036397| 0.024383| 0.012251| 0.004924| 0.001649| 0.000474| 0.000119|
|2  | 0.028435| 0.057148| 0.057426| 0.038471| 0.019329| 0.007769| 0.002602| 0.000747| 0.000188|
|3  | 0.029910| 0.060111| 0.060404| 0.040466| 0.020331| 0.008172| 0.002737| 0.000786| 0.000197|
|4  | 0.023596| 0.047421| 0.047652| 0.031923| 0.016039| 0.006447| 0.002159| 0.000620| 0.000156|
|5  | 0.014892| 0.029928| 0.030074| 0.020147| 0.010123| 0.004069| 0.001363| 0.000391| 0.000098|
|6  | 0.007832| 0.015740| 0.015817| 0.010596| 0.005324| 0.002140| 0.000717| 0.000206| 0.000052|
|7  | 0.003531| 0.007096| 0.007130| 0.004777| 0.002400| 0.000965| 0.000323| 0.000093| 0.000023|
|8  | 0.001393| 0.002799| 0.002812| 0.001884| 0.000947| 0.000381| 0.000127| 0.000037| 0.000009|
 
This has the away score on the top (as columns), and the home score along the side (as rows). Recall that we need to apply the $\tau$ function to this matrix, to account for low scoring games. Once that is done, we can sum the diagonal of the matrix to find the probability of a tie. The sum of the upper triangle is the probability of an away win, and the sum of the lower triangle is the home win.
 
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
{% endhighlight %}



{% highlight text %}
## Error in predictResult(res_all, home, away): object 'res_all' not found
{% endhighlight %}



{% highlight r %}
probs_2015_time <- predictResult(time_res_2015, home, away)
probs_all_time <- predictResult(time_res_all, home, away)
{% endhighlight %}
 
Now we can look at one of the results (say, 2015 not time weighted) and see the probability of a home win (Montreal), draw, or away win (Toronto): 0.5974644, 0.1734899, 0.2235644. In fact, we can plot the probabilities for the four fits we have, to show the effect of each fit type.
 

{% highlight text %}
## Error in rbind(probs_2015, probs_2015_time, probs_all, probs_all_time): object 'probs_all' not found
{% endhighlight %}



{% highlight text %}
## Error in colnames(probs_df) <- c("Montreal Win", "Draw", "Toronto Win"): object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in probs_df$Sim.Type <- c("2015 Season", "2015 Season Time Weighted", : object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in melt(probs_df, id.vars = "Sim.Type"): object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in ggplot(pmelt, aes(y = value, x = variable, fill = Sim.Type)): object 'pmelt' not found
{% endhighlight %}
 
While the odds of a draw haven't changed much, the odds of a Montreal or Toronto win have slightly. Note that over the past 10 years, Montreal has performed better, on average, than Toronto, so this is expected. As well, Monteal benefits from the home ice advantage. We can calculate this for a Toronto home game too.
 

{% highlight text %}
## Error in predictResult(res_all, away, home): object 'res_all' not found
{% endhighlight %}



{% highlight text %}
## Error in colnames(probs_df) <- c("Toronto Win", "Draw", "Montreal Win"): object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in probs_df$Sim.Type <- c("2015 Season", "2015 Season Time Weighted", : object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in melt(probs_df, id.vars = "Sim.Type"): object 'probs_df' not found
{% endhighlight %}



{% highlight text %}
## Error in ggplot(pmelt, aes(y = value, x = variable, fill = Sim.Type)): object 'pmelt' not found
{% endhighlight %}
 
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
## [1] 4 1
{% endhighlight %}
 
Lets' do that a few times, to see the different results we get.

{% highlight r %}
t(replicate(10, predictOneGame(pmatrix, home, away)))
{% endhighlight %}



{% highlight text %}
##       [,1] [,2]
##  [1,]    6    1
##  [2,]    5    2
##  [3,]    5    4
##  [4,]    3    1
##  [5,]    4    4
##  [6,]    2    5
##  [7,]    8    2
##  [8,]    6    2
##  [9,]    4    0
## [10,]    3    1
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
##  [1,] "2"  "1"  NA  
##  [2,] "2"  "3"  NA  
##  [3,] "7"  "1"  NA  
##  [4,] "4"  "3"  NA  
##  [5,] "3"  "2"  NA  
##  [6,] "4"  "3"  "SO"
##  [7,] "4"  "3"  NA  
##  [8,] "7"  "4"  NA  
##  [9,] "3"  "1"  NA  
## [10,] "7"  "2"  NA
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
    
    tmpTable$AwayOTW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == 
        "OT")]))
    tmpTable$AwaySOW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == 
        "SO")]))
    
    # OT Losses
    tmpTable$HomeOTL = as.numeric(table(df$HomeTeam[(df$OT.Win == "V")]))
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

|Team                  | GP|  W| OTL|  L| ROW|   P|  GF|  GA| DIFF|        PP| OT.Win.Percent|
|:---------------------|--:|--:|---:|--:|---:|---:|---:|---:|----:|---------:|--------------:|
|Tampa Bay Lightning   | 82| 47|   7| 24|  45| 101| 259| 210|   49| 1.2317073|      0.2222222|
|New York Rangers      | 82| 49|   2| 22|  47| 100| 248| 187|   61| 1.2195122|      0.6000000|
|Anaheim Ducks         | 82| 45|   4| 24|  40|  94| 230| 223|    7| 1.1463415|      0.5555556|
|Montreal Canadiens    | 82| 44|   4| 22|  43|  92| 215| 183|   32| 1.1219512|      0.4666667|
|Vancouver Canucks     | 82| 44|   3| 29|  41|  91| 238| 220|   18| 1.1097561|      0.5714286|
|St. Louis Blues       | 82| 43|   5| 24|  39|  91| 240| 199|   41| 1.1097561|      0.3750000|
|Nashville Predators   | 82| 42|   6| 25|  38|  90| 227| 204|   23| 1.0975610|      0.4285714|
|Los Angeles Kings     | 82| 40|   8| 27|  38|  88| 220| 198|   22| 1.0731707|      0.1578947|
|Chicago Blackhawks    | 82| 43|   1| 28|  38|  87| 224| 184|   40| 1.0609756|      0.7777778|
|Washington Capitals   | 82| 40|   6| 26|  37|  86| 237| 198|   39| 1.0487805|      0.2941176|
|New York Islanders    | 82| 40|   5| 28|  37|  85| 245| 228|   17| 1.0365854|      0.3750000|
|Minnesota Wild        | 82| 41|   2| 28|  39|  84| 226| 195|   31| 1.0243902|      0.4285714|
|Winnipeg Jets         | 82| 38|   8| 26|  35|  84| 225| 205|   20| 1.0243902|      0.2727273|
|Pittsburgh Penguins   | 82| 37|   8| 27|  35|  82| 215| 206|    9| 1.0000000|      0.2000000|
|Ottawa Senators       | 82| 36|   8| 26|  34|  80| 231| 210|   21| 0.9756098|      0.2727273|
|Detroit Red Wings     | 82| 37|   5| 25|  36|  79| 229| 212|   17| 0.9634146|      0.3333333|
|Colorado Avalanche    | 82| 34|   9| 31|  28|  77| 214| 224|  -10| 0.9390244|      0.2800000|
|Calgary Flames        | 82| 37|   2| 30|  35|  76| 233| 211|   22| 0.9268293|      0.5555556|
|San Jose Sharks       | 82| 36|   4| 33|  35|  76| 224| 227|   -3| 0.9268293|      0.2000000|
|Boston Bruins         | 82| 34|   7| 27|  32|  75| 206| 204|    2| 0.9146341|      0.3000000|
|Florida Panthers      | 82| 33|   8| 29|  29|  74| 201| 216|  -15| 0.9024390|      0.2000000|
|Columbus Blue Jackets | 82| 35|   3| 35|  31|  73| 229| 248|  -19| 0.8902439|      0.5384615|
|Philadelphia Flyers   | 82| 31|  11| 31|  28|  73| 213| 227|  -14| 0.8902439|      0.2142857|
|Dallas Stars          | 82| 34|   2| 31|  34|  70| 254| 252|    2| 0.8536585|      0.2000000|
|New Jersey Devils     | 82| 31|   6| 36|  26|  68| 180| 208|  -28| 0.8292683|      0.2941176|
|Toronto Maple Leafs   | 82| 28|   6| 44|  25|  62| 209| 260|  -51| 0.7560976|      0.3333333|
|Carolina Hurricanes   | 82| 28|   4| 41|  25|  60| 186| 219|  -33| 0.7317073|      0.3333333|
|Edmonton Oilers       | 82| 20|  11| 44|  18|  51| 194| 280|  -86| 0.6219512|      0.1200000|
|Arizona Coyotes       | 82| 19|   3| 50|  17|  41| 165| 267| -102| 0.5000000|      0.4545455|
|Buffalo Sabres        | 82| 19|   3| 51|  15|  41| 157| 269| -112| 0.5000000|      0.4545455|
 
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
##  [1,]    7    0   NA
##  [2,]    4    1   NA
##  [3,]    7    1   NA
##  [4,]    2    0   NA
##  [5,]    7    1   NA
##  [6,]    3    4   NA
##  [7,]    5    1   NA
##  [8,]    3    2   NA
##  [9,]    5    2   NA
## [10,]    3    2   NA
{% endhighlight %}
 
Next time we'll look at predicting a whole sesason.
