---
title: "Simulating a Season"
author: "Philip Bulsink"
date: "August 09, 2016"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey predicting Dixon-Coles
---
 


 
*Note: This is earlier work I did (last winter/spring) so some info may seem dated at time of posting. I've used data files current to then.*
 
[Last time]({{ site.baseurl }}/blog/2016-08-08/dixon_coles_scores.html) we predicted the score of a game using the time dependant Dixon-Coles method. 
 
The interesting application of this is predicting how teams will do between now and the end of the season. One would really like to know if their team would make the playoffs or not. While no model can predict the impact of major trades, coaching changes, or other unforseen factors, they can do a very reasonable job at predicting what will happen if teams continue playing as they have been.
 
<!--more-->
 
To do these simulations, we'll simulate each game of the remaining season multiple times. We need to plug into the model each game, and track the results. 
 
We'll start with a helper function to quickly make the probability matrix and give a score back. The build score matrix function is a compilation of what was done last post.

{% highlight r %}
buildScoreMatrix <- function(res, home, away, maxgoal = 8) {
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
    
    pmatrix <- matrix(nrow = nrow(probability_matrix), ncol = ncol(probability_matrix))
    # sum of probabilities matrix
    current_p <- 0
    for (i in 1:ncol(pmatrix)) {
        for (j in 1:nrow(pmatrix)) {
            pmatrix[j, i] <- current_p
            current_p <- current_p + probability_matrix[j, i]
        }
    }
    return(pmatrix)
}

predictScore <- function(res, stats, home, away, maxgoal = 8) {
    return(predictOneGame(buildScoreMatrix(res, home, away, maxgoal = maxgoal), 
        stats = stats, home, away))
}
{% endhighlight %}
 
Before we go predicting games, we need to have a list of things to predict. Taking the most recent season (2015-2016), we'll strip out all the games after today:

{% highlight r %}
nhlFutureGames <- function(df) {
    df <- df[, c("Date", "Visitor", "G", "Home", "G.1")]
    df$Date <- as.Date(df$Date)
    df <- df[!(complete.cases(df)), ]
    df$OT.SO <- ""
    df$OT.Win <- ""
    colnames(df) <- c("Date", "AwayTeam", "AG", "HomeTeam", "HG", "OT.SO", "OT.Win")
    return(df)
}

future_games <- nhlFutureGames(read.csv("./_data/20152016.csv"))
{% endhighlight %}
 
Now, for each game, simulate a result. But, we need some place to put the results... lets add them to the results table we made last time! As well, simulating a season once doesn't give us a lot of information. We may as well make a best guess where teams will finish. What we really want to do is simulate the season lots of times, to get a decent probability of where a team will finish in the standings. This will take two functions. One to finish the season, taking the schedule, stats (for overtime win percent, etc), and parameters that we generated in the Dixon-Coles process. The other will take that data, plus the season standings thus far, and give us a likely probability of where the teams will finish as a standings table.
 

{% highlight r %}
nhl2016 <- nhlDataPrep(read.csv("./_data/20152016.csv")[2:7])
nhl_2016_stats <- makeStatsTable(nhl2016)

buildStandingsTable <- function(stats, standings = NA) {
    if (is.na(standings)) {
        standings <- matrix(0, nrow = length(unique(stats$Team)), ncol = length(unique(stats$Team)))
        rownames(standings) <- sort(unique(stats$Team))
        colnames(standings) <- c(1:ncol(standings))
    }
    
    for (t in 1:nrow(standings)) {
        standings[stats[t, "Team"], t] <- standings[stats[t, "Team"], t] + 1
    }
    return(standings)
}

nhl_2016_standings <- buildStandingsTable(nhl_2016_stats)

predictRemainderOfSeason <- function(res, schedule, stats, maxgoal = 8) {
    # Takes in a schedule of games and returns the schedule with scores filled
    # out.
    for (game in 1:nrow(schedule)) {
        home <- as.character(schedule[game, "HomeTeam"])
        away <- as.character(schedule[game, "AwayTeam"])
        score <- predictScore(res, stats, home, away, maxgoal = maxgoal)
        if (!is.na(score[3])) {
            schedule[game, "OT.SO"] <- score[3]
            if (score[1] > score[2]) {
                schedule[game, "HG"] <- score[2]
                schedule[game, "AG"] <- score[2]
                schedule[game, "OT.Win"] <- "H"
            } else {
                schedule[game, "HG"] <- score[1]
                schedule[game, "AG"] <- score[1]
                schedule[game, "OT.Win"] <- "A"
            }
        } else {
            schedule[game, "HG"] <- score[1]
            schedule[game, "AG"] <- score[2]
        }
    }
    schedule$HG <- as.integer(schedule$HG)
    schedule$AG <- as.integer(schedule$AG)
    return(schedule)
}

simulateSeason <- function(res, schedule, stats, past_results, n = 10000, maxgoal = 8) {
    # simulates the remainder of the season n times, returning a standings table
    # with the times each team finished at each position
    standings <- matrix(0, nrow = length(unique(stats$Team)), ncol = length(unique(stats$Team)))
    rownames(standings) <- sort(unique(stats$Team))
    colnames(standings) <- c(1:ncol(standings))
    for (i in 1:n) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, 
            maxgoal = maxgoal)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        standings <- buildStandingsTable(stats = stats_table, standings = standings)
    }
    return(standings)
}
{% endhighlight %}
  
For the record, here's where teams are now (March 20, 2016):


{:.lgtable}
|         Team          |  GP  |  W  |  OTL  |  L  |  ROW  |  P  |  GF  |  GA  |  DIFF  |
|:---------------------:|:----:|:---:|:-----:|:---:|:-----:|:---:|:----:|:----:|:------:|
|  Washington Capitals  |  70  | 51  |   5   | 14  |  47   | 107 | 225  | 162  |   63   |
|     Dallas Stars      |  73  | 43  |   9   | 21  |  41   | 95  | 236  | 211  |   25   |
|   Los Angeles Kings   |  71  | 44  |   5   | 22  |  42   | 93  | 196  | 160  |   36   |
|    St. Louis Blues    |  73  | 42  |   9   | 22  |  37   | 93  | 194  | 185  |   9    |
|  Chicago Blackhawks   |  72  | 42  |   6   | 24  |  41   | 90  | 201  | 176  |   25   |
|   Florida Panthers    |  72  | 40  |   9   | 23  |  33   | 89  | 205  | 177  |   28   |
|   New York Rangers    |  72  | 40  |   8   | 24  |  37   | 88  | 203  | 190  |   13   |
|     Anaheim Ducks     |  70  | 39  |   9   | 22  |  36   | 87  | 179  | 162  |   17   |
|  Tampa Bay Lightning  |  72  | 41  |   5   | 26  |  38   | 87  | 196  | 171  |   25   |
|    San Jose Sharks    |  71  | 40  |   6   | 25  |  37   | 86  | 211  | 185  |   26   |
|  Pittsburgh Penguins  |  71  | 39  |   8   | 24  |  36   | 86  | 198  | 177  |   21   |
|     Boston Bruins     |  73  | 39  |   8   | 26  |  35   | 86  | 216  | 197  |   19   |
|  New York Islanders   |  70  | 38  |   9   | 23  |  34   | 85  | 196  | 177  |   19   |
|  Nashville Predators  |  72  | 36  |  13   | 23  |  33   | 85  | 197  | 185  |   12   |
|   Detroit Red Wings   |  72  | 36  |  11   | 25  |  34   | 83  | 184  | 190  |   -6   |
|  Philadelphia Flyers  |  70  | 34  |  12   | 24  |  32   | 80  | 182  | 189  |   -7   |
|  Colorado Avalanche   |  72  | 37  |   4   | 31  |  33   | 78  | 195  | 202  |   -7   |
|    Minnesota Wild     |  72  | 33  |  11   | 28  |  31   | 77  | 191  | 184  |   7    |
|  Carolina Hurricanes  |  72  | 31  |  14   | 27  |  30   | 76  | 175  | 195  |  -20   |
|    Ottawa Senators    |  73  | 34  |   8   | 31  |  28   | 76  | 211  | 223  |  -12   |
|   New Jersey Devils   |  72  | 34  |   7   | 31  |  32   | 75  | 164  | 188  |  -24   |
|  Montreal Canadiens   |  72  | 33  |   6   | 33  |  28   | 72  | 191  | 205  |  -14   |
|    Arizona Coyotes    |  71  | 31  |   7   | 33  |  30   | 69  | 188  | 214  |  -26   |
|    Buffalo Sabres     |  73  | 29  |  10   | 34  |  27   | 68  | 173  | 198  |  -25   |
|    Calgary Flames     |  71  | 30  |   6   | 35  |  28   | 66  | 196  | 222  |  -26   |
| Columbus Blue Jackets |  71  | 29  |   8   | 34  |  24   | 66  | 187  | 221  |  -34   |
|   Vancouver Canucks   |  71  | 27  |  12   | 32  |  23   | 66  | 167  | 205  |  -38   |
|    Edmonton Oilers    |  74  | 29  |   7   | 38  |  25   | 65  | 179  | 216  |  -37   |
|     Winnipeg Jets     |  71  | 29  |   5   | 37  |  27   | 63  | 182  | 213  |  -31   |
|  Toronto Maple Leafs  |  71  | 25  |  11   | 35  |  19   | 61  | 169  | 207  |  -38   |

Table: Current team standings
 
We can predict the season's remaining games, to figure out what chances our favourite team has of finishing in any position! We need to get the DC parametsrs, as per usual. We'll start with the nhl2016 data only, with no time weighting.

{% highlight r %}
res_2016 <- doDCPrediction(nhl2016)
{% endhighlight %}
 
If we run a preiction, we can get a nice stats table, as we'd find on the NHL website. Note that some games were cancelled for a snowstorm in January and may not have been rescheduled yet, so their teams may have less games played.
 

{% highlight r %}
remainder_2016 <- predictRemainderOfSeason(res_2016, future_games, nhl_2016_stats)
all_results <- rbind(nhl2016, remainder_2016)
stats_sim_2016 <- makeStatsTable(all_results)
pandoc.table(stats_sim_2016[, 1:10], style = "rmarkdown", caption = "Predicted standings at end of season")
{% endhighlight %}

{:.lgtable}
|         Team          |  GP  |  W  |  OTL  |  L  |  ROW  |  P  |  GF  |  GA  |  DIFF  |
|:---------------------:|:----:|:---:|:-----:|:---:|:-----:|:---:|:----:|:----:|:------:|
|  Washington Capitals  |  82  | 56  |   6   | 20  |  52   | 118 | 233  | 171  |   62   |
|   Los Angeles Kings   |  82  | 51  |   5   | 26  |  47   | 107 | 208  | 170  |   38   |
|     Dallas Stars      |  82  | 48  |  10   | 24  |  45   | 106 | 248  | 222  |   26   |
|    San Jose Sharks    |  82  | 48  |   7   | 27  |  44   | 103 | 229  | 195  |   34   |
|   New York Rangers    |  82  | 47  |   9   | 26  |  42   | 103 | 217  | 200  |   17   |
|    St. Louis Blues    |  82  | 47  |   9   | 26  |  42   | 103 | 204  | 192  |   12   |
|  Chicago Blackhawks   |  82  | 48  |   6   | 28  |  47   | 102 | 214  | 185  |   29   |
|  New York Islanders   |  82  | 46  |  10   | 26  |  41   | 102 | 212  | 185  |   27   |
|  Tampa Bay Lightning  |  82  | 48  |   5   | 29  |  43   | 101 | 209  | 179  |   30   |
|  Nashville Predators  |  82  | 43  |  14   | 25  |  39   | 100 | 214  | 195  |   19   |
|   Florida Panthers    |  82  | 45  |   9   | 28  |  38   | 99  | 216  | 185  |   31   |
|  Pittsburgh Penguins  |  82  | 44  |   9   | 29  |  40   | 97  | 207  | 192  |   15   |
|     Anaheim Ducks     |  82  | 42  |  12   | 28  |  38   | 96  | 188  | 179  |   9    |
|     Boston Bruins     |  82  | 43  |   8   | 31  |  39   | 94  | 222  | 209  |   13   |
|   Detroit Red Wings   |  82  | 40  |  13   | 29  |  38   | 93  | 196  | 203  |   -7   |
|  Philadelphia Flyers  |  82  | 38  |  14   | 30  |  35   | 90  | 192  | 205  |  -13   |
|  Carolina Hurricanes  |  82  | 37  |  15   | 30  |  35   | 89  | 189  | 205  |  -16   |
|  Colorado Avalanche   |  82  | 42  |   4   | 36  |  38   | 88  | 204  | 215  |  -11   |
|    Minnesota Wild     |  82  | 38  |  12   | 32  |  36   | 88  | 198  | 190  |   8    |
|   New Jersey Devils   |  82  | 39  |   9   | 34  |  37   | 87  | 179  | 200  |  -21   |
|  Montreal Canadiens   |  82  | 40  |   6   | 36  |  33   | 86  | 205  | 214  |   -9   |
|    Arizona Coyotes    |  82  | 38  |   7   | 37  |  36   | 83  | 202  | 227  |  -25   |
|    Ottawa Senators    |  82  | 37  |   8   | 37  |  31   | 82  | 217  | 232  |  -15   |
|    Buffalo Sabres     |  82  | 32  |  13   | 37  |  29   | 77  | 185  | 212  |  -27   |
|   Vancouver Canucks   |  82  | 31  |  15   | 36  |  27   | 77  | 181  | 222  |  -41   |
|    Calgary Flames     |  82  | 35  |   6   | 41  |  33   | 76  | 211  | 238  |  -27   |
| Columbus Blue Jackets |  82  | 32  |  11   | 39  |  27   | 75  | 198  | 240  |  -42   |
|  Toronto Maple Leafs  |  82  | 30  |  14   | 38  |  24   | 74  | 181  | 221  |  -40   |
|    Edmonton Oilers    |  82  | 32  |   9   | 41  |  28   | 73  | 187  | 226  |  -39   |
|     Winnipeg Jets     |  82  | 33  |   6   | 43  |  30   | 72  | 189  | 226  |  -37   |


Table: Predicted standings at end of season
 
But, while doing that once is cool, we need to do that over and over again to really get a picture of the chance each team has of making the playoffs by the end of the season. This we'll do with `n=1000`, to save time.
 

{% highlight r %}
nhl_2016_predicted_standings <- simulateSeason(res_2016, future_games, nhl_2016_stats, 
    past_results = nhl2016, n = 1000)
pandoc.table(nhl_2016_predicted_standings, style = "rmarkdown", caption = "Predicted position at end of season after 1000 simulations")
{% endhighlight %}


{::nomarkdown}<div class="lgtable">{:/}

|           &nbsp;            |  1   |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:---------------------------:|:----:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|      **Anaheim Ducks**      |  0   |  1  |  2  |  5  |  4  |  7  | 23  | 37  | 81  | 116  | 143  | 189  | 202  | 137  |  46  |  6   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Arizona Coyotes**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  24  |  93  | 716  | 118  |  29  |  10  |  5   |  1   |  0   |  0   |
|      **Boston Bruins**      |  0   |  0  |  0  |  1  |  1  |  1  |  1  | 11  | 14  |  24  |  51  |  91  | 156  | 328  | 240  |  67  |  10  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Buffalo Sabres**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  27  | 221  | 241  | 188  | 139  |  99  |  53  |  32  |
|     **Calgary Flames**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  5   |  41  | 146  | 150  | 158  | 145  | 132  | 118  | 105  |
|   **Carolina Hurricanes**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  1   |  7   |  31  | 121  | 202  | 214  | 254  | 122  |  46  |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Chicago Blackhawks**    |  0   | 54  | 94  | 137 | 138 | 128 | 143 | 131 | 75  |  44  |  29  |  21  |  5   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Colorado Avalanche**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  1   |  4   |  17  |  50  | 188  | 284  | 195  | 139  |  87  |  29  |  6   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Columbus Blue Jackets**  |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  42  | 246  | 200  | 149  | 147  | 109  |  63  |  44  |
|      **Dallas Stars**       |  0   | 188 | 294 | 217 | 129 | 80  | 47  | 17  | 21  |  5   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Detroit Red Wings**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  1  |  4  |  13  |  18  |  34  |  86  | 195  | 388  | 194  |  46  |  17  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Edmonton Oilers**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  5   |  33  |  69  | 108  | 143  | 191  | 231  | 220  |
|    **Florida Panthers**     |  0   |  0  |  2  |  6  | 17  | 28  | 45  | 58  | 95  | 140  | 177  | 179  | 154  |  76  |  20  |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Los Angeles Kings**    |  0   | 592 | 204 | 103 | 46  | 30  | 18  |  6  |  1  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Minnesota Wild**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  2   |  15  |  76  | 209  | 253  | 232  | 133  |  67  |  13  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Montreal Canadiens**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  7   |  14  |  45  | 111  | 303  | 431  |  86  |  1   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Nashville Predators**   |  0   |  0  |  0  |  3  |  9  | 17  | 30  | 57  | 87  | 166  | 198  | 169  | 139  |  85  |  33  |  6   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New Jersey Devils**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  5   |  39  |  92  | 193  | 238  | 265  | 115  |  47  |  5   |  0   |  1   |  0   |  0   |  0   |  0   |  0   |
|   **New York Islanders**    |  0   |  9  | 22  | 60  | 96  | 122 | 145 | 158 | 147 | 102  |  70  |  37  |  22  |  7   |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New York Rangers**     |  0   | 19  | 68  | 101 | 150 | 172 | 161 | 135 | 81  |  50  |  33  |  20  |  9   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Ottawa Senators**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  1   |  1   |  5   |  22  |  36  |  77  | 133  | 336  | 358  |  31  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Philadelphia Flyers**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  2  |  3   |  1   |  16  |  26  |  53  | 163  | 374  | 207  |  87  |  50  |  13  |  4   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Pittsburgh Penguins**   |  0   |  0  |  0  | 11  |  7  | 17  | 34  | 63  | 103 | 150  | 165  | 175  | 159  |  84  |  24  |  8   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **San Jose Sharks**     |  0   | 41  | 112 | 120 | 167 | 172 | 156 | 111 | 75  |  22  |  10  |  11  |  1   |  1   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **St. Louis Blues**     |  0   | 93  | 184 | 202 | 184 | 145 | 98  | 52  | 28  |  13  |  0   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Tampa Bay Lightning**   |  0   |  3  | 18  | 34  | 52  | 81  | 99  | 163 | 186 | 152  | 103  |  56  |  37  |  11  |  4   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Toronto Maple Leafs**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  3   |  19  |  63  |  96  | 145  | 152  | 226  | 296  |
|    **Vancouver Canucks**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  42  | 201  | 198  | 169  | 122  | 117  |  87  |  64  |
|   **Washington Capitals**   | 1000 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|      **Winnipeg Jets**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  15  |  49  | 122  | 154  | 199  | 222  | 239  |

{::nomarkdown}</div>{:/}

Table: Predicted position at end of season after 1000 simulations
 
But, all of the results come out together in the standings table, but playoff positions are ranked by conference, and division. Each division's top 3 teams make the playoffs, as well as two wild card teams from each conference. We'll set up the divisions, confereces, and playoffs here.
 

{% highlight r %}
nhl_divisions <- list(Atlantic = c("Boston Bruins", "Buffalo Sabres", "Detroit Red Wings", 
    "Florida Panthers", "Montreal Canadiens", "Ottawa Senators", "Tampa Bay Lightning", 
    "Toronto Maple Leafs"), Central = c("Colorado Avalanche", "Chicago Blackhawks", 
    "Dallas Stars", "Minnesota Wild", "Nashville Predators", "St. Louis Blues", 
    "Winnipeg Jets"), Metropolitan = c("Carolina Hurricanes", "Columbus Blue Jackets", 
    "Philadelphia Flyers", "Pittsburgh Penguins", "New Jersey Devils", "New York Islanders", 
    "New York Rangers", "Washington Capitals"), Pacific = c("Anaheim Ducks", 
    "Arizona Coyotes", "Calgary Flames", "Edmonton Oilers", "Los Angeles Kings", 
    "San Jose Sharks", "Vancouver Canucks"))

nhl_conferences <- list(East = c(unlist(nhl_divisions["Atlantic"]), unlist(nhl_divisions["Metropolitan"])), 
    West = c(unlist(nhl_divisions["Central"]), unlist(nhl_divisions["Pacific"])))


getDivisionStats <- function(stats, division) {
    return(stats[stats$Team %in% unlist(nhl_divisions[division]), ])
}

getConferenceStats <- function(stats, conference) {
    if (conference == "East") {
        a <- getDivisionStats(stats, "Atlantic")
        b <- getDivisionStats(stats, "Metropolitan")
    } else {
        a <- getDivisionStats(stats, "Central")
        b <- getDivisionStats(stats, "Pacific")
    }
    top6 <- rbind(a[1:3], b[1:3])
    top6 <- top6[order(-top6$P, -top6$PP, -top6$ROW, -top6$DIFF), ]
    remainder <- rbind(a[4:nrow(a), ], b[4:nrow(b), ])
    remainder <- remainder[order(-remainder$P, -remainder$PP, -remainder$ROW, 
        -remainder$DIFF)]
    
    return(rbind(top6, remainder))
}

getConferenceStandings <- function(standings, conference) {
    # Change this to sort by MAX of standings
    standings <- as.data.frame(standings)
    standings$Team <- rownames(standings)
    if (conference == "East") {
        a <- getDivisionStats(standings, "Atlantic")
        b <- getDivisionStats(standings, "Metropolitan")
    } else {
        a <- getDivisionStats(standings, "Central")
        b <- getDivisionStats(standings, "Pacific")
    }
    standings <- rbind(a, b)
    standings <- subset(standings, select = -Team)
    standings <- standings[do.call(order, c(as.list(standings), decreasing = TRUE)), 
        ]
    return(as.matrix(standings))
}
{% endhighlight %}
 
All we need to do is feed the standings table from the predictions to the `getConferenceStandings` function, and we'll get a table of the teams for each conference. The top 8 teams * make the playoffs.
 
From our calculations, we can see that the Eastern Conference standings are likely as follows:
 
{:.lgtable}
|           &nbsp;            |  1   |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:---------------------------:|:----:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|   **Washington Capitals**   | 1000 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New York Rangers**     |  0   | 19  | 68  | 101 | 150 | 172 | 161 | 135 | 81  |  50  |  33  |  20  |  9   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **New York Islanders**    |  0   |  9  | 22  | 60  | 96  | 122 | 145 | 158 | 147 | 102  |  70  |  37  |  22  |  7   |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Tampa Bay Lightning**   |  0   |  3  | 18  | 34  | 52  | 81  | 99  | 163 | 186 | 152  | 103  |  56  |  37  |  11  |  4   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Florida Panthers**     |  0   |  0  |  2  |  6  | 17  | 28  | 45  | 58  | 95  | 140  | 177  | 179  | 154  |  76  |  20  |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Pittsburgh Penguins**   |  0   |  0  |  0  | 11  |  7  | 17  | 34  | 63  | 103 | 150  | 165  | 175  | 159  |  84  |  24  |  8   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|      **Boston Bruins**      |  0   |  0  |  0  |  1  |  1  |  1  |  1  | 11  | 14  |  24  |  51  |  91  | 156  | 328  | 240  |  67  |  10  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Detroit Red Wings**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  1  |  4  |  13  |  18  |  34  |  86  | 195  | 388  | 194  |  46  |  17  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Philadelphia Flyers**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  2  |  3   |  1   |  16  |  26  |  53  | 163  | 374  | 207  |  87  |  50  |  13  |  4   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Carolina Hurricanes**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  1   |  7   |  31  | 121  | 202  | 214  | 254  | 122  |  46  |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Ottawa Senators**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  1   |  1   |  5   |  22  |  36  |  77  | 133  | 336  | 358  |  31  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New Jersey Devils**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  5   |  39  |  92  | 193  | 238  | 265  | 115  |  47  |  5   |  0   |  1   |  0   |  0   |  0   |  0   |  0   |
|   **Montreal Canadiens**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  7   |  14  |  45  | 111  | 303  | 431  |  86  |  1   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Columbus Blue Jackets**  |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  42  | 246  | 200  | 149  | 147  | 109  |  63  |  44  |
|     **Buffalo Sabres**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  27  | 221  | 241  | 188  | 139  |  99  |  53  |  32  |
|   **Toronto Maple Leafs**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  3   |  19  |  63  |  96  | 145  | 152  | 226  | 296  |


Table: Predicted Eastern Conference standings
 
So, the Detroit Red Wings likely squeak into the playoffs, while Philadelphia Flyers just miss them. But, of course, the odds are good that it could be the other way around too! Nothing is set in stone until the final whistle blows.
 
Similarly, the West standings would look like this:
 
{:.lgtable}
|          &nbsp;           |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:-------------------------:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|   **Los Angeles Kings**   |  0  | 592 | 204 | 103 | 46  | 30  | 18  |  6  |  1  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Dallas Stars**      |  0  | 188 | 294 | 217 | 129 | 80  | 47  | 17  | 21  |  5   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **St. Louis Blues**    |  0  | 93  | 184 | 202 | 184 | 145 | 98  | 52  | 28  |  13  |  0   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Chicago Blackhawks**   |  0  | 54  | 94  | 137 | 138 | 128 | 143 | 131 | 75  |  44  |  29  |  21  |  5   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **San Jose Sharks**    |  0  | 41  | 112 | 120 | 167 | 172 | 156 | 111 | 75  |  22  |  10  |  11  |  1   |  1   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Anaheim Ducks**     |  0  |  1  |  2  |  5  |  4  |  7  | 23  | 37  | 81  | 116  | 143  | 189  | 202  | 137  |  46  |  6   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Nashville Predators**  |  0  |  0  |  0  |  3  |  9  | 17  | 30  | 57  | 87  | 166  | 198  | 169  | 139  |  85  |  33  |  6   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Colorado Avalanche**   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  1   |  4   |  17  |  50  | 188  | 284  | 195  | 139  |  87  |  29  |  6   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Minnesota Wild**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  2   |  15  |  76  | 209  | 253  | 232  | 133  |  67  |  13  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Arizona Coyotes**    |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  24  |  93  | 716  | 118  |  29  |  10  |  5   |  1   |  0   |  0   |
|    **Calgary Flames**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  5   |  41  | 146  | 150  | 158  | 145  | 132  | 118  | 105  |
|   **Vancouver Canucks**   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  42  | 201  | 198  | 169  | 122  | 117  |  87  |  64  |
|    **Edmonton Oilers**    |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  5   |  33  |  69  | 108  | 143  | 191  | 231  | 220  |
|     **Winnipeg Jets**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  15  |  49  | 122  | 154  | 199  | 222  | 239  |


Table: Predicted Western Conference standings
 
It'd look cleaner (and be better for sorting) to average the points each team recieves (and keep the variance around for good measure). 
 

{% highlight r %}
new_stdev <- function(stdev, mean, pop, new_val) {
    n_m = new_mean(mean, pop, new_val)
    return(sqrt((((pop - 2) * stdev^2) + (new_val - n_m) * (new_val - mean))/(pop - 
        1)))
}

v_new_stdev <- Vectorize(new_stdev, c("stdev", "mean", "new_val"))

new_mean <- function(mean, pop, new_val) {
    return((mean * pop + new_val)/(pop + 1))
}

v_new_mean <- Vectorize(new_mean, c("mean", "new_val"))

point_predict <- function(res, schedule, stats, past_results, n = 10000, maxgoal = 8) {
    pp <- matrix(0, nrow = length(unique(stats$Team)), ncol = 6)
    rownames(pp) <- sort(unique(stats$Team))
    colnames(pp) <- c("Points", "Points_StDev", "Playoffs", "Playoffs_StDev", 
        "Presidents", "Presidents_StDev")
    
    scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, 
        maxgoal = maxgoal)
    stats_table <- makeStatsTable(rbind(past_results, scores))
    standings <- buildStandingsTable(stats_table)
    pp[, "Points"] <- stats_table[order(stats_table$Team), ]$P
    playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, 
        ]), rownames(getConferenceStandings(standings, "West")[1:8, ]))
    pp[playoff_list, "Playoffs"] <- 1
    pp[names(which(standings[, "1"] == 1, arr.ind = TRUE)), "Presidents"] <- 1
    
    if (n == 1) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, 
            maxgoal = maxgoal)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        standings <- buildStandingsTable(stats_table)
        playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, 
            ]), rownames(getConferenceStandings(standings, "West")[1:8, ]))
        
        pp[, "Points_StDev"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), 
            ]$P), 1, sd)
        pp[, "Points"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), 
            ]$P), 1, mean)
        pp[, "Playoffs_StDev"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% 
            playoff_list), 1, sd)
        pp[, "Playoffs"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% 
            playoff_list), 1, mean)
        pp[, "Presidents_StDev"] <- apply(cbind(pp[, "Presidents"], standings[, 
            "1"]), 1, sd)
        pp[, "Presidents"] <- apply(cbind(pp[, "Presidents"], standings[, "1"]), 
            1, mean)
    } else if (n > 2) {
        scores <- predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, 
            maxgoal = maxgoal)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        standings <- buildStandingsTable(stats_table)
        playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, 
            ]), rownames(getConferenceStandings(standings, "West")[1:8, ]))
        
        scores2 <- predictRemainderOfSeason(res = res, schedule = schedule, 
            stats = stats, maxgoal = maxgoal)
        stats_table2 <- makeStatsTable(rbind(past_results, scores2))
        standings2 <- buildStandingsTable(stats_table2)
        playoff_list2 <- c(rownames(getConferenceStandings(standings2, "East")[1:8, 
            ]), rownames(getConferenceStandings(standings2, "West")[1:8, ]))
        
        pp[, "Points_StDev"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), 
            ]$P, stats_table2[order(stats_table2$Team), ]$P), 1, sd)
        pp[, "Points"] <- apply(cbind(pp[, "Points"], stats_table[order(stats_table$Team), 
            ]$P, stats_table2[order(stats_table2$Team), ]$P), 1, mean)
        pp[, "Playoffs_StDev"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% 
            playoff_list, rownames(pp) %in% playoff_list2), 1, sd)
        pp[, "Playoffs"] <- apply(cbind(pp[, "Playoffs"], rownames(pp) %in% 
            playoff_list, rownames(pp) %in% playoff_list2), 1, mean)
        pp[, "Presidents_StDev"] <- apply(cbind(pp[, "Presidents"], standings[, 
            "1"], standings2[, "1"]), 1, sd)
        pp[, "Presidents"] <- apply(cbind(pp[, "Presidents"], standings[, "1"], 
            standings2[, "1"]), 1, mean)
        for (i in 4:n) {
            scores <- predictRemainderOfSeason(res = res, schedule = schedule, 
                stats = stats, maxgoal = maxgoal)
            stats_table <- makeStatsTable(rbind(past_results, scores))
            standings <- buildStandingsTable(stats_table)
            playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, 
                ]), rownames(getConferenceStandings(standings, "West")[1:8, 
                ]))
            
            pp[, "Points_StDev"] <- v_new_stdev(pp[, "Points_StDev"], pp[, "Points"], 
                i - 1, stats_table[order(stats_table$Team), ]$P)
            pp[, "Points"] <- v_new_mean(pp[, "Points"], i - 1, stats_table[order(stats_table$Team), 
                ]$P)
            pp[, "Playoffs_StDev"] <- v_new_stdev(pp[, "Playoffs_StDev"], pp[, 
                "Playoffs"], i - 1, rownames(pp) %in% playoff_list)
            pp[, "Playoffs"] <- v_new_mean(pp[, "Playoffs"], i - 1, rownames(pp) %in% 
                playoff_list)
            pp[, "Presidents_StDev"] <- v_new_stdev(pp[, "Presidents_StDev"], 
                pp[, "Presidents"], i - 1, standings[, "1"])
            pp[, "Presidents"] <- v_new_mean(pp[, "Presidents"], i - 1, standings[, 
                "1"])
        }
    }
    
    return(pp)
}
{% endhighlight %}
 
The interesting things are: chances of president's trophy and chances to make playoffs. 
 
We can get this info quite simply from the above code:
 

{% highlight r %}
results <- point_predict(res_2016, future_games, nhl_2016_stats, past_results = nhl2016, 
    n = 1000)
pandoc.table(results[, c(1, 3, 5)], style = "rmarkdown", caption = "Chances of playoff and President's Trophy per team")
{% endhighlight %}



|           &nbsp;            |  Points  |  Playoffs  |  Presidents  |
|:---------------------------:|:--------:|:----------:|:------------:|
|      **Anaheim Ducks**      |  97.27   |     1      |      0       |
|     **Arizona Coyotes**     |  80.26   |     0      |      0       |
|      **Boston Bruins**      |  95.33   |    0.95    |      0       |
|     **Buffalo Sabres**      |  75.64   |     0      |      0       |
|     **Calgary Flames**      |  74.83   |     0      |      0       |
|   **Carolina Hurricanes**   |  88.03   |    0.01    |      0       |
|   **Chicago Blackhawks**    |  101.6   |     1      |      0       |
|   **Colorado Avalanche**    |  89.71   |    0.66    |      0       |
|  **Columbus Blue Jackets**  |  76.01   |     0      |      0       |
|      **Dallas Stars**       |   104    |     1      |      0       |
|    **Detroit Red Wings**    |  93.92   |    0.79    |      0       |
|     **Edmonton Oilers**     |  73.77   |     0      |      0       |
|    **Florida Panthers**     |  98.53   |     1      |      0       |
|    **Los Angeles Kings**    |  105.3   |     1      |      0       |
|     **Minnesota Wild**      |  88.89   |    0.34    |      0       |
|   **Montreal Canadiens**    |  84.53   |     0      |      0       |
|   **Nashville Predators**   |  98.03   |     1      |      0       |
|    **New Jersey Devils**    |  87.58   |    0.01    |      0       |
|   **New York Islanders**    |   101    |     1      |      0       |
|    **New York Rangers**     |  101.6   |     1      |      0       |
|     **Ottawa Senators**     |  85.59   |     0      |      0       |
|   **Philadelphia Flyers**   |  91.72   |    0.24    |      0       |
|   **Pittsburgh Penguins**   |  97.74   |     1      |      0       |
|     **San Jose Sharks**     |   102    |     1      |      0       |
|     **St. Louis Blues**     |  103.4   |     1      |      0       |
|   **Tampa Bay Lightning**   |  99.63   |     1      |      0       |
|   **Toronto Maple Leafs**   |   73.6   |     0      |      0       |
|    **Vancouver Canucks**    |  75.93   |     0      |      0       |
|   **Washington Capitals**   |  119.2   |     1      |      1       |
|      **Winnipeg Jets**      |  73.18   |     0      |      0       |

Table: Chances of playoff and President's Trophy per team
 
Next time we'll evaluate the performance of the different models and validate our predictions!
 
* Note that there is a tiebreaker not currently built in whereby head to head results matter for two tied teams. This isn't usually required for playoff make or break decisions, particularly in a Monte-Carlo prediction. *
