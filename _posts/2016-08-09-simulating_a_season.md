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



|         Team          |  GP  |  W  |  OTL  |  L  |  ROW  |  P  |  GF  |  GA  |  DIFF  |
|:---------------------:|:----:|:---:|:-----:|:---:|:-----:|:---:|:----:|:----:|:------:|
|  Washington Capitals  |  82  | 60  |   7   | 15  |  55   | 127 | 275  | 189  |   86   |
|  Chicago Blackhawks   |  82  | 49  |   7   | 26  |  48   | 105 | 244  | 202  |   42   |
|   Los Angeles Kings   |  82  | 50  |   5   | 27  |  48   | 105 | 227  | 185  |   42   |
|  Tampa Bay Lightning  |  82  | 49  |   5   | 28  |  45   | 103 | 236  | 198  |   38   |
|   New York Rangers    |  82  | 47  |   9   | 26  |  42   | 103 | 240  | 224  |   16   |
|    St. Louis Blues    |  82  | 46  |   9   | 27  |  41   | 101 | 216  | 212  |   4    |
|     Dallas Stars      |  82  | 45  |  10   | 27  |  42   | 100 | 262  | 246  |   16   |
|  Nashville Predators  |  82  | 43  |  14   | 25  |  40   | 100 | 230  | 212  |   18   |
|    San Jose Sharks    |  82  | 46  |   7   | 29  |  43   | 99  | 242  | 220  |   22   |
|     Anaheim Ducks     |  82  | 43  |  10   | 29  |  40   | 96  | 212  | 204  |   8    |
|  New York Islanders   |  82  | 43  |  10   | 29  |  39   | 96  | 232  | 217  |   15   |
|     Boston Bruins     |  82  | 43  |   9   | 30  |  39   | 95  | 246  | 228  |   18   |
|   Florida Panthers    |  82  | 43  |   9   | 30  |  36   | 95  | 233  | 219  |   14   |
|  Pittsburgh Penguins  |  82  | 42  |  10   | 30  |  39   | 94  | 220  | 210  |   10   |
|   Detroit Red Wings   |  82  | 41  |  11   | 30  |  39   | 93  | 209  | 218  |   -9   |
|  Philadelphia Flyers  |  82  | 39  |  15   | 28  |  37   | 93  | 219  | 234  |  -15   |
|    Minnesota Wild     |  82  | 40  |  12   | 30  |  38   | 92  | 226  | 209  |   17   |
|  Colorado Avalanche   |  82  | 42  |   4   | 36  |  38   | 88  | 230  | 239  |   -9   |
|  Carolina Hurricanes  |  82  | 36  |  15   | 31  |  35   | 87  | 204  | 227  |  -23   |
|   New Jersey Devils   |  82  | 38  |   8   | 36  |  36   | 84  | 192  | 213  |  -21   |
|    Ottawa Senators    |  82  | 38  |   8   | 36  |  31   | 84  | 239  | 255  |  -16   |
|  Montreal Canadiens   |  82  | 38  |   7   | 37  |  33   | 83  | 235  | 239  |   -4   |
|    Arizona Coyotes    |  82  | 36  |   8   | 38  |  35   | 80  | 226  | 251  |  -25   |
|    Buffalo Sabres     |  82  | 34  |  10   | 38  |  32   | 78  | 203  | 222  |  -19   |
| Columbus Blue Jackets |  82  | 35  |   8   | 39  |  29   | 78  | 224  | 263  |  -39   |
|    Calgary Flames     |  82  | 35  |   6   | 41  |  32   | 76  | 231  | 263  |  -32   |
|     Winnipeg Jets     |  82  | 35  |   5   | 42  |  30   | 75  | 219  | 252  |  -33   |
|    Edmonton Oilers    |  82  | 33  |   8   | 41  |  28   | 74  | 206  | 243  |  -37   |
|   Vancouver Canucks   |  82  | 30  |  14   | 38  |  26   | 74  | 198  | 243  |  -45   |
|  Toronto Maple Leafs  |  82  | 31  |  11   | 40  |  25   | 73  | 210  | 249  |  -39   |

Table: Predicted standings at end of season
 
But, while doing that once is cool, we need to do that over and over again to really get a picture of the chance each team has of making the playoffs by the end of the season. This we'll do with `n=1000`, to save time.
 

{% highlight r %}
nhl_2016_predicted_standings <- simulateSeason(res_2016, future_games, nhl_2016_stats, 
    past_results = nhl2016, n = 1000)
pandoc.table(nhl_2016_predicted_standings, style = "rmarkdown", caption = "Predicted position at end of season after 1000 simulations")
{% endhighlight %}



|           &nbsp;            |  1   |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:---------------------------:|:----:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|      **Anaheim Ducks**      |  0   |  9  | 25  | 45  | 52  | 72  | 79  | 93  | 104 | 117  | 123  | 107  |  78  |  65  |  24  |  7   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Arizona Coyotes**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  6   |  14  |  53  | 165  | 354  | 182  |  89  |  68  |  31  |  21  |  13  |  3   |
|      **Boston Bruins**      |  0   |  0  |  2  |  2  |  4  |  7  | 10  | 25  | 44  |  65  |  86  | 108  | 173  | 241  | 143  |  64  |  18  |  7   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Buffalo Sabres**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  1   |  20  | 112  | 213  | 201  | 148  | 156  |  85  |  46  |  17  |
|     **Calgary Flames**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  9   |  30  |  78  | 130  | 158  | 175  | 122  | 130  | 105  |  62  |
|   **Carolina Hurricanes**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  3   |  10  |  51  | 102  | 205  | 236  | 199  | 130  |  54  |  9   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Chicago Blackhawks**    |  0   | 78  | 151 | 149 | 129 | 136 | 115 | 66  | 62  |  43  |  28  |  17  |  20  |  5   |  0   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Colorado Avalanche**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  2   |  0   |  3   |  14  |  63  | 146  | 233  | 212  | 165  |  96  |  55  |  11  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Columbus Blue Jackets**  |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  8   |  87  | 154  | 146  | 160  | 145  | 139  |  94  |  63  |
|      **Dallas Stars**       |  0   | 205 | 297 | 159 | 114 | 88  | 45  | 44  | 22  |  10  |  10  |  5   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Detroit Red Wings**    |  0   |  0  |  0  |  0  |  2  |  0  |  2  |  6  |  9  |  8   |  20  |  61  |  74  | 152  | 331  | 192  |  89  |  35  |  16  |  1   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Edmonton Oilers**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  18  |  28  |  64  | 106  | 150  | 169  | 235  | 228  |
|    **Florida Panthers**     |  0   |  9  | 34  | 47  | 68  | 99  | 84  | 103 | 107 |  93  | 129  |  78  |  71  |  55  |  20  |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Los Angeles Kings**    |  0   | 575 | 177 | 87  | 68  | 37  | 18  | 18  | 12  |  1   |  5   |  1   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Minnesota Wild**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  3   |  6   |  9   |  64  | 146  | 275  | 203  | 162  |  70  |  47  |  14  |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Montreal Canadiens**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  1   |  1   |  14  |  30  |  67  | 132  | 247  | 334  | 119  |  36  |  14  |  5   |  0   |  0   |  0   |  0   |
|   **Nashville Predators**   |  0   |  2  |  1  |  6  | 14  | 21  | 31  | 47  | 77  | 101  | 117  | 142  | 175  | 156  |  78  |  21  |  7   |  4   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New Jersey Devils**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  1   |  0   |  0   |  6   |  22  |  61  | 120  | 197  | 251  | 201  | 109  |  28  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |
|   **New York Islanders**    |  0   | 16  | 34  | 65  | 91  | 73  | 109 | 93  | 115 | 101  | 103  |  86  |  66  |  26  |  16  |  3   |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New York Rangers**     |  0   | 14  | 61  | 104 | 111 | 98  | 127 | 113 | 102 |  69  |  69  |  57  |  40  |  24  |  9   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Ottawa Senators**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  3   |  9   |  25  |  72  |  93  | 202  | 241  | 237  |  96  |  17  |  5   |  0   |  0   |  0   |  0   |  0   |
|   **Philadelphia Flyers**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  2  |  3  |  6   |  7   |  18  |  44  |  75  | 159  | 309  | 171  | 110  |  57  |  32  |  7   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Pittsburgh Penguins**   |  0   |  2  |  5  | 14  | 30  | 37  | 49  | 76  | 88  | 125  | 100  | 146  | 141  | 110  |  54  |  20  |  2   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **San Jose Sharks**     |  0   | 35  | 66  | 117 | 116 | 122 | 107 | 116 | 79  |  99  |  46  |  57  |  27  |  9   |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **St. Louis Blues**     |  0   | 50  | 124 | 151 | 140 | 129 | 120 | 83  | 78  |  45  |  41  |  24  |  11  |  2   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Tampa Bay Lightning**   |  0   |  5  | 23  | 54  | 61  | 81  | 104 | 115 | 98  | 117  | 114  |  89  |  69  |  54  |  14  |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Toronto Maple Leafs**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  24  |  47  |  60  |  89  | 139  | 198  | 439  |
|    **Vancouver Canucks**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  9   |  58  | 118  | 153  | 143  | 151  | 143  | 145  |  78  |
|   **Washington Capitals**   | 1000 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|      **Winnipeg Jets**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  6   |  37  |  94  | 122  | 135  | 156  | 174  | 164  | 110  |

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
 

|           &nbsp;            |  1   |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:---------------------------:|:----:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|   **Washington Capitals**   | 1000 |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **New York Islanders**    |  0   | 16  | 34  | 65  | 91  | 73  | 109 | 93  | 115 | 101  | 103  |  86  |  66  |  26  |  16  |  3   |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New York Rangers**     |  0   | 14  | 61  | 104 | 111 | 98  | 127 | 113 | 102 |  69  |  69  |  57  |  40  |  24  |  9   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Florida Panthers**     |  0   |  9  | 34  | 47  | 68  | 99  | 84  | 103 | 107 |  93  | 129  |  78  |  71  |  55  |  20  |  3   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Tampa Bay Lightning**   |  0   |  5  | 23  | 54  | 61  | 81  | 104 | 115 | 98  | 117  | 114  |  89  |  69  |  54  |  14  |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Pittsburgh Penguins**   |  0   |  2  |  5  | 14  | 30  | 37  | 49  | 76  | 88  | 125  | 100  | 146  | 141  | 110  |  54  |  20  |  2   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|      **Boston Bruins**      |  0   |  0  |  2  |  2  |  4  |  7  | 10  | 25  | 44  |  65  |  86  | 108  | 173  | 241  | 143  |  64  |  18  |  7   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Detroit Red Wings**    |  0   |  0  |  0  |  0  |  2  |  0  |  2  |  6  |  9  |  8   |  20  |  61  |  74  | 152  | 331  | 192  |  89  |  35  |  16  |  1   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|   **Philadelphia Flyers**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  2  |  3  |  6   |  7   |  18  |  44  |  75  | 159  | 309  | 171  | 110  |  57  |  32  |  7   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **New Jersey Devils**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  1   |  0   |  0   |  6   |  22  |  61  | 120  | 197  | 251  | 201  | 109  |  28  |  3   |  1   |  0   |  0   |  0   |  0   |  0   |
|   **Carolina Hurricanes**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  3   |  10  |  51  | 102  | 205  | 236  | 199  | 130  |  54  |  9   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Ottawa Senators**     |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  3   |  9   |  25  |  72  |  93  | 202  | 241  | 237  |  96  |  17  |  5   |  0   |  0   |  0   |  0   |  0   |
|   **Montreal Canadiens**    |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  1   |  1   |  14  |  30  |  67  | 132  | 247  | 334  | 119  |  36  |  14  |  5   |  0   |  0   |  0   |  0   |
|  **Columbus Blue Jackets**  |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  8   |  87  | 154  | 146  | 160  | 145  | 139  |  94  |  63  |
|     **Buffalo Sabres**      |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  1   |  20  | 112  | 213  | 201  | 148  | 156  |  85  |  46  |  17  |
|   **Toronto Maple Leafs**   |  0   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  3   |  24  |  47  |  60  |  89  | 139  | 198  | 439  |

Table: Predicted Eastern Conference standings
 
So, the Detroit Red Wings likely squeak into the playoffs, while Philadelphia Flyers just miss them. But, of course, the odds are good that it could be the other way around too! Nothing is set in stone until the final whistle blows.
 
Similarly, the West standings would look like this:
 

|          &nbsp;           |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |  11  |  12  |  13  |  14  |  15  |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |  24  |  25  |  26  |  27  |  28  |  29  |  30  |
|:-------------------------:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|   **Los Angeles Kings**   |  0  | 575 | 177 | 87  | 68  | 37  | 18  | 18  | 12  |  1   |  5   |  1   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Dallas Stars**      |  0  | 205 | 297 | 159 | 114 | 88  | 45  | 44  | 22  |  10  |  10  |  5   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Chicago Blackhawks**   |  0  | 78  | 151 | 149 | 129 | 136 | 115 | 66  | 62  |  43  |  28  |  17  |  20  |  5   |  0   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **St. Louis Blues**    |  0  | 50  | 124 | 151 | 140 | 129 | 120 | 83  | 78  |  45  |  41  |  24  |  11  |  2   |  2   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **San Jose Sharks**    |  0  | 35  | 66  | 117 | 116 | 122 | 107 | 116 | 79  |  99  |  46  |  57  |  27  |  9   |  3   |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|     **Anaheim Ducks**     |  0  |  9  | 25  | 45  | 52  | 72  | 79  | 93  | 104 | 117  | 123  | 107  |  78  |  65  |  24  |  7   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Nashville Predators**  |  0  |  2  |  1  |  6  | 14  | 21  | 31  | 47  | 77  | 101  | 117  | 142  | 175  | 156  |  78  |  21  |  7   |  4   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|  **Colorado Avalanche**   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  2   |  0   |  3   |  14  |  63  | 146  | 233  | 212  | 165  |  96  |  55  |  11  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Minnesota Wild**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  3   |  6   |  9   |  64  | 146  | 275  | 203  | 162  |  70  |  47  |  14  |  1   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |
|    **Arizona Coyotes**    |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  6   |  14  |  53  | 165  | 354  | 182  |  89  |  68  |  31  |  21  |  13  |  3   |
|    **Calgary Flames**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  1   |  9   |  30  |  78  | 130  | 158  | 175  | 122  | 130  | 105  |  62  |
|   **Vancouver Canucks**   |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  9   |  58  | 118  | 153  | 143  | 151  | 143  | 145  |  78  |
|     **Winnipeg Jets**     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  6   |  37  |  94  | 122  | 135  | 156  | 174  | 164  | 110  |
|    **Edmonton Oilers**    |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  0   |  2   |  18  |  28  |  64  | 106  | 150  | 169  | 235  | 228  |

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

point_predict <- function(res, schedule, stats, past_results, n = 10000, maxgoal = 8, 
    m = NULL) {
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
            maxgoal = maxgoal, m = m)
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
            maxgoal = maxgoal, m = m)
        stats_table <- makeStatsTable(rbind(past_results, scores))
        standings <- buildStandingsTable(stats_table)
        playoff_list <- c(rownames(getConferenceStandings(standings, "East")[1:8, 
            ]), rownames(getConferenceStandings(standings, "West")[1:8, ]))
        
        scores2 <- predictRemainderOfSeason(res = res, schedule = schedule, 
            stats = stats, maxgoal = maxgoal, m = m)
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
                stats = stats, maxgoal = maxgoal, m = m)
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
{% endhighlight %}



{% highlight text %}
## Error in predictRemainderOfSeason(res = res, schedule = schedule, stats = stats, : unused argument (m = m)
{% endhighlight %}



{% highlight r %}
pandoc.table(results[, c(1, 3, 5)], style = "rmarkdown", caption = "Chances of playoff and President's Trophy per team")
{% endhighlight %}



{% highlight text %}
## Error in pandoc.table.return(...): object 'results' not found
{% endhighlight %}
 
Next time we'll evaluate the performance of the different models and validate our predictions!
 
* Note that there is a tiebreaker not currently built in whereby head to head results matter for two tied teams. This isn't usually required for playoff make or break decisions, particularly in a Monte-Carlo prediction. *
