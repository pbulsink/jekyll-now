---
title: "New Elo for NHL"
author: "Philip Bulsink"
date: '2016-10-12'
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey ranking Elo
---
 

 
Last time, we looked at Elo ratings for NHL teams. We saw that the more fancy Elo ratings didn't keep the average constant, that is, ratings were inflated through time. As well, they didn't take into consideration any summertime normalization, whereby the elo ratings were adjusted towards the mean as is very common in sports Elo. Any other common adjustments, such as placing increased rating on playoff games, aren't contained therein either. I'll look into developing a set of those tools, specific to our usages.. 
 
<!--more-->
 
##Calculating Elo Rating
 
When two teams play eachother, their Elo 'score' is both an expectation of who will win, as well as a component of how the scores move afterwards. For example, if a highly ranked team loses to a very poorly ranked team, the change in Elo for both teams will be much more than if two teams that are similarly ranked play eachother. 
Elo is calculated first by getting an expected win chance, then comparing the expected to the actual performance of each team.
 
$E_A = \frac 1 {1 + 10^{(R_B - R_A)/400}}$
 
$E_B = \frac 1 {1 + 10^{(R_A - R_B)/400}}$
 
Where $E_A$ and $E_B$ are the expected chance of winning (actually the expected score ratio), and $R_A$ and $R_B$ are the rating of team A and B, respectively. Note that $E_A + E_B = 1$. 
 
Once this is calculated, these values are compared to the actual results. Results for each game are either 0, 0.5, or 1, corresponding to a win by team B, a draw, or a win by team A.
 
The new rating or team A ($R_A^\prime$) is thus:
 
$R_A^\prime = R_A + K(S_A - E_A)$
 
where $S_A$ is the actual result of the game, and $K$ is a factor determining by how much scores can move in each game. I'll talk about that later, but we'll start with $K = 8$. Similarly for team B $R_B^\prime = R_B + K(S_B - E_B)$, where $S_B = 1 - S_A$. Note that the absolute change in score for each team is the same, so that the average of the league is unmoved.
 
For example, two teams are playing eachother, team A has a rating of $R_A=1600$, and team B has a rating of $R_B=1450$. For this scenario, $E_A = 0.7034$ and $E_B = 0.2966$. If team B wins, then the new rating of A is $R_A^\prime = 1594$ and, similarly, $R_B^\prime=1456$ for B. Both teams moved by 6 points (well, 5.6 points, but we'll keep it as integers for now).
 
Teams who are ranked evenly would move 4 points. If an underdog team won with 0.05 chance of doing so, the rank change would be 8 (actually 7.6), but if the expected team won, the change would be too small to move the ranking on an integer basis.
 
Implementing this in R is trivial.
 

{% highlight r %}
predictEloResult<-function(home_rank, away_rank){
    return(1/(1+(10^((away_rank-home_rank)/400))))
}
 
newRankings<-function(home_rank, away_rank, result, k=8){
    #result is in set [0, 0.5, 1]
    d_rank<-k*(result - predictEloResult(home_rank, away_rank))
    return(c(home_rank+d_rank, away_rank-d_rank))
}
{% endhighlight %}
 
##Elo Over a Season
Elo was designed as a ranking system for chess, and is commonly used in a context where teams may not play eachother often. By having a unified ranking system, it was possible to know who was 'the best' at any time, based on their historical performance and how they perform currently. But, in the NHL, we have a more formal round robin season and playoffs to determine the best, year after year. So the value of Elo is in its predictive power, as far as past performance is an indicator of future performance. 
 
At the end of the season, it would make sense that teams are rated roughly along the lines of where they placed in the standings, particularly the teams that had long playoff runs. They have more wins than losses in the previous n games. But, comparing teams mid-season is more difficult, there are 'difficulty of schedule' issues, or a team may be on fire (like Montreal in the fall of 2015) and look destined to great things prior to falling off the bandwagon (like Montreal in the winter/spring of 2016). Elo allows us to watch the rise and fall of teams in much shorter timeframes. 
 
One aspect that is commonly considred in season based Elo ratings is a normalization of the rankings, at the end of every season, to account for off-season adjustments. Bad teams usually get good draft picks. Good teams often lose a star when free agency comes around. Typically, rankings are normalized to the mean by 1/3. So, if a rating is $R$, the mean is $\overline{R}$ and the new rating is $R^\prime$, then
 
$R^\prime = \frac{3(R) + \overline{R}}{4}$
 
This raises the question of what to rank new teams. While some systems rank them at the mean (1500), this implies that the new team will be better than about half of the league. Instead, it's common to bring new teams in at 1400 or 1350. However, this would bias the overall ranking of the league downward every time a new team arrived. Thus, instead of using the actual mean of 1500 to moderate towards, one can moderate slightly above, say to $\overline{R} = 1505$. This will cause slow drift upwards in league rankings, but will be counterbalanced by the arrival of new teams, such as Las Vegas for the 2017-2018 season.
 
##Keeping Track of Elo
There's a few ways that we could keep track of the Elo ratings of teams. The most cumbersome (in terms of storage) yet only complete way is to have a gigantic matrix, that updates after every game day, with all of the new scores, moved or not. That could look like this, or be a simple cbind in the middle of other code.
 

{% highlight r %}
updateEloHistory<-function(new_elos, elo_history=NULL){
    #Only works if new_elos and elo_histoy have the same nrows. 
    if(history){
        cbind(elo_history, new_elos)
    }
    else{
        history<-data.frame(new_elos)
    }
}
{% endhighlight %}
 
##Sewing This All Together
All of this code can be added together (with helper loop functions) to give us a set of functions that will rank teams using the Elo system, with the discussed extra adjustments. That code can be seen in the github for this site, available [here](http://github.com/pbulsink/pbulsink.github.io).
 
This set of tools isn't ready yet, but I'm making progress. Keep your eyes open!
 
##But Wait, There's More!
Some things we haven't taken into consideration. 
 
- Since the 1990-1991 season, the home team scores on average 0.3050 goals per game more than the away team. This turns into a home ice advantage of just over 10% (the Home team has won 55% of all games since 1990-1991, but that has varied a bit). To properly adjust for this, we should account for home ice advantage in our predictions. The rating difference that this advantage corresponds to is about 35 points. 
 
- Should all games be considered at the same strength? Some models give playoff games a higher importance, up to 1.2 times that of a regular season game. We can try that, and see how it does.
 
 
 
 
