---
title: "NHL and Elo Through the Years - Part 3"
author: "Philip Bulsink"
date: "November 11, 2016"
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey Rating Elo plots 
---
 

 
Having Elo ratings for teams over all time is cool, but how do we know that it's meaningful? Sure, we can look at the Stanley Cup winning team each year, and see that they typically have a good rating. Or, we can anicdotally look back at our favourite team, remember how good or bad they were for a few seasons in the past, and see that they were near the top or the bottom of the pile at that point in time.
 
<!--more-->
 
For example, here's a plot of the Stanley Cup (or, at least the season championship) winning team's rating and the average rating of the league(s) at that time. Remember, I have WHA data mixed in, you'll notice that the Houston Aeros fit through the cracks on this quick analysis. And, because the teams history is carried through under their current name, you can see that Arizona Coyotes won the championship at one time (1976 WHA as the Winnipeg Jets). You can see that the winning team is typically ranked much better than the average, as expected.

{% highlight r %}
sc_ratings<-list('year'=numeric(), 'rating'=numeric(), 'team'=character(), 'mean_rating'=numeric())
for (i in c(1918:2016)){
    sc_ratings$year<-c(sc_ratings$year, i)
    last_game<-tail(nhl_results[nhl_results$Date < as.Date(paste0(i, '-07-31')),],1)
    win_team<-""
    ifelse(last_game$Result > 0.5, win_team<-as.character(last_game$Home), win_team<-as.character(last_game$Visitor))
    sc_ratings$team <-c(sc_ratings$team, win_team)
    rating<-tail(elo_all_long[(elo_all_long$Team == make.names(win_team) & elo_all_long$Date < as.Date(paste0(i,'-07-31'))),],1)$Rating
    sc_ratings$rating<-c(sc_ratings$rating, rating)
    sc_ratings$mean_rating<-c(sc_ratings$mean_rating, tail(elo_data$Meta[elo_data$Meta$season.end < as.Date(paste0(i, '-07-31')), 'mean'],1))
}
sc_ratings<-as.data.frame(sc_ratings)
 
ggplot(sc_ratings, aes(x=year, y=rating)) + 
     geom_point(aes(colour = factor(team))) + 
     geom_point(aes(x=year, y=mean_rating), colour='grey') + 
     theme_bw() + theme(legend.position='bottom') + 
     guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) + 
     labs(color="Team") + 
     xlab("Year") + 
     ylab("Rating") + 
     ggtitle("Rating of Championship Team vs. Year")
{% endhighlight %}

![plot of chunk stanley_cup_winner](/images/stanley_cup_winner-1.png)
 
But, there should be some quantitative things we can check to make sure that a) ratings make a difference in how teams do, and b) if we use it to make predictions, that those have at least some value. 
 
First, we'll extract the Elo rating for each team going into a game.
 

{% highlight r %}
eloAtGameTime<-function(game){
    h_elo<-tail(elo_long[(elo_long$Date < as.Date(game['Date']) & elo_long$Team == make.names(game['Home'])),'Rating'],1)
    v_elo<-tail(elo_long[(elo_long$Date < as.Date(game['Date']) & elo_long$Team == make.names(game['Visitor'])),'Rating'],1)
    return(c(h_elo, v_elo))
}
gameresults<-apply(nhl_data, 1, function(x) eloAtGameTime(x))
nhl_data$HomeElo<-gameresults[1,]
nhl_data$VisitorElo<-gameresults[2,]
nhl_data$EloDiff<-nhl_data$HomeElo-nhl_data$VisitorElo
{% endhighlight %}
 
Having done that (warning, this is a slow implementation, speeding it up would be very very helpful), we can try making some plots of Elo vs. different aspects of the games' result. Let's start with simply looking at the predictive power for each game
#plot scatter of elo adv. (including home) by win proportion.
 

{% highlight r %}
ggplot(nhl_data) + 
    geom_boxplot(aes(x=as.factor(Result), y=EloDiff)) + 
    geom_smooth(aes(x=Result*8, y=EloDiff), method=lm) +
    theme_bw() +
    ggtitle("Elo Ranking Difference vs. Result") +
    xlab("Result (0 = Away Win)") +
    ylab("Elo Difference (Home-Away)")
{% endhighlight %}

![plot of chunk elo_vs_results](/images/elo_vs_results-1.png)
 
Is it predictive? Yes.... Is it strongly predictive? I'd say no. There are pleanty of examples where the better team loses, or the worse team wins. At some point there was a team that was rated over 400 points higher, and lost. Similarly, there are pleanty of examples of teams over 300 points worse and losing. 
 
The thing is, we don't know by what margins these teams won or lost. Maybe we can get more of that information out of a goal differential relationship. 
 

{% highlight r %}
nhl_data$GoalDiff<-nhl_data$HomeGoals-nhl_data$VisitorGoals
ggplot(nhl_data) + 
    geom_boxplot(aes(x=as.factor(GoalDiff), y=EloDiff)) + 
    geom_smooth(aes(x=GoalDiff+14, y=EloDiff), method=lm, colour='red') +
    geom_smooth(aes(x=GoalDiff+14, y=EloDiff)) + 
    theme_bw() +
    ggtitle("Elo Difference vs. Goal Difference") +
    xlab("Goal Differential (Home - Away)") +
    ylab("Elo Difference (Home-Away)")
{% endhighlight %}

![plot of chunk elo_vs_goal_diff](/images/elo_vs_goal_diff-1.png)
 
That looks much better. There are many examples of teams with better ratings losing, but they typically don't lose by much. The inverse is true too. 
 
We can look at the data and say, with more confidence, that there is a loose relationship between Elo rating differential and goal differential. While there's still lots of uncertainty (as with all macro prediction schemes), there is a relationship.
 

 
For those who are curious, the equation of that line of best fit is y=10.420061162924x-5.47926227051295. 
