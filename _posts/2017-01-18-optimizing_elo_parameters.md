---
title: "Optimizing Elo Parameters for Game Predictions"
author: "Philip Bulsink"
date: "January 18, 2017"
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey Rating Elo plots 
---
 

 
In the past few weeks, I've been optimizing parameters for Elo based predicting of NHL data. The code is complex and won't be put here. Check the [sourcecode](http://github.com/pbulsink/pbulsink.github.io) in the repo.
 
I've put those results together in a combo plot, built using `ggplot` and `gridExtra` to make things better arranged. Check the [sourcecode for this post](http://github.com/pbulsink/pbulsink.github.io) for the details on that. 
 
<!--more-->
 
I evaluated a range of K prime and gamma K values using [Log Loss](https://en.wikipedia.org/wiki/Loss_functions_for_classification#Logistic_loss) and [Brier scoring](https://en.wikipedia.org/wiki/Brier_score) methods. 
For both of these methods, lower scores are better. 
 

 

{% highlight r %}
grid.arrange(mLL6, mLLWD, mLLWOTD, mB6, mBWD, mBWOTD, bLL, bB, pR, ncol=3, nrow=3)
{% endhighlight %}

![plot of chunk multiplot_scores](/images/multiplot_scores-1.png)
 
How good is this? Not super good.
 
The first row of results are complex multi-possibility LogLoss situations. These are hard to give a 'coinflip' expected score to, but they correllate with a '6 scenario' correct (predicting chances of Win, OTWin, SOWin, SOLoss, OTLoss, and Loss). Similarly, the predictions for Win/Loss/Draw and Win/OTWin/'Draw'/OTLoss/Loss combine aspects of the randomness of OT or SO games. All of the predictions for each are based on combinations of their own results, so OT wins are included in determining the win chance for binary case, but not for the '6 scenario' case. 
 
Similarly, the second row are the same scenarios calculated for Brier score.
 
The last row are the scenarios pared down to predictions of win/loss. Log loss of any given game when you predict it to go 50/50 is, using the MLmetrics package, `MLmetrics::LogLoss(0.5, 1) ==` 0.6931472. Similarly, the average coinflip Brier score should result in a score around 0.25. And our 'percentRight' should be about 50%. So, we do perform better than that periodically.
 
Sometimes, though, these things look reversed, such as the better performance of the Percent Right plot at high gammaK and kPrime, but the poorer performance there for all the Brier results. If I can solve that, I'll update this post.
 
At the end, though, I don't see this as being that useful. Being only a few points better at predicting game results than a coinflip is not a selling point. But, maybe the value is in season predicting? I'll look at some of that in the future. 
 
*Update*
But first, I've come to realize that I was overly-complicated in generating predictions for the winning team. Recall that the original ELO formula contains a predictive equation for the home team's chances $P_home$:
 
$$P_{home} = 1/(1+10~^{(R_{away} - R_{home})})$$
 
where $$R_{home}$$ is the home team ranking, and $$R_{away}$$ is the away ranking. 
 
Using this, I've re-run the optimization, and achieved the following results:
 

 

{% highlight r %}
grid.arrange(loglossplot, brierplot, percentplot, ncol=3, nrow=1)
{% endhighlight %}

![plot of chunk multiplot_scores2](/images/multiplot_scores2-1.png)
 
But, after all that, we can look and see that this isn't really an improvement. Oh well.
