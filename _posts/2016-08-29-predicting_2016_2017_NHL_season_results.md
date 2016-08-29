---
title: "Predicting 2016-2017 NHL Season Results"
author: "Philip Bulsink"
date: "August 29, 2016"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
tags: R hockey predicting Dixon-Coles 
---
 
Now to some new posts! 
 
When making a prediction engine, it's always fun to see what next season looks like. We have the schedule for the 2016-2017 NHL season, and we have all the data from the past seasons, so let's get some calculations going! 
 
<!--more-->
 
This will hopefully eventually maybe become a daily-updating model, that watches teams as they play though the season to see what happens. The cool thing would be to have available the effect of different scenarios, such as winning 'tonight's' game, or someone elses' fate depending on your own results. These are maybe eventual projects.
 
All the code I'm using is available in earlier posts, so all you need to know is:
- There are no corrections for draft picks, trades, retirements, or coaching changes.
- For the time dependant Dixon-Coles, I'm using data for the past 10 years only, including playoffs, with a &xi; value of 0.0191464. 
- For the quick Dixon-Coles, I'm using only 2 years of data. 
- Each team has 0.5 chance of winning if a game goes to overtime.
- I'm simulating the season 1,000 times for each model. 
 
So, without further ado, here is where I predict each team to come:
 

 

{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}



{% highlight text %}
## Warning in log(tau(y1, y2, lambda, mu, rho)): NaNs produced
{% endhighlight %}
 

Ten Year (Time weighted) Predictions:

{% highlight text %}
## Loading required package: reshape2
{% endhighlight %}

![plot of chunk prediction2017_ten_plot](/images/prediction2017_ten_plot-1.png)

|                      |  Points| Points_StDev| Playoffs| Playoffs_StDev| Presidents| Presidents_StDev|
|:---------------------|-------:|------------:|--------:|--------------:|----------:|----------------:|
|Anaheim Ducks         | 109.000|     7.233416|    0.999|      0.0316386|      0.375|        0.4842630|
|Arizona Coyotes       |  80.200|     7.453306|    0.245|      0.4301299|      0.000|        0.0000000|
|Boston Bruins         |  92.403|     7.785852|    0.705|      0.4564995|      0.012|        0.1089943|
|Buffalo Sabres        |  81.846|     7.477373|    0.187|      0.3903020|      0.001|        0.0316386|
|Calgary Flames        |  83.726|     8.131040|    0.419|      0.4935513|      0.001|        0.0316386|
|Carolina Hurricanes   |  87.105|     7.681840|    0.462|      0.4987185|      0.000|        0.0000000|
|Chicago Blackhawks    |  92.570|     7.736179|    0.817|      0.3870538|      0.010|        0.0995984|
|Colorado Avalanche    |  80.640|     7.929385|    0.265|      0.4417752|      0.000|        0.0000000|
|Columbus Blue Jackets |  79.613|     7.880256|    0.149|      0.3564451|      0.000|        0.0000000|
|Dallas Stars          |  88.630|     7.869941|    0.667|      0.4714044|      0.003|        0.0547448|
|Detroit Red Wings     |  85.773|     7.553647|    0.361|      0.4807715|      0.001|        0.0316386|
|Edmonton Oilers       |  76.206|     7.489911|    0.115|      0.3193414|      0.000|        0.0000000|
|Florida Panthers      |  93.698|     7.848801|    0.766|      0.4237962|      0.009|        0.0945350|
|Los Angeles Kings     | 103.748|     7.339339|    0.993|      0.0834562|      0.124|        0.3299116|
|Minnesota Wild        |  88.838|     7.829049|    0.680|      0.4669433|      0.003|        0.0547448|
|Montreal Canadiens    |  66.957|     7.520434|    0.001|      0.0316386|      0.000|        0.0000000|
|Nashville Predators   |  98.922|     7.397501|    0.963|      0.1889508|      0.037|        0.1889508|
|New Jersey Devils     |  72.540|     7.470007|    0.019|      0.1366615|      0.000|        0.0000000|
|New York Islanders    |  92.233|     7.742756|    0.696|      0.4600804|      0.007|        0.0834562|
|New York Rangers      |  90.219|     7.921334|    0.595|      0.4913837|      0.005|        0.0706043|
|Ottawa Senators       |  83.888|     8.195576|    0.291|      0.4546784|      0.000|        0.0000000|
|Philadelphia Flyers   |  96.354|     7.382187|    0.864|      0.3431319|      0.021|        0.1435277|
|Pittsburgh Penguins   | 106.311|     7.446622|    0.992|      0.0891734|      0.217|        0.4122105|
|San Jose Sharks       |  98.719|     7.883655|    0.954|      0.2096949|      0.051|        0.2202181|
|St. Louis Blues       |  91.675|     7.620314|    0.801|      0.3992298|      0.006|        0.0773043|
|Tampa Bay Lightning   |  98.241|     7.494192|    0.917|      0.2761585|      0.035|        0.1839638|
|Toronto Maple Leafs   |  74.215|     7.567980|    0.030|      0.1707581|      0.000|        0.0000000|
|Vancouver Canucks     |  65.538|     7.888516|    0.005|      0.0706043|      0.000|        0.0000000|
|Washington Capitals   | 101.645|     7.395835|    0.965|      0.1839638|      0.082|        0.2746395|
|Winnipeg Jets         |  72.839|     8.016188|    0.077|      0.2668584|      0.000|        0.0000000|
 
Two Year (Fast DC) Predictions:

 
![plot of chunk prediction2017_two_plot](/images/prediction2017_two_plot-1.png)

|                      |  Points| Points_StDev| Playoffs| Playoffs_StDev| Presidents| Presidents_StDev|
|:---------------------|-------:|------------:|--------:|--------------:|----------:|----------------:|
|Anaheim Ducks         |  93.169|     7.731947|    0.770|      0.4212540|      0.029|        0.1679745|
|Arizona Coyotes       |  68.936|     8.027814|    0.015|      0.1216742|      0.000|        0.0000000|
|Boston Bruins         |  90.412|     7.684662|    0.608|      0.4883438|      0.016|        0.1256008|
|Buffalo Sabres        |  67.986|     7.788246|    0.005|      0.0706043|      0.000|        0.0000000|
|Calgary Flames        |  85.326|     7.933175|    0.375|      0.4846078|      0.002|        0.0447214|
|Carolina Hurricanes   |  80.648|     7.709971|    0.169|      0.3751272|      0.001|        0.0316386|
|Chicago Blackhawks    |  97.545|     7.249420|    0.914|      0.2800491|      0.083|        0.2761585|
|Colorado Avalanche    |  85.329|     7.716444|    0.375|      0.4846078|      0.002|        0.0447214|
|Columbus Blue Jackets |  79.475|     7.746379|    0.123|      0.3287661|      0.000|        0.0000000|
|Dallas Stars          |  92.013|     7.886374|    0.720|      0.4490768|      0.016|        0.1256008|
|Detroit Red Wings     |  89.420|     7.765708|    0.567|      0.4956501|      0.014|        0.1176081|
|Edmonton Oilers       |  70.936|     7.846020|    0.015|      0.1216742|      0.000|        0.0000000|
|Florida Panthers      |  90.791|     7.517580|    0.633|      0.4824692|      0.015|        0.1216742|
|Los Angeles Kings     |  96.647|     7.409917|    0.896|      0.3055663|      0.082|        0.2746395|
|Minnesota Wild        |  94.382|     7.674147|    0.816|      0.3874415|      0.040|        0.1961554|
|Montreal Canadiens    |  88.925|     7.933237|    0.528|      0.4993810|      0.010|        0.0995984|
|Nashville Predators   |  95.036|     7.680257|    0.831|      0.3751272|      0.048|        0.2139803|
|New Jersey Devils     |  79.252|     7.716103|    0.127|      0.3328050|      0.001|        0.0316386|
|New York Islanders    |  93.563|     7.780624|    0.743|      0.4374170|      0.032|        0.1761763|
|New York Rangers      |  99.077|     7.910492|    0.915|      0.2791610|      0.145|        0.3524535|
|Ottawa Senators       |  89.174|     7.880902|    0.548|      0.4978538|      0.008|        0.0891734|
|Philadelphia Flyers   |  85.659|     7.927004|    0.348|      0.4764627|      0.006|        0.0773043|
|Pittsburgh Penguins   |  93.905|     7.709638|    0.764|      0.4250474|      0.033|        0.1778790|
|San Jose Sharks       |  91.245|     7.829255|    0.691|      0.4621827|      0.021|        0.1435277|
|St. Louis Blues       |  94.316|     7.626936|    0.814|      0.3890674|      0.035|        0.1839638|
|Tampa Bay Lightning   |  98.013|     7.529268|    0.901|      0.2989611|      0.113|        0.3169099|
|Toronto Maple Leafs   |  75.768|     8.129146|    0.063|      0.2432063|      0.000|        0.0000000|
|Vancouver Canucks     |  84.334|     7.855733|    0.339|      0.4738439|      0.003|        0.0547448|
|Washington Capitals   | 101.966|     7.567301|    0.958|      0.2007900|      0.242|        0.4283335|
|Winnipeg Jets         |  86.073|     7.776350|    0.429|      0.4950918|      0.003|        0.0547448|
 
