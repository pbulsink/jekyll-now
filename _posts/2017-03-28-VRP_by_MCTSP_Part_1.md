---
title: "TSP in R Part 1"
author: "Philip Bulsink"
date: '2017-03-28'
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
maps: true
tags: R TSP Ottawa 
---
 

 
I've been playing around recently with some Travelling Salesperson Problems (TSP), and by extension some Vehicle Routing Problems (VRP). For example, when people come to visit Ottawa, are they being the most optimal with visiting a list of sites, that is, spending the least time or distance in their cars travelling between places? If their trip takes more than one day, does that change the order they see things?
 
<!--more-->
 
They'd like to know that a) their daily routes are optimized, and b) their stops are optimized to the day. The first question is one of the typical TSP, and the second is a basic VRP. Instead of having, say, 3 vehicles running routes from a single starting point (like a hotel), there's one vehicle running a route each of the 3 days of their visit.
 
So, we'll start from a list of things to see, and go from there. First, we need to process the data and get it into a usable format
 

{% highlight r %}
loadLocationData <- function(f) {
  locations <- read.csv(f, stringsAsFactors = FALSE)
  locations$Number <- as.numeric(locations$Number)
  return(locations)
}
 
locations<-loadLocationData("./_data/museums.csv")
head(locations)
{% endhighlight %}



{% highlight text %}
##                                     Name Number               Street    City
## 1             Prime Minister's Residence     24           Sussex Dr.  Ottawa
## 2 Billings Estate National Historic Site   2100            Cabot St.  Ottawa
## 3                   Britannia Yacht Club   2777          Cassels St.  Ottawa
## 4                          Bytown Museum      1           Canal Lane  Ottawa
## 5                          Byward Market     55 Byward Market Square  Ottawa
## 6   Cameron Highlanders of Ottawa Museum      2  Queen Elizabeth Dr.  Ottawa
{% endhighlight %}
 
The list of museums is given as Name, Street Number, Street, City. 
 
We'll use the [Google Maps Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/) to calculate the distance between each point. We can then optimize the route visiting each one offline before asking Google for the actual route. 
 
We'll stitch every part of the location frame together to give a list of places for Google. It does better without a place name. We can then feed this into our API URL creator. Note I add the API key, you'll have to get your own:
 

{% highlight r %}
#dm_api_key = [YOUR API KEY HERE]
 
constructDistanceUrl <- function(origins, destinations, dm_api_key) {
  root <- "https://maps.googleapis.com/maps/api/distancematrix/"
  u <- paste(root, "json", "?origins=", origins, "&destinations=", destinations, "&key=",
    dm_api_key, sep = "")
  return(u)
}
{% endhighlight %}
 
But hold on. The free tier of the Google API only allows for a maximum of 25 origins or destinations at once. And a total of 100 elements per request (where elements = origins x destinations). And a total of 2500 elements in a day . We have to a) make a way to break up our requests, and b) prevent going over 2500/day.  We can also only call 
 
We'll request in chunks:
 
| Request | Origin | Destination |
|---------|--------|-------------|
| 1 | 1:10 | 1:10 |
| 2 | 1:10 | 11:20 |
| ... | ... | ... |
| k | 11:20 | 1:10 |
| k+1 | 11:20 | 11:20 |
| ... | ... | ... |
| n | 40:48 | 40:48 |
 

{% highlight r %}
buildRequestList <- function(nlocations) {
  s_main <- 1:10
  s_last <- (1:(nlocations%%10)) + (10 * (nlocations%/%10))
  s_reps <- nlocations%/%10 - 1
 
  call_list <- list()
  counter <- 1
  for (i in 0:s_reps) {
    for (j in 0:s_reps) {
      r <- list(origin = s_main + i * 10, destin = s_main + j * 10)
      call_list[counter] <- list(r)
      counter <- counter + 1
    }
    r <- list(origin = s_last, destin = s_main + i * 10)
    call_list[counter] <- list(r)
    counter <- counter + 1
    r <- list(origin = s_main + i * 10, destin = s_last)
    call_list[counter] <- list(r)
    counter <- counter + 1
  }
  r <- list(origin = s_last, destin = s_last)
  call_list[counter] <- list(r)
  return(call_list)
}
{% endhighlight %}
 
This gives us a set of requests, by which point we'll have all of the distance matrices we need. Then we'll sew them together for one giant matrix.
 

{% highlight r %}
getAPIurls <- function(locations, api_key_file = "./_data/apikey.txt") {
  # Reformat location information for API Calls
  places <- apply(locations, 1, function(x) paste(x[2], x[3], x[4]))
  places <- trimws(places)
  places <- gsub(" ", "+", places)
 
  # Get Request Indices
  request_indices <- buildRequestList(length(places))
 
  urllist <- character()
 
  for (i in 1:length(request_indices)) {
    o <- request_indices[[i]]$origin
    origins <- paste0(places[o], collapse = "|")
    d <- request_indices[[i]]$destin
    destinations <- paste0(places[d], collapse = "|")
    urllist <- c(urllist, constructDistanceUrl(origins = origins, destinations = destinations, dm_api_key = dm_api_key))
  }
 
  return(urllist)
}
 
urllist<-getAPIurls(locations)
substring(urllist[1],1,400)
{% endhighlight %}



{% highlight text %}
## [1] "https://maps.googleapis.com/maps/api/distancematrix/json?origins=24+Sussex+Dr.+Ottawa|2100+Cabot+St.+Ottawa|2777+Cassels+St.+Ottawa|1+Canal+Lane+Ottawa|55+Byward+Market+Square+Ottawa|2+Queen+Elizabeth+Dr.+Ottawa|901+Prince+of+Wales+Dr.+Ottawa|11+Aviation+Parkway+Ottawa|2421+Lancaster+Rd.+Ottawa|7800+Golf+Club+Way+Ashton&destinations=24+Sussex+Dr.+Ottawa|2100+Cabot+St.+Ottawa|2777+Cassels+St.+Ottaw"
{% endhighlight %}
 
We can process the returned json with this function. It looks gnarly but will suffice until I rewrite with applys:
 

{% highlight r %}
processDistanceMatrixJson <- function(jdata, value = "distance", ...) {
  stopifnot(value %in% c("distance", "duration"))
  norigin <- length(jdata$origin_addresses)
  ndestin <- length(jdata$destination_addresses)
  dm <- matrix(NA, nrow = norigin, ncol = ndestin)
  for (i in 1:norigin) {
    for (j in 1:ndestin) {
      q <- jdata$rows[[i]]$elements[[j]]
      if (q$status == "OK") {
        dm[i, j] <- as.numeric(q[[value]]$value)
      }
    }
  }
  rownames(dm) <- jdata$origin_addresses
  colnames(dm) <- jdata$destination_addresses
  return(dm)
}
{% endhighlight %}
 
We can do this for all calculated request groups:
 

{% highlight r %}
getGDMResults <- function(urllist) {
  dmat_list <- tmat_list <- rawurl_list <- json_list <- error_list <- list(rep(NA, length(urllist)))
  for (i in 1:length(urllist)) {
    rawurl_list[[i]] <- RCurl::getURL(urllist[i])
    json_list[[i]] <- rjson::fromJSON(rawurl_list[[i]])
    if (json_list[[i]]$status == "OK") {
      dmat_list[[i]] <- processDistanceMatrixJson(json_list[[i]], value = "distance")
      tmat_list[[i]] <- processDistanceMatrixJson(json_list[[i]], value = "duration")
    }
    Sys.sleep(2)
  }
  return(list(dmat = dmat_list, tmat = tmat_list, rawurl = rawurl_list, json = json_list,
    errors = error_list))
}
 
matlist<-getGDMResults(urllist = urllist)
{% endhighlight %}
 
Time is given in seconds, and distance in meters. I had to hide the names of each place because they made the table too big. Note that these are not symmetrical, because sometimes there are one way streets or other limitations on routes. 
 

{% highlight r %}
unname(matlist$dmat[[1]])
{% endhighlight %}



{% highlight text %}
##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
##  [1,]     0  8400 16953  4879  2569  3406  9393  5708  9452 46980
##  [2,]  8370     0 19858 12693  7789  5386  6006 10883  5007 48822
##  [3,] 20202 20489     0 14284 17985 16302 12238 24150 21269 34148
##  [4,]  2480  8473 14727     0  1554  2132  8576  8189  9253 46163
##  [5,]  2224  7719 16445  5841     0  1661  7822  7933  8499 45409
##  [6,]  3509  5291 14793  5199  2095     0  7248 11713  8832 43757
##  [7,]  9218  7065 11045  7575 10121  7281     0 16286 13404 40009
##  [8,]  5613 11051 22613 10493  6238  9019 13990     0 11255 51577
##  [9,]  9066  5037 20281 13843  8211  9120 10594  7959     0 49245
## [10,] 47179 47466 34085 44475 44962 43279 39215 51127 48246     0
{% endhighlight %}
 

 
Once we have everything available, in lists of matrices, we can stitch them into a majorly big matrix. Again, gnarly until rewritten with applys:

{% highlight r %}
assembleMatrix <- function(mlist) {
  locations <- unique(as.character(unlist(sapply(mlist, rownames))))
  nloc <- length(locations)
  mat_total <- matrix(NA, nrow = nloc, ncol = nloc)
  rownames(mat_total) <- colnames(mat_total) <- locations
  for (i in 1:length(mlist)) {
    d <- mlist[[i]]
    if (!is.na(d) && !is.null(d)) {
        mat_total[match(rownames(d), rownames(mat_total),  nomatch=0), match(colnames(d), colnames(mat_total), nomatch = 0)]<-d
    }
  }
  return(mat_total)
}
 
distance_matrix <- assembleMatrix(matlist$dmat)
time_matrix <- assembleMatrix(matlist$tmat)
 
#Save the distance and times matrices to file for later use
saveRDS(distance_matrix, "./_data/distance_matrix.RDS")
saveRDS(time_matrix, "./_data/time_matrix.RDS")
{% endhighlight %}
 
Now that we have the distance matrices, we can start the Travelling Salesperson Problem work with the R package `TSP`. More in the next post.
