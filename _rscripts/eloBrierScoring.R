source("./_rscripts/eloSeasonPredicting.R")

#' Calculates and returns brier score
#'
#' @param predicted A vector or data frame of predictions (0..1)
#' @param results A vector or data frame of results (0,1)
#' @return a number representing the binary Brier score. 0 is perfect prediction
brier<-function(predicted, results){
    n<-nrow(predicted)
    return(sum((predicted-results)^2)/n)
}

#' Calculates and returns adjbrier score
#'
#' @param predicted A vector or data frame of predictions (0..1)
#' @param results A vector or data frame of results (0,1)
#' @return a number representing the binary Brier score. 0 is perfect prediction
adjBrier<-function(predicted, results){
    n<-nrow(predicted)
    return(list('score'=n-(predicted-results)^2), 'max'=n)
}

#' Calculates odds of each outcome and tabulates results
#'
#' @param eloHist EloHistory covering all days of games in schedule
#' @param schedule All games to calculate brier score over
#' @param pWin pWin logit fit
#' @param pLoss pLoss logit fit
#' @return A list of 2 data frames of predictions and actual results
getPredictedResults<-function(eloHist, schedule,pWin,pLoss){
    #For recent Elo, we have results of 0, 0.25, 0.4, 0.6, 0.75, 1
    #Coded as VWin,VOT,VSO,HSO,HOT,HWin
    #While ties existed in the past, they'll artificially boost our performance
    ngames<-nrow(schedule)
    predicted<-data.frame('VWin'=rep(0,ngames), 'VOT'=rep(0), 'VSO'=rep(0), 'HSO'=rep(0), 'HOT'=rep(0), 'HWin'=rep(0))
    results<-predicted
    for(g in c(1:ngames)){
        home<-make.names(as.character(schedule[g,'Home']))
        visitor<-make.names(as.character(schedule[g,'Visitor']))
        elo_diff<-tail(eloHist[eloHist$Date < schedule[g, 'Date'],home],1)-tail(eloHist[eloHist$Date < schedule[g, 'Date'],visitor],1)

        #Results
        if (schedule[g,'Result'] == 0){
            results[g,'VWin']<-1
        }
        else if (schedule[g,'Result'] == 0.25){
            results[g, 'VOT']<-1
        }
        else if (schedule[g,'Result'] == 0.40){
            results[g, 'VSO']<-1
        }
        else if (schedule[g,'Result'] == 0.60){
            results[g, 'HSO']<-1
        }
        else if (schedule[g,'Result'] == 0.75){
            results[g, 'HOT']<-1
        }
        else if (schedule[g,'Result'] == 1){
            results[g, 'HWin']<-1
        }
        #Predictions
        predicted[g,'VWin']<-v<-predict(pLoss, data.frame("EloDiff"=elo_diff))
        predicted[g,'HWin']<-h<-predict(pWin, data.frame("EloDiff"=elo_diff))
        #Draw
        d<-1-(v+h)
        #log5 home win in OT/SO
        phw<-(w-w*l)/(w+l-2*w*l)
        predicted[g,'VOT']<-0.4*(1-phw)*d
        predicted[g,'VSO']<-0.6*(1-phw)*d
        predicted[g,'HSO']<-0.6*phw*d
        predicted[g,'HOT']<-0.4*phw*d
    }
    return(list('predicted'=predicted, 'results'=results))
}

#'one call script to calculate brier score. Calculates predicted odds, results, brier
#'
#' @param eloHist EloHistory covering all days of games in schedule
#' @param schedule All games to calculate brier score over
#' @param pWin pWin logit fit
#' @param pLoss pLoss logit fit
#' @return a single numerical brier score
seasonBrierScore<-function(eloHist, schedule, pWin, pLoss){
    pr<-getPredictedResults(eloHist, schedule, pWin, pLoss)
    predicted<-pr$predicted
    results<-pr$results
    return(brier(predicted, results))
}

#'one call script to calculate adjusted brier score. Calculates predicted odds, results, brier
#'
#' @param eloHist EloHistory covering all days of games in schedule
#' @param schedule All games to calculate brier score over
#' @param pWin pWin logit fit
#' @param pLoss pLoss logit fit
#' @return a single numerical brier score
seasonBrierScore<-function(eloHist, schedule, pWin, pLoss){
    pr<-getPredictedResults(eloHist, schedule, pWin, pLoss)
    predicted<-pr$predicted
    results<-pr$results
    return(adjBrier(predicted, results))
}
