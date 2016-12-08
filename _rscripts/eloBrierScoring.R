source("./_rscripts/eloSeasonPredicting.R")
source("./_rscripts/calculateEloRatings.R")
require(scoring)
require(reshape2)
require(optimx)
require(doParallel)
require(MLmetrics)

#' Calculates odds of each outcome and tabulates results
#'
#' @param eloHist EloHistory covering all days of games in schedule
#' @param schedule All games to calculate brier score over
#' @param pWin pWin logit fit
#' @param pLoss pLoss logit fit
#' @return A list of 2 data frames of predictions and actual results
getPredictedResults<-function(eloHist, schedule,pWin,pLoss){
    #For recent Elo, we have results of 0, 0.25, 0.4, 0.6, 0.75, 1
    #Coded as VWin, Draw ,HWin
    ngames<-nrow(schedule)
    predicted<-data.frame('VWin'=rep(0,ngames), 'D'=rep(0), 'HWin'=rep(0))
    results<-rep(0, ngames)
    for(g in c(1:ngames)){
        home<-make.names(as.character(schedule[g,'Home']))
        visitor<-make.names(as.character(schedule[g,'Visitor']))
        elo_diff<-tail(eloHist[eloHist$Date < schedule[g, 'Date'],home],1)-tail(eloHist[eloHist$Date < schedule[g, 'Date'],visitor],1)

        #Results
        if (schedule[g,'Result'] < 0.4){
            results[g]<-1
        }
        else if (schedule[g,'Result'] > 0.6){
            results[g]<-3
        }
        else{
            results[g]<-2
        }
        #Predictions
        predicted[g,'VWin']<-v<-predict(pLoss, data.frame("EloDiff"=elo_diff), type='response')
        predicted[g,'HWin']<-h<-predict(pWin, data.frame("EloDiff"=elo_diff), type='response')
    }
    predicted$D<-1-(predicted$VWin+predicted$HWin)
    return(list('predicted'=predicted, 'results'=results))
}

#'one call script to calculate scores score. Calculates predicted odds, results, returns list of brier, LL scores
#'
#' @param eloHist EloHistory covering all days of games in schedule
#' @param schedule All games to calculate brier score over
#' @param pWin pWin logit fit
#' @param pLoss pLoss logit fit
#' @return a single numerical brier score. 0 is best, 1 is worst
seasonScore<-function(eloHist, schedule, pWin, pLoss){
    pr<-getPredictedResults(eloHist, schedule, pWin, pLoss)
    predicted<-pr$predicted
    results<-pr$results
    message('multibrier')
    multiBrier<-sum(calcscore(results~predicted$VWin+predicted$D+predicted$HWin, bounds=c(0,1)))/nrow(predicted)
    results[results == 1]<-'VWin'
    results[results == 2]<-'D'
    results[results == 3]<-'HWin'
    pre<-predicted$HWin/(predicted$HWin+predicted$VWin)
    res<-rep(0, length(results))
    res[results == 'HWin']<-1
    message('multiLL')
    multiLL<-MultiLogLoss(y_true=results, y_pred=predicted)
    message('binll')
    binLL<-LogLoss(y_true=res, y_pred=pre)
    message('binbrier')
binBrier<-sum(unlist(brierscore(res~pre)))/length(pre)
    return(list('multiLL'=multiLL, 'binLL'=binLL, 'multiBrier'=multiBrier, 'binBrier'=binBrier))
}

scoreEloVar<-function(p=c('kPrime'=10, 'gammaK'=1), regressStrength=3, homeAdv=0, newTeam=1300, nhl_data){
    #Calculate Elo by parameter
    kPrime=p[1]
    gammaK=p[2]
    #regressStrength=p[3]
    #homeAdv=p[4]
    #newTeam=1500-4*p[5] # Thus, as p5 goes up, newteam ratings go down. Typically p[5] = 50, newTeam=1300
    message(paste0("Calculating Elo with kPrime=",kPrime," gammaK=",gammaK," newTeam=",newTeam," regStrength=",regressStrength," homeAdv=",homeAdv))
    elo<-calculateEloRatings(nhl_data, k=kPrime, gammaK = gammaK, new_teams = newTeam, regress_strength = regressStrength, home_adv = homeAdv, k_var = TRUE, meta=FALSE)

    #Calculate pWin, pLoss
    message("Calculating pWin, pLoss")
    pResults<-pResCalc(elo$Ratings, nhl_data)
    pWin<-pResults$pWin
    pLoss<-pResults$pLoss
    pw<-predict(pWin, data.frame("EloDiff"=0), type='response')
    message(paste0('pWin at even: ', pw))

    #Calculate Brier Score
    message("Calculating Scores Score")
    elo16<-elo$Ratings[(elo$Ratings$Date>as.Date("2015-08-01") & elo$Ratings$Date<as.Date("2016-08-01")),]
    nhl16<-nhl_data[(nhl_data$Date > as.Date("2015-08-01") & nhl_data$Date < as.Date("2016-08-01")),]
    s<-seasonScore(elo16, schedule = nhl16, pWin, pLoss)
    message(paste0("LLMulti: ", s['multiLL'], " LLBinary: ", s['binLL'], " BrierMulti: ", s['multiBrier'], " BrierBinary: ", s['binBrier']))
    scores<-list('kPrime'=kPrime, 'gammaK'=gammaK, 'multiLL'=s['multiLL'], 'binLL'=s['binLL'], 'multiBrier'=s['multiBrier'], 'binBrier'=s['binBrier'])
    return(scores)

}

optEloVar<-function(nhl_data){
    return(optim(par=c(10, 1), fn=scoreEloVar, nhl_data=nhl_data))  #, lower=0, upper=100))
}

pResCalc<-function(elo, nhl_data){
    message('massage elo data')

    #PWin/PLoss since 2005 when Shootout came into effect.
    elo<-elo[elo$Date > as.Date('2005-08-31'),]
    nhl_data<-nhl_data[nhl_data$Date > as.Date('2005-08-31'),]
    elo_long<-melt(elo, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)
    elo_long<-rbind(elo_long, NA)
    #removes rows where no change has occurred (a team didn't play)
    elo_long<-elo_long[(filter(elo_long,c(-1,1))!= 0)[,3],]
    elo_long<-elo_long[complete.cases(elo_long),]

    eloAtGameTime<-function(game){
        elod<-tail(elo[elo$Date < as.Date(game['Date']), make.names(c(game['Home'], game['Visitor']))], 1)
        return(as.numeric(elod[1] - elod[2]))
    }
    nhl_data$EloDiff<-0
    nhl_data$EloDiff<-apply(nhl_data, 1, function(x) eloAtGameTime(x))

    propresults<-list(EloDiff=numeric(), Win=numeric(), Draw=numeric(), Loss=numeric())

    message('tabulate elo results')
    for (i in unique(round(nhl_data$EloDiff))){
        propresults$EloDiff<-c(propresults$EloDiff, i)
        x<-nhl_data[round(nhl_data$EloDiff) == i,]
        propresults$Win<-c(propresults$Win, length(x[x$Result>0.6,'Result']))
        propresults$Draw<-c(propresults$Draw, length(x[(x$Result <= 0.6 & x$Result >= 0.4),'Result']))
        propresults$Loss<-c(propresults$Loss, length(x[x$Result < 0.4, 'Result']))
    }
    propresults<-as.data.frame(propresults)

    propresults<-propresults[order(propresults$EloDiff), ]
    propresults$Total<-propresults$Win+propresults$Draw+propresults$Loss

    propresults$nWin<-propresults$Draw+propresults$Loss
    propresults$nLoss<-propresults$Draw+propresults$Win

    pWin<-glm(cbind(Win, nWin)~EloDiff, data = propresults, family = binomial('logit'))
    pLoss<-glm(cbind(Loss, nLoss)~EloDiff, data = propresults, family = binomial('logit'))

    return(list('pWin'=pWin, 'pLoss'=pLoss))
}

eloVarPlotData<-function(nhl_data){
    cl <- makeCluster(6, outfile="./stdout.log")
    registerDoParallel(cl)
    exportFuns<-c('scoreEloVar', 'seasonScore', 'calculateEloRatings', 'pResCalc', 'splitDates', '.eloSeason', 'predictEloResult', 'newRankings', 'variableK', 'getPredictedResults')
    scores<-foreach(i=0:100, .export=exportFuns, .packages = c('scoring', 'reshape2', 'MLmetrics')) %:% foreach(j=0:30) %dopar% scoreEloVar(p=c(i/2, j/10), regressStrength=3, homeAdv=0, newTeam=1300, nhl_data)

    stopCluster(cl)

    return(scores)
}
