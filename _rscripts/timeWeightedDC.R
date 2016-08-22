tau <- Vectorize(function(xx, yy, lambda, mu, rho){
    if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
    } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
    } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
    } else if (xx == 1 & yy == 1){return(1 - rho)
    } else {return(1)}
})

DCweights <- function(dates, currentDate=Sys.Date(), xi=0){
    datediffs <- dates - as.Date(currentDate)
    datediffs <- as.numeric(datediffs *-1)
    w <- exp(-1*xi*datediffs)
    w[datediffs <= 0] <- 0 #Future dates should have zero weights
    return(w)
}

DClogLik <- function(y1, y2, lambda, mu, rho=0, weights=NULL){
    #rho=0, independence
    #y1 home goals
    #y2 away goals
    loglik <- log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu))
    if (is.null(weights)){
        return(sum(loglik))
    } else {
        return(sum(loglik*weights))
    }
}

DCmodelData <- function(df){
    team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))

    # attack, with sum-to-zero constraint
    ## home
    hm.a <- model.matrix(~ HomeTeam - 1, data=df)
    hm.a[df$HomeTeam == team.names[length(team.names)], ] <- -1
    hm.a <- hm.a[,1:(length(team.names)-1)]

    # away
    am.a <- model.matrix(~ AwayTeam -1, data=df)
    am.a[df$AwayTeam == team.names[length(team.names)], ] <- -1
    am.a <- am.a[,1:(length(team.names)-1)]

    # defence, same as before
    hm.d <- model.matrix(~ HomeTeam - 1, data=df)
    am.d <- model.matrix(~ AwayTeam -1, data=df)

    return(list(homeTeamDMa=hm.a, homeTeamDMd=hm.d,
                awayTeamDMa=am.a, awayTeamDMd=am.d,
                homeGoals=df$HG, awayGoals=df$AG,
                dates=df$Date,
                teams=team.names))
}

DCoptimFn <- function(params, DCm, xi=0, currentDate=Sys.Date()){
    home.p <- params[1]
    rho.p <- params[2]

    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams+1)], ncol=1) #one column less
    defence.p <- matrix(params[(nteams+2):length(params)], ncol=1)

    # need to multiply with the correct matrices
    lambda <- exp(DCm$homeTeamDMa %*% attack.p + DCm$awayTeamDMd %*% defence.p + home.p)
    mu <- exp(DCm$awayTeamDMa %*% attack.p + DCm$homeTeamDMd %*% defence.p)

    w<-DCweights(DCm$dates, xi=xi, currentDate = currentDate)
    return(
        DClogLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p, w) * -1
    )
}

doDCPrediction<-function(df, xi=0, currentDate = Sys.Date()){
    #Get a useful data set
    dcm<-DCmodelData(df)
    nteams <- length(dcm$teams)

    #dummy fill parameters
    #initial parameter estimates
    attack.params <- rep(0.01, times=nteams-1) # one less parameter
    defence.params <- rep(-0.7, times=nteams)
    home.param <- 0.1
    rho.init <- 0.1
    par.inits <- c(home.param, rho.init, attack.params, defence.params)

    #informative names
    #skip the last team
    names(par.inits) <- c('HOME', 'RHO',
                          paste('Attack', dcm$teams[1:(nteams-1)], sep='.'),
                          paste('Defence', dcm$teams, sep='.'))

    res <- optim(par=par.inits, fn=DCoptimFn, DCm=dcm, xi=xi, method = "Nelder-Mead", hessian=FALSE, currentDate=currentDate)

    parameters <- res$par

    #compute last team attack parameter
    missing.attack <- sum(parameters[3:(nteams+1)]) * -1

    #put it in the parameters vector
    parameters <- c(parameters[1:(nteams+1)], missing.attack, parameters[(nteams+2):length(parameters)])
    names(parameters)[nteams+2] <- paste('Attack.', dcm$teams[nteams], sep='')

    #increase attack by one
    parameters[3:(nteams+2)] <- parameters[3:(nteams+2)] + 1

    #decrease defence by one
    parameters[(nteams+3):length(parameters)] <- parameters[(nteams+3):length(parameters)] - 1

    res$par<-parameters
    return(res)
}
