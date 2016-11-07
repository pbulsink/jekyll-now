### Functions for Currie estimation of LOD/LOQ From Rajakovic et. al.,
### Talanta 102 (2012) 79-87. doi:10.1016/j.talanta.2012.08.016 and Currie,
### Anal.  Chem. 40 (1968) 586-593 and Currie, Pure Appl. Chem. 67 (1995)
### 1699-1723 by Philip Bulsink

variance <- function(x) {
    # Sum (i=1..n) (xi-xbar)^2
    return(sum((x - mean(x))^2))
}

intercept.sd <- function(x, y) {
    # s_a
    return(regression.sd(x, y) * ((1/length(x) + ((mean(x)^2)/variance(x)))))
}

regression.sd <- function(x, y) {
    # s_y/x
    fit <- lm(y ~ x)
    a <- fit$coefficients[[1]]
    b <- fit$coefficients[[2]]
    return(sqrt((sum(y - a - b * x)^2)/(length(x) - 2)))
}

eta <- function(M, x) {
    return((1/M) + (1/length(x)) + (sum(mean(x)^2)/(variance(x))))
}

eta.root <- function(M, x) {
    return(sqrt(eta(M, x)))
}

concentration.lod <- function(x, y, M = 1, falsePositive = 0.95) {
    tp <- qt(1 - 0.5 * (1 - falsePositive), df = (length(x) - 1))
    return((tp * regression.sd(x, y) * eta.root(M, x))/(lm(y ~ x)$coefficients[[2]]))
}

concentration.loq <- function(x, y, M = 1, falsePositive = 0.95, falseNegative = 0.99) {
    tp <- qt(1 - 0.5 * (1 - falsePositive), df = (length(x) - 1))
    tq <- qt(1 - 0.5 * (1 - falseNegative), df = (length(x) - 1))
    return(((tp + tq) * regression.sd(x, y) * eta.root(M, x))/(lm(y ~ x)$coefficients[[2]]))
}

response.lod <- function(x, y, M = 1, falsePositive = 0.95) {
    tp <- qt(1 - 0.5 * (1 - falsePositive), df = (length(x) - 1))
    return(tp * regression.sd(x, y) * eta.root(M, x))
}

response.loq <- function(x, y, M = 1, falsePositive = 0.95, falseNegative = 0.99) {
    tp <- qt(1 - 0.5 * (1 - falsePositive), df = (length(x) - 1))
    tq <- qt(1 - 0.5 * (1 - falseNegative), df = (length(x) - 1))
    return((tp + tq) * regression.sd(x, y) * eta.root(M, x))
}

calculateCurrieLimits <- function(cal, replicate_cals = 1, falsePositive = 0.95, 
    falseNegative = 0.99) {
    # input: list(x=..., y-...) with x, y values of calibration.
    stopifnot(is.list(cal))
    
    lod_loq<-list(
        lod_x = concentration.lod(cal$x, cal$y, replicate_cals, falsePositive),
        lod_y = response.lod(cal$x, cal$y, replicate_cals, falsePositive),
        loq_x = concentration.loq(cal$x, cal$y, replicate_cals, falsePositive, falseNegative),
        loq_y = response.loq(cal$x, cal$y, replicate_cals, falsePositive, falseNegative)
        )
    return(lod_loq)
}

inputCurrieData <- function(dataplace) {
    
}
