###
#
# Functions for Currie estimation of LOD/LOQ
# From Rajakovic et. al., Talanta 102 (2012) 79-87. doi:10.1016/j.talanta.2012.08.016
# and Currie, Anal. Chem. 40 (1968) 586-593
# and Currie, Pure Appl. Chem. 67 (1995) 1699-1723
#
# by Philip Bulsink
#
###

variance <- function(x) {
  #Sum (i=1..n) (xi-xbar)^2
  mean.x<-mean(x)
  variance<-0
  for(i in c(1:length(x))){
    var.x.sum <- variance + (x[i]-mean.x)^2
  }
  return(variance)
}

intercept.sd <- function(x,y) {
  #s_a
  return(regression.sd(x,y)*((1/length(x)+((mean(x)^2)/variance(x)))))
}

regression.sd <- function(y,x) {
  #s_y/x
  return()
}

eta <- function(M, x) {
  return((1/M)+(1/length(x))+((mean(x)^2)/(variance(x))))
}

eta.root <- function(eta) {
  return(sqrt(eta))
}

concentration.lod <- function() {
}

concentration.loq <- function() {
}

response.lod <- function() {
}

response.loq <- function() {
}

calculateCurrieLimits <- function(cal, falsePositive=0.95, falseNegative=0.99) {
  #input: list(x=..., y-...) with x, y values of calibration. 
  #compoundname, stock calibration, dilutionfactor, response, dilutionfactor,response...
}

inputCurrieData <- function(dataplace) {
  
}
