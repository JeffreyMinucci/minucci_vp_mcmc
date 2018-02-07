##
#  Code to calculate likelihood for a varroapop run given field data on colony population 
#
#  Author: Jeffrey Minucci
#
#
##


#Create hypothetical field data (replace with real data when available)
#adult_pop_month1 <- floor(rnorm(10,4500,1000))


#Function to calculate log likelihood for one date of observations and one prediction for all sites

vp_loglik_simple <- function(actual,pred,var){
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum((actual - pred)^2)
  return(ll)
}


#Function to calculate log likelihood for all dates of observations and one prediction for all sites
# notes: actual - a matrix containing values for 10 sites (rows) across the 3 time points (columns)
#        pred - vector of length 3 containing the model predictions for months 1 2 and 4
#        var - a single variance term (for now calculate from one month's data)
vp_loglik_dates <- function(actual,pred,var){
  residuals <- c(actual[,1]-pred[1],actual[,2]-pred[2],actual[,3]-pred[3])
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum(residuals^2)
  return(ll)
}