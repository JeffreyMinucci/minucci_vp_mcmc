##
# Calculate likelihood for a varroapop run given field data on colony population 
#
# code by: Jeffrey Minucci
#
##


# Function to calculate log likelihood for one date of observations and one prediction for all sites
#
# @param actual: vector of actual population data
# @param prediction: a single VP population prediction
# @param var: the predicted variance in population values
# 
# @return: a loglikelihood value

vp_loglik_simple <- function(actual,pred,var){
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum((actual - pred)^2)
  return(ll)
}



#Function to calculate log likelihood for all dates of observations and one prediction for all sites
# @param actual: a matrix containing values for 10 sites (rows) across the 3 time points (columns)
# @param pred: vector of length 3 containing the model predictions for months 1 2 and 4
# @param var: a single variance term (for now calculate from one month's data)
#
# @return: a loglikelihood value

vp_loglik_dates <- function(actual,pred,var){
  residuals <- c(actual[,1]-pred[1],actual[,2]-pred[2],actual[,3]-pred[3])
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum(residuals^2)
  return(ll)
}

#Function to calculate log likelihood for all dates of observations and one prediction for all sites
# @param actual: a matrix containing ACTUAL values for 10 sites (rows) across the 3 time points (columns)
# @param pred: a matrix containing PREDICTED values for 10 sites (rows) across the 3 time points (columns)
# @param var: a single variance term (for now calculate from one month's data)
#
# @return: a loglikelihood value

vp_loglik_sites <- function(actual,pred,var,debug=FALSE){
  if(! all.equal(dim(pred),c(10,3))) stop("Prediction matrix must be 10 rows by 3 columns!")
  residuals <- c(actual[,1]-pred[,1],actual[,2]-pred[,2],actual[,3]-pred[,3])
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum(residuals^2)
  if(debug) print(residuals)
  return(ll)
}

#compiled version
require(compiler)
vp_loglik_sites_c <- cmpfun(vp_loglik_sites)