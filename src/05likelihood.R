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
  
}

