##
#  Code to calculate likelihood for a varroapop run given field data on colony population 
#
#  Author: Jeffrey Minucci
#
#
##

#load output files
#CONTROL
load(paste(vpdir_out_control,"tdarray_control.RData", sep = ""))
dim(tdarray_control)

#Create hypothetical field data (replace with real data when available)
adult_pop_month1 <- floor(rnorm(10,4500,1000))

#Model prediction - note this is total colony size
pred <- tdarray_control[30,1,1]
var_est <- var(adult_pop_month1) #for now get var from actual data


vp_loglik_simple <- function(actual,pred,var){
  ll <- (length(actual)/-2)*log(2*pi) - (length(actual)/2)*log(var)- (1/(2*var)) * sum((actual - pred)^2)
  
}


#test log likelihood function
like <- vp_loglik_simple(adult_pop_month1,pred,var_est)
