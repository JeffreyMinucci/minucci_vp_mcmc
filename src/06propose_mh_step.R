##
#  Function to generate a proposal for the next MCMC step - for given parameters
#
#  Code by: Jeffrey Minucci
#
#
##

# metropolis_proposal function
#
# generates a proposed next step in the markov chain using a proposal function. 
# currently the proposal function is a gaussian dist. for all dimensions but with variance
# that varies based on the scale of each parameter (i.e. smaller steps for parameters that have small ranges or values)
#
# takes parameters:
#     @param current: vector of the current parameter values (to be optimized)
#     @param scales: vector that expresses the variance for each parameter
#     @param step: a single number representing the step length tuning parameter
# @return:
#     a vector of parameters that corresponds to a proposed jump in parameter space

require(compiler)

metropolis_proposal <- function(current,scales,step){
  new <- rep(0,length(current))
  new <- current + (rnorm(length(current),0,1) * scales * step)
  return(new)
}

#compiled version
metropolis_proposal_c <- cmpfun(metropolis_proposal)