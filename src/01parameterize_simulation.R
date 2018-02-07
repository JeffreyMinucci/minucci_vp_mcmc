##
# Randomly initialize starting parameters
#
# Note: Creates a single row dataframe. We will fill in our subsequent markov steps by adding rows. 
#
# info on parameter priors/ranges:
# https://docs.google.com/spreadsheets/d/1SG1aceXAoWoxFPag52y09zaYtvnkYH4gXVp9LXj1Yxw/
#
##


# function to randomly generate an MCMC starting point - with uniform distribution
# parameters:

generate_vpstart <- function(static_names, static_vals, to_optimize, lower, upper, verbose=FALSE){
  if(!(length(to_optimize) == length(upper) & length(to_optimize)==length(lower))){
    stop("Vectors of parameter names and their bounds are not the same length!")
  }
  if(length(static_names) != length(static_vals)){
    stop("Vectors of static param names and their values are not the same length!")
  }
  values <- runif(length(to_optimize),lower,upper)
  inputdf <- data.frame(t(static_vals), t(values),stringsAsFactors = FALSE)
  colnames(inputdf) <- c(static_names,to_optimize)
  if(verbose){
    print("Starting point:")
    print(inputdf)
  } 
  return(inputdf)
}


