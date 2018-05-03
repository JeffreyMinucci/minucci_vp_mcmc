######
# Function to run a new VarroaPop Metropolis MCMC analysis with rare pollen field data
#
# author: Jeffrey Minucci
# 
#
#   Metropolis MCMC Program outline
#
#   0) Load packages, initialize file and directory paths
#   1) Randomly generate one set of parameters for the initial step
#   2) Write VP inputs
#   3) Run VP simulation
#   4) Read outputs
#   5) Calculate likelihood of field data (colony size - adults) given these parameters
#   
#   Repeat Nsims:
#   6) Generate new proposal parameters
#   7) Repeat steps 3-6 for proposal
#   8) Accept proposal as new state with probabliliy likelihood(proposal)/likelihood(current)
#   
#   @return: a list containing (1) a data frame of parameter values at each MCMC step, 
#           (2) a vector of the likelihood trace, and (3) the acceptance rate
##


# 
# @param nsims 
# @param param step_length
# @param vp_dir Gives the directory of the project
# @param dir_structure Optional - a list containing:
#   $input path of input folder
#   $output path of output folder
#   $log path of log folder
#   $exe_folder path of executable folder
#   $exe_file name of the vp executable file 
#   $field_pops path to the field population data (to fit)
#   $field_initials path to the initial colony conditions
#   $weather path to folder containing weather file
#   $neonic_profiles path to folder containing neonic profiles
#
# @param static_vars A list containing:
#   $names A vector of the names of the parameters to set but keep static
#   $values A vector of the values of the parameters to set but keep static
# @param optimize_vars A list containing:
#   $names A vector of the names of the parameters to optimize via MCMC
#   $bound_l A vector of the lower bounds for the parameters to optimize
#   $bound_u A vector of the upper bounds for the parameters to optimize
#   $scales Optional - a vector of the 'scaling factors' for the parameters to optimize
# @param start_point - Optional - 1 row dataframe of inputs (from a previous run)
#
# @return A list containing:
#   $param_trace A dataframe of VP inputs, with a row for each MCMC step
#   $like_trace A vector of likelihoods at each MCMC step
#   $accept_rate The fraction of proposals that were accepted
#   $mcmc_params A list of the params passed to the function
#   $prop_out_trace A 10 x 3 x iteration array giving each proposals predictions for the 10 sites by 3 dates
#     where out of bounds proposals have arrays of NA
#   #bounds_rate The fraction of proposals that were out of bounds

new_vp_mcmc <- function(vrp_filename = "default_jeff.vrp", nsims=20, step_length=.25, vp_dir, dir_structure=NULL, static_vars, 
                        optimize_vars, parallel=T, start_point = NULL, logs=T, verbose=T, debug=F){
  
  
  #load packages if needed
  require(doParallel)
  
  #parallel back end
  if(parallel){
    if(Sys.info()[4]=="DZ2626UJMINUCCI") cores<-10 else cores<-detectCores-1
    if(cores == 1) registerDoSEQ() else{
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      on.exit(registerDoSEQ(),add=T)
    }
  } else registerDoSEQ()
  if(verbose) print(paste("Number of cores utilized:",getDoParWorkers()))
  
  #If directory structure is not given, use default
  if(is.null(dir_structure)){
    vpdir_in <- paste(vp_dir, "input/", sep = "")
    vpdir_out <- paste(vp_dir, "output/vp_output/", sep = "")
    vpdir_log <- paste(vp_dir, "log/", sep = "")
    vpdir_exe <- paste(vp_dir, "bin/", sep = "")
    vp_binary <- "VarroaPop.exe"
    vp_field_data <- paste(vp_dir,"data/raw/field_bee_areas.csv",sep="")
    vp_field_initials <- paste(vp_dir,"data/raw/field_initial_conditions.csv",sep="")
    vpdir_weather <- paste(vp_dir, "data/external/weather/",sep="")
    vpdir_neonic_prof <- paste(vp_dir, "data/processed/neonic_profiles/", sep="")
    dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                         exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                         weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)
  }
  
  #if scales of parameters to optimize is not given, set at 1/10th of range
  if(is.null(optimize_vars[["scales"]])) 
    optimize_vars[["scales"]] <- (optimize_vars[["bound_u"]]-optimize_vars[["bound_l"]])/10
  
  
  #load initial conditions
  initial_conditions <- read.csv(dir_structure[["field_initials"]],stringsAsFactors=FALSE)

  #load bee populations to fit
  field_data <- read.csv(dir_structure[["field_pops"]])
  bees_per_cm2 <- 1.45  #convert area of bees to individuals  
  bee_pops <- as.matrix(field_data[,c("bees_cm2_5","bees_cm2_6","bees_cm2_8")]) * bees_per_cm2 
  bee_initial <- field_data[,c("bees_cm2_4")] * bees_per_cm2
  #NOTE: need to consider hive that split
  
  #dates to sample populations for each site
  dates <- field_data[,c("date_5","date_6","date_8")]
  days_sampled <- sapply(dates, function(x) as.Date(x,format="%m/%d/%Y") - as.Date(field_data[,"date_4"],format="%m/%d/%Y")) 

  
  #load functions
  source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
  source(paste(vpdir,"src/02write_input.R",sep = "")) 
  source(paste(vpdir,"src/03simulate_w_exe_parallel.R",sep = ""))
  source(paste(vpdir,"src/04read_output.R",sep = ""))
  source(paste(vpdir,"src/05likelihood.R",sep=""))
  source(paste(vpdir,"src/06propose_mh_step.R",sep=""))
  
  
  ###   1) Randomly generate one set of parameters for the initial step OR use end of previous run (if given)
  i <- 1 #counter for results and log files
  if(is.null(start_point)){
    inputdata <- generate_vpstart(static_vars[["names"]], static_vars[["values"]], #generates 1 row dataframe with starting parameter values
                                 optimize_vars[["names"]], optimize_vars[["bound_l"]],
                                 optimize_vars[["bound_u"]], verbose) 
  } else inputdata <- start_point
  static_params <- as.data.frame(inputdata[,!(colnames(inputdata) %in%  optimize_vars[["names"]])],stringsAsFactors=F)
  colnames(static_params) <- colnames(inputdata)[!(colnames(inputdata) %in%  optimize_vars[["names"]])]

  ###   2) Write VP inputs
  write_vp_input_sites_c(params = inputdata[1,colnames(inputdata) != "sd"], in_path = dir_structure[["input"]],init_cond=initial_conditions,
                         neonic_path = dir_structure[["neonic_profiles"]])
  
  
  ###   3) Run VP simulation
  system.time(run_vp_parallel_c(i,dir_structure[["exe_folder"]],dir_structure[["exe_file"]],
                              vrp_filename, dir_structure[["input"]],
                              dir_structure[["output"]],dir_structure[["log"]],logs=logs,debug=debug))
  
  
  ###   4) Read outputs
  #output_list = vector("list",nsims)
  output_trace = array(rep(NA, 10*3*nsims),c(10,3,nsims))
  output_array <- read_output_c(i,dir_structure[["output"]])
  #output_list[i] <- output_array

  ###   5) Calculate likelihood of field data (colony size - adults) given these parameters
  if("sd" %in% colnames(inputdata)){
    sd_est = inputdata[1,"sd"]
  }
  else{
    sd_est <- mean(c(sd(bee_initial),sd(bee_pops[,1]),sd(bee_pops[,2]),sd(bee_pops[,3]))) #for now get var from actual data
  }
  #print(sd_est)
  pop_est <- t(sapply(1:(dim(output_array)[3]),function(x, site) rowSums(output_array[days_sampled[x,]+1,c(2:4),x])))
  #print(pop_est)
  like <- vp_loglik_sites_c(bee_pops,pop_est,sd_est^2,debug=F)
  like_trace<- rep(0,nsims) #initialize vector to hold likelihood trace
  like_trace[1] <- like
  output_trace[,,i] <- pop_est
  
  
  ###  Repeat nsims:
  ###  6) Generate new proposal parameters
  ###  7) Repeat steps 3-6 for proposal
  ###      Accept proposal as new state with probabliliy likelihood(proposal)/likelihood(current)
  ###      Return to 6
  
  accepts <- 0 #initialize accept tracker
  bounds <- 0 #initial out of bounds tracker
  
  for(i in 2:nsims){
    if(verbose) print(paste("MCMC step: ",i-1," log-likelihood: ",like_trace[i-1]))
    proposal <- metropolis_proposal_c(inputdata[i-1,optimize_vars[["names"]]],optimize_vars[["scales"]],step_length)
    proposal_all <- cbind(static_params,proposal)
    if(debug) print(proposal_all)
    if(length(optimize_vars[["names"]])==1) colnames(proposal_all)[length(static_vars[["names"]])+1] <- optimize_vars[["names"]]
    if(!(any(proposal > bound_u) | any((proposal < bound_l)))){
      write_vp_input_sites_c(proposal_all[names(inputdata) != "sd"], dir_structure[["input"]], init_cond = initial_conditions,
                             neonic_path = dir_structure[["neonic_profiles"]])
      run_vp_parallel_c(i,dir_structure[["exe_folder"]],dir_structure[["exe_file"]],
                      vrp_filename, dir_structure[["input"]],
                      dir_structure[["output"]],dir_structure[["log"]],logs=logs,debug=debug)
      output_array <- read_output_c(i,dir_structure[["output"]])
      #print(output_array)
      if("sd" %in% colnames(proposal_all)){
        sd_est = proposal_all[1,"sd"]
        if(debug) print(paste("Proposed sd =", sd_est))
      }
      pop_est <- t(sapply(1:(dim(output_array)[3]),function(x, site) rowSums(output_array[days_sampled[x,]+1,c(2:4),x])))
      like <- vp_loglik_sites_c(bee_pops,pop_est,sd_est^2,debug=F)
      output_trace[,,i] <- pop_est
      
      if(debug){
        print(paste("proposal: ",like))
        print(paste("current: ",like_trace[i-1]))
      }
      if(log(runif(1)) < (like-like_trace[i-1])){
        inputdata <- rbind(inputdata,proposal_all,make.row.names=F)
        like_trace[i] <- like
        accepts <- accepts + 1
      }
      else{
        if(verbose) print(paste("Rejecting log-likelihood: ",like))
        inputdata <- rbind(inputdata,inputdata[i-1,],make.row.names=F)
        like_trace[i] <- like_trace[i-1]
      }
    }
    else{
      if(verbose) print("Proposal out of bounds!")
      inputdata <- rbind(inputdata,inputdata[i-1,],make.row.names=F)
      like_trace[i] <- like_trace[i-1]
      bounds <- bounds + 1
    }
  }
  if(verbose){
    print(paste("MCMC run completed. Final log-likelihood: ",like_trace[nsims]))
    print("Final optimized parameters:")
    print(inputdata[nsims,optimize_vars[["names"]]])
  }
  
  mcmc_params <- list(vrp_filename=vrp_filename, nsims=nsims, step_length=step_length, vp_dir=vp_dir, 
                     dir_structure=dir_structure, static_vars = static_vars, 
                     optimize_vars= optimize_vars, logs=logs, verbose=verbose, debug=debug)
  toReturn <- list(param_trace = inputdata, like_trace = like_trace, accept_rate = accepts/nsims, mcmc_params = mcmc_params,
                   prop_out_trace = output_trace, bounds_rate = bounds/nsims)
  class(toReturn) <- "vp_mcmc_run"
  return(toReturn)
  
}


#compiled version
require(compiler)
new_vp_mcmc_c <- cmpfun(new_vp_mcmc)





# Continue a vp_mcmc run
#
# Function to continue a vp_mcmc run from an existing "new_vp_mcmc" function output object
#
# @param nsims Number of additional simulations to run
# @param step_length Optional - new step length?
# @param old_vp_mcmc Output of the previous vp_mcmc run

continue_vp_mcmc <- function(nsims, old_vp_mcmc, step_length=NULL){
  
  #read info from the previous vp_mcmc run
  orig_params <- old_vp_mcmc$mcmc_params
  orig_param_trace <- old_vp_mcmc$param_trace[complete.cases(old_vp_mcmc$param_trace),]
  orig_nsims <- nrow(orig_param_trace)
  orig_like_trace <- old_vp_mcmc$like_trace[1:orig_nsims]
  orig_prop_out_trace <- old_vp_mcmc$prop_out_trace[,,1:orig_nsims]
  orig_accept_rate <- old_vp_mcmc$accept_rate
  orig_bounds_rate <- old_vp_mcmc$bounds_rate
  
  #modify previous parameters for new run
  new_params <- orig_params
  new_params$nsims <- nsims + 1 #nsims = additonal simulations on top of existing
  if(!is.null(step_length)) new_params$step_length <- step_length
  new_params[["start_point"]] <- orig_param_trace[orig_nsims,]
  
  #run new_vp_mcmc from end of last run
  new_results <- do.call(new_vp_mcmc_c,args=new_params)
  
  #add new results to old results and output vp_mcmc object
  new_results$param_trace <- rbind(orig_param_trace,new_results$param_trace[-1,])
  row.names(new_results$param_trace) <- c(1:nrow(new_results$param_trace))
  new_results$like_trace <- c(orig_like_trace, new_results$like_trace[-1])
  new_results$accept_rate <- orig_accept_rate*(orig_nsims/(orig_nsims+nsims)) + new_results$accept_rate*(nsims/(orig_nsims+nsims))
  new_results$mcmc_params$nsims <- length(new_results$like_trace)
  new_results$mcmc_params$step_length <- new_params$step_length
  new_results$prop_out_trace <- abind(orig_prop_out_trace, new_results$prop_out_trace[,,-1], along=3)
  new_results$bounds_rate <- ifelse(!is.null(orig_bounds_rate), #TODO: remove this backwards compatibility after 1 run
                                    orig_bounds_rate*(orig_nsims/(orig_nsims+nsims)) + new_results$bounds_rate*(nsims/(orig_nsims+nsims)),
                                    new_results$bounds_rate)
  
  return(new_results)
}


#compiled version

continue_vp_mcmc_c <- cmpfun(continue_vp_mcmc)

