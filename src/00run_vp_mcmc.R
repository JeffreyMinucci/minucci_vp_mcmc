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
# @param dir_structure Optional: a list containing:
#   $input path of input folder
#   $output path of output folder
#   $log path of log folder
#   $exe_folder path of executable folder
#   $exe_file name of the vp executable file 
#   $field_pops path to the field population data (to fit)
#   $field_initials path to the initial colony conditions
# @param static_vars A list containing:
#   $names A vector of the names of the parameters to set but keep static
#   $values A vector of the values of the parameters to set but keep static
# @param optimize_vars A list containing:
#   $names A vector of the names of the parameters to optimize via MCMC
#   $bound_l A vector of the lower bounds for the parameters to optimize
#   $bound_u A vector of the upper bounds for the parameters to optimize
#   $scales Optional - a vector of the 'scaling factors' for the parameters to optimize
#
# @return A list containing:
#   $param_trace A dataframe of VP inputs, with a row for each MCMC step
#   #like_trace A vector of likelihoods at each MCMC step
#   #accept_rate The fraction of proposals that were accepted

new_vp_mcmc <- function(vrp_filename = "default_jeff.vrp", nsims=20, step_length=.25, vp_dir, dir_structure=NULL, static_vars, 
                        optimize_vars, logs=T, verbose=T, debug=F){
  
  
  #load packages if needed
  require(doParallel)
  
  #parallel back end
  if(Sys.info()[4]=="DZ2626UJMINUCCI") cores<-10 else cores<-detectCores-1
  if(cores == 1) registerDoSEQ()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  on.exit(registerDoSEQ(),add=T)
  if(verbose) print(paste("Number of cores utilized:",getDoParWorkers()))
  
  #If directory structure is not given, use default
  if(is.null(dir_structure)){
    vpdir_in <- paste(vp_dir, "input/control/", sep = "")
    vpdir_out <- paste(vp_dir, "output/control/", sep = "")
    vpdir_log <- paste(vp_dir, "log/control/", sep = "")
    vpdir_exe <- paste(vp_dir, "bin/", sep = "")
    vp_binary <- "VarroaPop.exe"
    vp_field_data <- paste(vp_dir,"data/raw/field_bee_areas.csv",sep="")
    vp_field_initials <- paste(vp_dir,"data/raw/field_initial_conditions.csv",sep="")
    dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                         exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials)
  }
  
  #if scales of parameters to optimize is not given, set at 1/10th of range
  if(is.null(optimize_vars[["scales"]])) 
    optimize_vars[["scales"]] <- (optimize_vars[["bound_u"]]-optimize_vars[["bound_l"]])/10
  
  
  #load initial conditions
  initial_conditions <- read.csv(dir_structure[["field_initials"]])

  #load bee populations to fit
  field_data <- read.csv(dir_structure[["field_pops"]])
  bees_per_cm2 <- 1.45  #convert area of bees to individuals  
  bee_pops <- as.matrix(field_data[,c("bees_cm2_5","bees_cm2_6","bees_cm2_8")]) * bees_per_cm2 
  bee_initial <- field_data[,c("bees_cm2_4")] * bees_per_cm2
  #NOTE: need to consider hive that split
  #ballpark times: t1 = 5/21, t2 = 6/23, t4 = 8/18
  #NOTE: include exact dates when evaluating each site
  
  #load functions
  source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
  source(paste(vpdir,"src/02write_input.R",sep = "")) 
  source(paste(vpdir,"src/03simulate_w_exe_parallel.R",sep = ""))
  source(paste(vpdir,"src/04read_output.R",sep = ""))
  source(paste(vpdir,"src/05likelihood.R",sep=""))
  source(paste(vpdir,"src/06propose_mh_step.R",sep=""))
  
  
  ###   1) Randomly generate one set of parameters for the initial step
  i <- 1 #counter for results and log files
  inputdata <- generate_vpstart(static_vars[["names"]], static_vars[["values"]], #generates 1 row dataframe with starting parameter values
                                optimize_vars[["names"]], optimize_vars[["bound_l"]],
                                optimize_vars[["bound_u"]], verbose) 
  static_params <- inputdata[,!(colnames(inputdata) %in%  optimize_vars[["names"]])]
  
  
  ###   2) Write VP inputs
  write_vp_input_sites_c(params = inputdata[1,], in_path = dir_structure[["input"]],init_cond=initial_conditions)
  
  
  ###   3) Run VP simulation
  system.time(run_vp_parallel_c(i,dir_structure[["exe_folder"]],dir_structure[["exe_file"]],
                              vrp_filename, dir_structure[["input"]],
                              dir_structure[["output"]],dir_structure[["log"]],logs=logs,debug=debug))
  
  
  ###   4) Read outputs
  output_array <- read_output_c(i,dir_structure[["output"]])
  
  
  ###   5) Calculate likelihood of field data (colony size - adults) given these parameters
  var_est <- mean(c(var(bee_initial),var(bee_pops[,1]),var(bee_pops[,2]),var(bee_pops[,3]))) #for now get var from actual data
  like <- vp_loglik_sites_c(bee_pops,t(sapply(1:dim(output_array)[3],
                                            function(x) rowSums(output_array[c(24,56,112),c(2:4),x]))),var_est,debug=F)
  like_trace<- rep(0,nsims) #initialize vector to hold likelihood trace
  like_trace[1] <- like
  
  
  ###  Repeat nsims:
  ###  6) Generate new proposal parameters
  ###  7) Repeat steps 3-6 for proposal
  ###      Accept proposal as new state with probabliliy likelihood(proposal)/likelihood(current)
  ###      Return to 6
  
  accepts <- 0 #initialize accept tracker
  
  for(i in 2:nsims){
    print(paste("MCMC step: ",i-1," log-likelihood: ",like_trace[i-1]))
    proposal <- metropolis_proposal_c(inputdata[i-1,optimize_vars[["names"]]],optimize_vars[["scales"]],step_length)
    proposal_all <- cbind(static_params,proposal)
    write_vp_input_sites_c(proposal_all, dir_structure[["input"]], init_cond = initial_conditions)
    if(!(any(proposal > bound_u) | any((proposal < bound_l)))){
      run_vp_parallel_c(i,dir_structure[["exe_folder"]],dir_structure[["exe_file"]],
                      vrp_filename, dir_structure[["input"]],
                      dir_structure[["output"]],dir_structure[["log"]],logs=logs,debug=debug)
      output_array <- read_output_c(i,dir_structure[["output"]])
      like <- vp_loglik_sites_c(bee_pops,t(sapply(1:dim(output_array)[3],
                                                function(x) rowSums(output_array[c(24,56,112),c(2:4),x]))),var_est)
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
    }
  }
  if(verbose){
    print(paste("MCMC run completed. Final log-likelihood: ",like_trace[nsims]))
    print("Final optimized parameters:")
    print(inputdata[nsims,optimize_vars[["names"]]])
  }
  
  return(list(param_trace = inputdata, like_trace = like_trace, accept_rate = accepts/nsims))
  
}


#compiled version
require(compiler)
new_vp_mcmc_c <- cmpfun(new_vp_mcmc)


