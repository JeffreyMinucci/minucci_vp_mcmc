######
# minucci_vp_mcmc
#
# author: Jeffrey Minucci
# 
# note: some VarroaPop wrapper code adapted from Carmen Kuan 
#
# Description: code to parameterize VarroaPop+Pesticide model using RARE pollen field data
#              and metropolis-hastings MCMC 
# 
######




##
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
#   Note: will want to do this for multiple starting positions and choose the best final likelihood?
##



## Check dependencies and versions
## 

list.of.packages <- c("plyr", "dplyr", "reshape2", "ggplot2", "grid", "gridExtra", "abind", 
                      "ppcor","doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

#load library dependencies
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(sensitivity)
library(abind)
library(dplyr)
library(doParallel)


#Determine path directory based on the user machine######
#jeff epa dev machine
if(Sys.info()[4]=="DZ2626UJMINUCCI"){
  vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "default_jeff.vrp"
}

#tom epa windows 2
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  vpdir<-path.expand("k:/git/minucci_vp_mcmc/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "default_tom.vrp" #will need to be generated from default_jeff.vrp with pointer to Tom's weather file location
}


#set subdirectories
vpdir_input <- paste(vpdir, "input/", sep = "")
vpdir_output <- paste(vpdir, "output/", sep = "")
vpdir_log_control <- paste(vpdir, "log/control/", sep = "")
vpdir_log_neonic <- paste(vpdir, "log/neonic/", sep = "")
vpdir_fig <- paste(vpdir, "reports/figures/", sep = "")
vpdir_exe <- paste(vpdir, "bin/", sep = "")
vpdir_in_control <- paste(vpdir_input, "control/", sep = "")
vpdir_out_control <- paste(vpdir_output, "control/", sep = "")
vpdir_weather <- paste(vpdir, "data/external/weather/", sep = "")

#path field data on bee population
vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")

#path field data on bee initial conditions
vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")

#varroapop executable version
vp_binary <- "VarroaPop.exe"
vpdir_executable <- paste(vpdir_exe, vp_binary, sep="")


#simulation start and end
#must have mm/dd/yyyy format
SimStart<- "4/30/1999"
SimEnd <- "8/25/1999"

#read field data
field_data <- read.csv(vp_field_data)
bees_per_cm2 <- 1  #convert area of bees to individuals  
bee_pops <- as.matrix(field_data[,c("bees_cm2_5","bees_cm2_6","bees_cm2_8")]) * bees_per_cm2 
bee_initial <- field_data[,c("bees_cm2_4")] * bees_per_cm2
#NOTE: need to consider hive that split
#ballpark times: t1 = 5/21, t2 = 6/23, t4 = 8/18
#NOTE: include exact dates when evaluating each site

#starting bee pop 
ICWorkerAdults = mean(bee_initial)

#static parameter list
static_names <- c("SimStart","SimEnd")
static_values <- c(SimStart,SimEnd)


##############################################################
###############################################################
#Run random walk Metropolis-Hastings MCMC


####   0) Initial settings

#parameters to optimize via MCMC
optimize_list <- c("ICQueenStrength","IPollenTrips","INectarTrips",
                   "ICForagerLifespan")
#   Notes: ICForagerLifespan appears to be converted to integer by removing decimal places in VP
bound_l <- c(1,4,4,4) #lower bondary of the domain for each parameter to be optimized
bound_u <- c(5,30,48,16) #upper bondary of the domain for each parameter to be optimized
scales <- (bound_u-bound_l)/10 #for now using the range divided by 10

#controls on MCMC algorithm
step_length <- .25 #ideal for 6 dimensions seems to be around .25? 
nsims <- 15000
verbose=T
debug=F

#parallel back end
if(Sys.info()[4]=="DZ2626UJMINUCCI") cores<-10 else cores<-3
cl <- makeCluster(cores)
registerDoParallel(cl)

#load functions
source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
source(paste(vpdir,"src/02write_input.R",sep = "")) 
source(paste(vpdir,"src/03simulate_w_exe_parallel.R",sep = "")) 
source(paste(vpdir,"src/05likelihood.R",sep=""))
source(paste(vpdir,"src/06propose_mh_step.R",sep=""))


###   1) Randomly generate one set of parameters for the initial step
i <- 1 #counter for results and log files
inputdata <- generate_vpstart(static_names, static_values, #generates 1 row dataframe with starting parameter values
                              optimize_list, bound_l, bound_u, verbose) 
static_params <- inputdata[,!(colnames(inputdata) %in% optimize_list)]


###   2) Write VP inputs
write_vp_input_sites(inputdata[1,])


###   3) Run VP simulation
system.time(run_vp_parallel(i,vpdir_exe,vpdir_executable,vrp_filename,vpdir_in_control,
                            vpdir_out_control,vpdir_log_control,logs=T,debug=T))


###   4) Read outputs
system.time(source(paste(vpdir,"src/04read_output.R",sep = "")))


###   5) Calculate likelihood of field data (colony size - adults) given these parameters
var_est <- mean(c(var(bee_initial),var(bee_pops[,1]),var(bee_pops[,2]),var(bee_pops[,3]))) #for now get var from actual data
like <- vp_loglik_sites(bee_pops,t(sapply(1:dim(tdarray_control)[3],
                                          function(x) rowSums(tdarray_control[c(24,56,112),c(2:4),x]))),var_est,debug=F)
like_trace<- rep(0,nsims) #initialize vector to hold likelihood trace
like_trace[1] <- like


###  Repeat nsims:
###  6) Generate new proposal parameters
###  7) Repeat steps 3-6 for proposal
###      Accept proposal as new state with probabliliy likelihood(proposal)/likelihood(current)
###      Return to 6


for(i in 2:nsims){
  print(paste("MCMC step: ",i-1," log-likelihood: ",like_trace[i-1]))
  proposal <- metropolis_proposal(inputdata[i-1,optimize_list],scales,step_length)
  proposal_all <- cbind(static_params,proposal)
  write_vp_input_sites(proposal_all)
  if(!(any(proposal > bound_u) | any((proposal < bound_l)))){
    system.time(run_vp_parallel(i,vpdir_exe,vpdir_executable,vrp_filename,vpdir_in_control,
                                vpdir_out_control,vpdir_log_control,logs=F,debug=F))
    source(paste(vpdir,"src/04read_output.R",sep = ""))    #read output into tdarray_control
    like <- vp_loglik_sites(bee_pops,t(sapply(1:dim(tdarray_control)[3],
                                              function(x) rowSums(tdarray_control[c(24,56,112),c(2:4),x]))),var_est)
    if(debug){
      print(paste("proposal: ",like))
      print(paste("current: ",like_trace[i-1]))
    }
    if(log(runif(1)) < (like-like_trace[i-1])){
      inputdata <- rbind(inputdata,proposal_all,make.row.names=F)
      like_trace[i] <- like
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
  print(inputdata[nsims,optimize_list])
}

stopCluster(cl)

#to save results of a run:
#write.csv(inputdata, file = paste(vpdir_out_control, "inputdata_final.csv", sep = ""))
#write.csv(like_trace, file = paste(vpdir_out_control, "likelihood_trace.csv", sep = ""))


##############################################################
###############################################################


#Note: move below to a post-processing script

accept_rate <- length(unique(like_trace[like_trace!=0]))/length(like_trace[like_trace!=0])
#hist(inputdata$ICForagerLifespan[3000:10000])
#hist(inputdata$ICQueenStrength[3000:10000])

library(MCMCvis)
MCMCtrace(as.matrix(inputdata[, optimize_list]),filename="test", wd=paste(vpdir,"reports/figures/",sep=""))
MCMCtrace(as.matrix(inputdata[, optimize_list]),filename="test_fulltrace",
          iter=15000,wd=paste(vpdir,"reports/figures/",sep=""))

