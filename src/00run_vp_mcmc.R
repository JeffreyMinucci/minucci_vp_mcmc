######
# minucci_vp_mcmc
#
# author: Jeffrey Minucci
# 
# VarroaPop wrapper code adapted from C. Kuan and K. Garber*
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



# install current version of R

# install current version of RStudio

#check to make sure required packages are installed
list.of.packages <- c("plyr", "dplyr", "reshape2", "ggplot2", "grid", "gridExtra", "sensitivity", "abind", 
                      "ppcor")
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

#echo environment
Sys.info()
Sys.info()[4]
.Platform
version

#Determine path directory based on the user machine######
#jeff epa dev machine
if(Sys.info()[4]=="DZ2626UJMINUCCI"){
  vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "default_jeff.vrp"
}

#tom epa windows 2
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  vpdir<-path.expand("k:/git/garber_vp/")
  vpdir2<-path.expand("k:/git/garber_vp/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "comparison_stp_epa_windows_kdrive_garber.vrp"
}
#tom mac air
if(Sys.info()[4]=="stp-air"){
  vpdir<-path.expand("~/git/garber_vp/")
}
#andrew epa
if(Sys.info()[4]=="LZ2032EAKANAREK"){
  vpdir <- path.expand("C:/Users/AKanarek/Documents/GitHub/garber_vp/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "comparison.vrp"
} 
#kris epa
if(Sys.info()[4]==""){
  vpdir <- path.expand("C:/Users/AKanarek/Documents/GitHub/garber_vp/")
  # varroapop file (without directory, the file needs to be in vpdir_exe above)
  vrp_filename <- "comparison.vrp"
} 




#subdirectories
vpdir_input <- paste(vpdir, "input/", sep = "")
vpdir_output <- paste(vpdir, "output/", sep = "")
vpdir_log_control <- paste(vpdir, "log/control/", sep = "")
vpdir_log_neonic <- paste(vpdir, "log/neonic/", sep = "")
vpdir_fig <- paste(vpdir, "reports/figures/", sep = "")
vpdir_exe <- paste(vpdir, "bin/", sep = "")
#vpdir_io <- paste(vpdir, "io/", sep = "")
vpdir_in_control <- paste(vpdir_input, "control/", sep = "")
vpdir_in_neonic <- paste(vpdir_input, "neonic/", sep = "")
vpdir_out_control <- paste(vpdir_output, "control/", sep = "")
vpdir_out_neonic <- paste(vpdir_output, "neonic/", sep = "")
vpdir_weather <- paste(vpdir, "data/external/weather/", sep = "")

#varroapop executable version
vp_binary <- "VarroaPop.exe"
vpdir_executable <- paste(vpdir_exe, vp_binary, sep="")
#vpdir2_executable <- paste(vpdir2, vp_binary, sep="")

#number of vp runs per iteration (will be 1 until multiple pesticide load scenarios are implemented) 
Nsims <- 1

#weather file - not sure if this variable is doing anything 
WeatherFileName <- "Midwest5Yr.wth"

#simulation start and end
#must have mm/dd/yyyy format
SimStart<- "5/20/1999"
SimEnd <- "6/29/1999"




##############################################################
###############################################################
#Run random walk Metropolis-Hastings MCMC

#   0) Initial settings
optimize_list <- c("ICQueenStrength","RQWkrDrnRatio","ICDroneMiteSurvivorship","ICWorkerMiteSurvivorship",
                   "ICForagerLifespan","InitColNectar","InitColPollen","RQQueenStrength")
bound_l <- c(1,1,0,0,4,0,0,1) #lower bondary of the domain for each parameter to be optimized
bound_u <- c(5,5,100,100,16,8000,8000) #upper bondary of the domain for each parameter to be optimized
scales <- (bound_u-bound_l)/10 #for now using the range divided by 10


nsims <- 500
i <- 1 #counter for results and log files

#   1) Randomly generate one set of parameters for the initial step
source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) #generates inputdata_control, dataframe with parameter values

static_params <- inputdata_control[,!(colnames(inputdata_control) %in% optimize_list)]

#   2) Write VP inputs
source(paste(vpdir,"src/02write_input.R",sep = "")) #load write input function
write_vp_input(inputdata_control[1,])


#   3) Run VP simulation
system.time(source(paste(vpdir,"src/03simulate_w_exe.R",sep = "")))

#   4) Read outputs
system.time(source(paste(vpdir,"src/04read_output.R",sep = "")))


#   5) Calculate likelihood of field data (colony size - adults) given these parameters
source(paste(vpdir,"src/05likelihood.R",sep="")) #creates var "like" which holds the likelihood
like_trace<- rep(0,nsims)
like_trace[1] <- like


#   Repeat Nsims:
#   6) Generate new proposal parameters
#   7) Repeat steps 3-6 for proposal
#      Accept proposal as new state with probabliliy likelihood(proposal)/likelihood(current)
#      Return to 6


#load functions
source(paste(vpdir,"src/06propose_mh_step.R",sep=""))

for(i in 2:nsims){
  proposal <- metropolis_proposal(inputdata_control[i-1,optimize_list],scales,.1)
  proposal_all <- cbind(static_params,proposal)
  write_vp_input(proposal_all)
  if(!(any(proposal > bound_u) | any((proposal < bound_l)))){
    
    source(paste(vpdir,"src/03simulate_w_exe.R",sep = "")) #run sim for proposal  
    #note: need to check for out of range parameters (before .exe run?)
    
    source(paste(vpdir,"src/05likelihood.R",sep="")) #creates var "like" which holds the likelihood
    if(log(runif(1)) > (like_trace[i-1] - like)){
      inputdata_control <- rbind(inputdata_control,proposal_all,make.row.names=F)
      like_trace[i] <- like
    }
    else{
      inputdata_control <- rbind(inputdata_control,inputdata_control[i-1,],make.row.names=F)
      like_trace[i] <- like_trace[i-1]
    }
  }
  else{
    print("Proposal out of bounds!")
    inputdata_control <- rbind(inputdata_control,inputdata_control[i-1,],make.row.names=F)
    like_trace[i] <- like_trace[i-1]
  }
}


##############################################################
###############################################################









##############################################################
###############################################################
#run everything
# define distributions for input parameters
source(paste(vpdir,"src/01parameterize_simulation.R",sep = ""))

#echo the first log file
#scan(file = paste(vpdir_log, "log1.txt", sep=""), what = "raw")

# create and save input text files for simulations
source(paste(vpdir,"src/02write_input.R",sep = ""))

#may need to turn off virus checker!
# automate simulations for 'Nsims' number of simulations
source(paste(vpdir,"src/03simulate_w_exe.R",sep = ""))

# read text files and save results in 3d arrays
source(paste(vpdir,"src/04read_output.R",sep = ""))

# load input and output objects into environment
#source(paste(vpdir,"src/05load_io.R",sep = ""))

# calculate loglikelihood of this parameter combination
source(paste(vpdir,"src/05likelihood.R",sep=""))




