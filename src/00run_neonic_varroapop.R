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
  vrp_filename <- "default_stp_jeff.vrp"
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

#number of simulations 
Nsims <- 1

#weather file
#can be .dvf or .wth
#question for Bob
# i think it needs to agree with whatever the vrp file says?
vrp_weather <- "w93193-tempadj.dvf"
#vrp_weather <- "Midwest5Yr.wth"

#simulation start and end
#must have mm/dd/yyyy format
simstart <- "01/01/1988"
simend <- "12/31/1990"

##############################################################
###############################################################
#run everything
# define distributions for input parameters
source(paste(vpdir,"src/01parameterize_simulation.R",sep = ""))

#echo the first log file
scan(file = paste(vpdir_log, "log1.txt", sep=""), what = "raw")

# create and save input text files for simulations
source(paste(vpdir,"src/02write_input.R",sep = ""))

#may need to turn off virus checker!
# automate simulations for 'Nsims' number of simulations
source(paste(vpdir,"src/03simulate_w_exe.R",sep = ""))

# read text files and save results in 3d arrays
source(paste(vpdir,"src/04read_output.R",sep = ""))

# load input and output objects into environment
source(paste(vpdir,"src/05load_io.R",sep = ""))



##########################################################
# #git stuff - you only need to do this once
# #install
# https://help.github.com/desktop/guides/getting-started/installing-github-desktop/
# 
# # to clone this onto a machine with github installed
# #navigate in the github shell to a directory where you have read/write privileges
# #then from the directory above where you want to install the R source code:
# git clone https://github.com/puruckertom/beeRpop.git
# #####
# 
# #git stuff you have to do more often
# #switch into that directory
# cd beeRpop
# #check status (you should be )
# git status
# #check for changes on this and other branches
# git fetch
# #checkout a different branch
# git branch
# git checkout andrew
# 
# #after making some changes and you want to push those changes to the cloud
# git fetch
# git pull
# git commit -am "some explanatory message about what this commit was about"



