##
# Read VP results for MCMC analysis
# code by Jeff Minucci
#
#
# Function to read results from a set of 10 vp runs (one for each site/neonic profile)
# outputs a 3d array of results (dim 1 = day, dim 2 = response var, dim 3 = site)
#
# @param i: iteration number. For determing what MCMC step iteration to read results for. 
# @param out_path: directory to look for results files in (e.g. D:/minucci_vp_mcmc/output/control/)
# 
# @return: a dataframe of VP outputs (columns) by simulation date (rows) 
##

read_output <- function(i,out_path,debug=F){
  
  require(abind)
  Nsims <- 10 #number of results files (sites) per MCMC iteration
  outvar<- c("Date","Colony Size","Adult Drones","Adult Workers", "Foragers", "Capped Drone Brood", "Capped Worker Brood",
               "Drone Larvae", "Worker Larvae", "Drone Eggs", "Worker Eggs", "Free Mites", "Drone Brood Mites",
               "Worker Brood Mites", "Mites/Drone Cell", "Mites/Worker Cell", "Mites Dying", "Proportion Mites Dying",
               "Colony Pollen (g)", "Pollen Pesticide Concentration", "Colony Nectar", "Nectar Pesticide Concentration",
               "Dead Drone Larvae", "Dead Worker Larvae", "Dead Drone Adults", "Dead Worker Adults", "Dead Foragers",
               "Queen Strength", "Average Temperature (celsius)", "Rain")
  
  
  # read output files
  
  outputs <- array(data=NA, c(nrows,ncols-1,Nsims))
  for (j in 1:Nsims) {
    df <- read.table(paste(out_path,"results",i,"_",j,".txt", sep=""), header= FALSE, sep= "", 
                     skip = 6, stringsAsFactors = FALSE, row.names=NULL, col.names = outvar)
    newarray <- df[,2:ncol(df)]
    outputs[1:nrow(df),1:(ncol(df)-1),j] <- abind(newarray[1:nrow(df),1:(ncol(df)-1)], along=3)
  }
  return(outputs)
  
}

#compiled version
require(compiler)
read_output_c <- cmpfun(read_output)


# 
# # load up varroapop data into a 3d array dataframe ######
# 
# 
# Nsims <- 10 #number of results files (sites) per MCMC iteration
# if(i==1){
#   df <- read.table(paste(vpdir_out_control,"results",i,".txt", sep=""), header= FALSE, sep= "", 
#                    skip = 6, stringsAsFactors = FALSE, row.names=NULL)
#   dim(df)
#   nrows <- dim(df)[[1]] #this is dependent on the duration of the simulation as set in the comparison.vrp file
#   ncols <- dim(df)[[2]]
#   
#   timearray<- array(data=NA, c(nrows))
#   timearray[2:nrows]<- df[2:nrows,1]
#   timearray<- as.Date(timearray,"%m/%d/%Y")
#   timediff <- timearray[3]-timearray[2]
#   timearray[1] <- timearray[2]-timediff
#   length(timearray)
#   ncols
#   outvar<- c("Date","Colony Size","Adult Drones","Adult Workers", "Foragers", "Capped Drone Brood", "Capped Worker Brood",
#              "Drone Larvae", "Worker Larvae", "Drone Eggs", "Worker Eggs", "Free Mites", "Drone Brood Mites",
#              "Worker Brood Mites", "Mites/Drone Cell", "Mites/Worker Cell", "Mites Dying", "Proportion Mites Dying",
#              "Colony Pollen (g)", "Pollen Pesticide Concentration", "Colony Nectar", "Nectar Pesticide Concentration",
#              "Dead Drone Larvae", "Dead Worker Larvae", "Dead Drone Adults", "Dead Worker Adults", "Dead Foragers",
#              "Queen Strength", "Average Temperature (celsius)", "Rain")
#   length(outvar)
# }
# 
# # read output files
# 
# tdarray_control <- array(data=NA, c(nrows,ncols-1,Nsims))
# dim(tdarray_control)
# for (j in 1:Nsims) {
#   df <- read.table(paste(vpdir_out_control,"results",i,"_",j,".txt", sep=""), header= FALSE, sep= "", 
#                   skip = 6, stringsAsFactors = FALSE, row.names=NULL, col.names = outvar)
#   newarray <- df[,2:ncols]
#   tdarray_control[1:nrows,1:(ncols-1),j] <- abind(newarray[1:nrows,1:(ncols-1)], along=3)
# }
# #print(tdarray_control)
# 
# #save(tdarray_control, file = paste(vpdir_out_control,"tdarray_control.RData", se = ""))
# #rm(tdarray_control)
# 
# if(i==1) save(timearray,file = paste(vpdir_output,"timearray.RData", sep = ""))
