##
# Write VarroaPop Inputs
# code by Jeff Minucci
#
##


# function to create a single input file from a set of parameters in the form of
# a one row dataframe, where columns are named 
#
# @param params: vector of VarroaPop inputs to be written to .txt file
# @param in_path: directory to write input .txt file to
#
# @return: nothing - writes inputs to a .txt file in in_path for VP to read
#

write_vp_input <- function(params, in_path){
  inputs <- paste(colnames(params),as.character(params[1,]),sep="=")
  write(inputs, file = paste(in_path, "input_mcmc.txt", sep = ""), sep="")
  
}


# function to create 10 input files from a set of parameters (one for each site) in the form of
# a one row dataframe, where columns are named. Sites differ in initial hive conditions and neonic exposure
#
# @param params: vector of VarroaPop inputs to be written to .txt file 
# @in_path: directory to write input .txt file to (e.g. D:/minucci_vp_mcmc/input/)
# @init_cond: data frame of initial field values
# @data_path: path to data folder - to use custom neonic contam
#
# @return: nothing - writes inputs to a .txt file in in_path for VP to read
#

write_vp_input_sites <- function(params, in_path, init_cond, neonic_path){
  cm2_to_bees <- 1.45
  cm2_to_brood <- 3.43 #based on size of brood cells (.54 x .54 cm)
  cm2_to_larvae <- 3.43 *.66 #based on size of brood cells and time spend as larvae vs egg
  cm2_to_egg <- 3.43 *.33  #based on size of brood cells and time spend as larvae vs egg
  cm2_to_nectar <- .823 #based on volume of .54 x .54 x 1.25 cm cells and nectar density ~ 1.0 g/ml
  cm2_to_pollen <- 1.193 #based on volume of .54 x .54 x 1.25 cm cells and corn pollen density ~ 1.045 g/ml
  initials <- c("ICWorkerAdults","ICWorkerBrood","ICWorkerLarvae","ICWorkerEggs","InitColPollen","InitColNectar")
  initials_vals <- as.matrix(init_cond[,c(4,5,6,6,7,8)]) %*% diag(c(cm2_to_bees,cm2_to_brood,
                                                         cm2_to_larvae,cm2_to_egg,cm2_to_pollen,cm2_to_nectar))
  for(j in 1:10){
    inits <- paste(initials,initials_vals[j,],sep="=")
    firstDay <- as.character(as.Date(init_cond[j,9],format="%m/%d/%Y")+1,format="%m/%d/%Y")
    start <- paste("SimStart",firstDay,sep="=")
    #neonic_file <- paste("NecPolFileName=","neonic_profile_",j,".csv",sep="")
    #neonic_file <- gsub("/", "\\",paste("NecPolFileName=",neonic_path,"neonic_profile_test.csv",sep=""),fixed=T)
    neonic_file <- paste("NecPolFileName=",neonic_path,"neonic_profile_test.csv",sep="")
    neonic_enable <- "NecPolFileEnable=true"
    inputs <- c(inits,start,neonic_enable,neonic_file,paste(colnames(params),as.character(params[1,]),sep="="))
    write(inputs, file = paste(in_path, "input_mcmc_",j,".txt", sep = ""), sep="")
  }
}


#compiled version
require(compiler)
write_vp_input_sites_c <- cmpfun(write_vp_input_sites)

