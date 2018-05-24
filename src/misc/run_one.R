#####
#
# Code to run a single set of parameters through the 10 RARE site scenarios
#
#####


#pseudocode:
#
# take in parameters and paths that won't change
# for i in 10
# do a RunVarroaPopLocal for each site
# return results in a 3d array

run_one <- function(parameters,
                             exe_file = system.file("varroapop_files","exe","VarroaPop.exe",package="VarroaPopWrapper"),
                             vrp_file = system.file("varroapop_files","exe","default.vrp",package="VarroaPopWrapper"),
                             in_path = paste(system.file("varroapop_files",package="VarroaPopWrapper"),"/",sep=""),
                             #in_filename = "vp_input.txt",
                             log_path = paste(system.file("varroapop_files",package="VarroaPopWrapper"),"/",sep=""),
                             out_path = paste(system.file("varroapop_files",package="VarroaPopWrapper"),"/",sep=""),
                             out_filename = "vp_results.txt",
                             save_files = FALSE,
                             logs = FALSE, verbose = FALSE){
  require(foreach)
  require(abind)
  require(VarroaPopWrapper) #install_github("quanted/VarroaPopWrapper",ref="master")
  source("D:/Git_files/minucci_vp_mcmc/src/02write_input.R")
  neonic_prof <- "d:/Git_files/minucci_vp_mcmc/data/processed/neonic_profiles/"
  field_initials <- "d:/Git_files/minucci_vp_mcmc/data/raw/field_initial_conditions.csv"
  initial_conditions <- read.csv(field_initials,stringsAsFactors=FALSE)
  write_vp_input_sites_c(parameters, in_path, initial_conditions, neonic_prof)
  to_return <- foreach(i=1:10) %do% {
    in_filename = paste("input_mcmc_",i,".txt",sep="")
    run_vp(exe_file, vrp_file, paste(in_path,in_filename,sep=""), out_path, out_filename, log_path, logs, verbose)
    result <- VarroaPopWrapper::read_output(out_path, out_filename)
    print(result)
    if(!save_files){
      if(logs){
        file.remove(paste(in_path,in_filename,sep=""), paste(log_path,"vp_log.txt",sep=""),
                    paste(out_path,out_filename,sep=""))
      }
      else{
        file.remove(paste(in_path,in_filename,sep=""),paste(out_path,out_filename,sep=""))
      }
      
    }
    result
  }
  return(to_return)
  
}