#Wrapper to run 10 sims of VP in parallel - one for each site, which is a unique combination of starting 
#parameters and neonic exposure
# 
# inputs:
#
# i = iteration number. For determing what to name results and logs. Useful if calling this function inside of a loop.
# exe_path = path to the folder which contains the .exe (e.g. 'D:/minucci_vp_mcmc/bin/)
# exe_name = full path to the .exe (e.g. 'D:/minucci_vp_mcmc/bin/VarroaPop.exe')
# vrp_name = name of the .vrp file (e.g. 'default_jeff.vrp')
# input_name = name of the input file (e.g. 'input_mcmc.txt')
# in_path = path to the input folder e.g. 'D:/minucci_vp_mcmc/input/control/
# out_path = path to the output folder e.g. 'D:/minucci_vp_mcmc/output/control/
# log_path = path to the log folder e.g. 'D:/minucci_vp_mcmc/log/control/ (can be omitted if logs=F)
# logs = T/F, should VP write a log file to the log_path folder?


run_vp_parallel <- function(i,exe_path, exe_name, vrp_name, in_path, out_path,log_path=NULL,logs=T,debug=F){
  if(debug) print(paste("Number of cores utilized:",getDoParWorkers()))
  foreach(j=1:10) %dopar% {
    # n<-inputtest[i]
    inputfile<- paste("input_mcmc_",j,".txt",sep="")
    outputfile<- paste("results",i,"_",j,".txt", sep="")
    logfile<- paste("log",i,"_",j,".txt", sep="")
    
    if(logs){
      vpdir_command <- paste(exe_path, vrp_name, 
                           " /b /or ", out_path, outputfile, 
                           " /i ", in_path, inputfile, " /ol ", log_path, logfile, sep="")
    } else {
      vpdir_command <- paste(exe_path, vrp_name, 
                             " /b /or ", out_path, outputfile, 
                             " /i ", in_path, inputfile, sep="")
    }

    #if(debug) print(paste(exe_name, vpdir_command, sep=" ")) #will not print parallel
    system2(exe_name, vpdir_command)
  }
}
