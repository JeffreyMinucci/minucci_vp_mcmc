#Runs 10 sims of VP in parallel - one for each site, which is a unique combination of starting 
#parameters and neonic exposure

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
