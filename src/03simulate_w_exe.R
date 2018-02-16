#run simulations###########
#wrapper code to run the VP exe
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
# iterate_inputs = T/F, should the iteration number be appended to the input filename with an underscore?
#                  Default = F. Use this if you are reading from a new input file each run through a loop, 
#                  instead of rewriting the same file.


run_vp<- function(i = 1,exe_path, exe_name, vrp_name, input_name, in_path, out_path,log_path=NULL,logs=T,debug=F,iterate_inputs){
  inputfile<- ifelse(iterate_inputs==T,paste(input_name,i,sep="_"),input_name)
  outputfile<- paste("results",i,".txt", sep="")
  logfile<- paste("log",i,".txt", sep="")
  if(logs){
    vpdir_command <- paste(exe_path, vrp_name, 
                           " /b /or ", out_path, outputfile, 
                           " /i ", in_path, inputfile, " /ol ", log_path, logfile, sep="")
  } else {
    vpdir_command <- paste(exe_path, vrp_name, 
                           " /b /or ", out_path, outputfile, 
                           " /i ", in_path, inputfile, sep="")
  }
  if(debug) print(paste(exe_name, vpdir_command, sep=" "))
  
  system2(vpdir_executable, vpdir_command)
}