##
# Run VarroaPop simulations
# code by Jeff Minucci
#
# wrapper code to run the VP exe for MCMC analysis
#
#
# @param i: iteration number. For determing what to name results and logs. Useful if calling this function inside of a loop.
# @param exe_path: path to the folder which contains the .exe (e.g. 'D:/minucci_vp_mcmc/bin/)
# @param exe_name: name of the .exe (e.g. 'VarroaPop.exe')
# @param vrp_name: name of the .vrp file (e.g. 'default_jeff.vrp')
# @param input_name: name of the input file (e.g. 'input_mcmc.txt')
# @param in_path: path to the input folder e.g. 'D:/minucci_vp_mcmc/input/
# @param out_path: path to the output folder e.g. 'D:/minucci_vp_mcmc/output/vp_output/
# @param log_path: path to the log folder e.g. 'D:/minucci_vp_mcmc/log/ (can be omitted if logs=F)
# @param logs: T/F, should VP write a log file to the log_path folder?
# @param iterate_inputs: T/F, should the iteration number be appended to the input filename with an underscore?
#                  Default = F. Use this if you are reading from a new input file each run through a loop, 
#                  instead of rewriting the same file.
#
# @return: nothing - VP writes output to the out_path directory
##

run_vp<- function(i = 1,exe_path, exe_name, vrp_name, input_name, in_path, 
                  out_path,log_path=NULL,logs=T,debug=F,iterate_inputs=F){
  inputfile<- ifelse(iterate_inputs==T,paste(input_name,i,sep="_"),input_name)
  outputfile<- paste("results",i,".txt", sep="")
  logfile<- paste("log",i,".txt", sep="")
  vpdir_executable <- paste(exe_path,exe_name,sep="")
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


#compiled version
require(compiler)
run_vp_c <- cmpfun(run_vp)
