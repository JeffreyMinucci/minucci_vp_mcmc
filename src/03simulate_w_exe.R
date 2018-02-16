#run simulations###########
#command to run simulations in varroapop with relative directory structure and write output files



#CONTROL

# n<-inputtest[i]
inputfile<- "input_mcmc.txt"
outputfile<- paste("results",i,".txt", sep="")
logfile<- paste("log",i,".txt", sep="")
# username install, vrp reference for weather file has to point to vpdir2 directory
# vpdir2_command <- paste(vpdir2, vrp_filename, 
#                        " /b /or ", vpdir_out_control, outputfile, 
#                        " /i ", vpdir_in_control, inputfile, " /ol ", vpdir_log, logfile, sep="")
# print(paste(vpdir2_executable, vpdir2_command, sep=" "))
# system2(vpdir2_executable, vpdir2_command)
#git executable, vrp reference for weather file has to point to vpdir_exe directory
vpdir_command <- paste(vpdir_exe, vrp_filename, 
                       " /b /or ", vpdir_out_control, outputfile, 
                       " /i ", vpdir_in_control, inputfile, " /ol ", vpdir_log_control, logfile, sep="")
#print(paste(vpdir_executable, vpdir_command, sep=" "))
system2(vpdir_executable, vpdir_command)
