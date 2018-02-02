#run simulations###########
#command to run simulations in varroapop with relative directory structure and write output files



#document the system status
#exe
print(paste(file.exists(vpdir_executable), ": executable file at", vpdir_executable))
#sample input
sample_input_file <- paste(vpdir_in_control, "input1.txt", sep="")
print(paste(file.exists(sample_input_file), ": sample input file at", sample_input_file))
#varroapop file
varroapop_file <- paste(vpdir_exe, vrp_filename, sep="")
print(paste(file.exists(varroapop_file), ": varroapop inputs file at", varroapop_file))
#weather file
weather_input <- paste(vpdir_weather, vrp_weather, sep="")
print(paste(file.exists(weather_input), ": weather file at", weather_input))
#varroapop <-> weather file message
print("the .vrp file has to know where the weather file is, R cannot tell it!")

#on epa windows symantec is returning a security threat every ~45 simulations
#the R process must be able to execute the varroapo0p process
#so a machine with elevated privileges R needs to be installed to a user-owned directory
#or disable symantec endpoint protection
# creating exceptions has no effect
# https://support.symantec.com/en_US/article.TECH173432.html
# https://submit.symantec.com/false_positive/


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
print(paste(vpdir_executable, vpdir_command, sep=" "))
system2(vpdir_executable, vpdir_command)

# 
# #NEONIC
# for (i in 1:Nsims) {
#   # n<-inputtest[i]
#   inputfile<- paste("input",i,".txt", sep="")
#   outputfile<- paste("results",i,".txt", sep="")
#   logfile<- paste("log",i,"exp.txt", sep="")
#   vpdir_command <- paste(vpdir_exe, vrp_filename, 
#                          " /b /or ", vpdir_out_neonic, outputfile, 
#                          " /i ", vpdir_in_neonic, inputfile, " /ol ", vpdir_log_neonic, logfile, sep="")
#   print(paste(vpdir_executable, vpdir_command, sep=" "))
#   system2(vpdir_executable, vpdir_command)
# }
