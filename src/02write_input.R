
# function to create a single input file from a set of parameters in the form of
# a one row dataframe, where columns are named 

write_vp_input <- function(params){
  inputs <- paste(colnames(params),as.character(params[1,]),sep="=")
  write(inputs, file = paste(vpdir_in_control, "input_mcmc.txt", sep = ""), sep="")
  
}


