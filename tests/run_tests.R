library('testthat')

source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
source(paste(vpdir,"src/02write_input.R",sep = "")) 
source(paste(vpdir,"src/03simulate_w_exe_parallel.R",sep = ""))
source(paste(vpdir,"src/04read_output.R",sep = ""))
source(paste(vpdir,"src/05likelihood.R",sep=""))
source(paste(vpdir,"src/06propose_mh_step.R",sep=""))

#test_dir('tests/', reporter = 'Summary')
test_dir('tests/')
