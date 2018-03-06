library('testthat')

if(Sys.info()[4]=="DZ2626UJMINUCCI"){
  vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
  vrp_filename <- "default_jeff.vrp"
}

#tom epa windows 2
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  vpdir<-path.expand("k:/git/minucci_vp_mcmc/")
  vrp_filename <- "default_tom.vrp" #will need to be generated from default_jeff.vrp with pointer to Tom's weather file location
}

#load functions
source(paste(vpdir,"src/00run_vp_mcmc.R",sep = "")) 
source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
source(paste(vpdir,"src/02write_input.R",sep = "")) 
source(paste(vpdir,"src/03simulate_w_exe_parallel.R",sep = ""))
source(paste(vpdir,"src/04read_output.R",sep = ""))
source(paste(vpdir,"src/05likelihood.R",sep=""))
source(paste(vpdir,"src/06propose_mh_step.R",sep=""))

#run tests


#run tests and write output to unit_testing.md
write(paste("Tests run on:",Sys.time()), file = paste(vpdir, "unit_testing.md", sep = ""))
tests <- capture.output(test_dir('tests/', reporter='summary'))
write(paste(tests,sep="\n"), file = paste(vpdir, "unit_testing.md", sep = ""), append=T)

