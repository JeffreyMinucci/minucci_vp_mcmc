#Unit testing for 03simulate_w_exe_parallel.R


context("Executing VP via wrapper (10 runs)")

test_that("running 10 runs of vp for basic case w/logs and no errors", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
    vp_vrp <- "testing_jeff.vrp"
  }
  vpdir_testin <- paste(vp_dir, "tests/input_test/", sep = "")
  vpdir_exepath <- paste(vp_dir, "bin/", sep = "")
  vpdir_testout <- paste(vp_dir, "tests/output_test/", sep = "")
  vpdir_testlogs <- paste(vp_dir, "tests/logs_test/", sep = "")
  vp_exe <- "VarroaPop.exe"
  vpdir_neonic_prof <- paste(vp_dir, "data/processed/neonic_profiles/",sep="")
  initial_dir <- paste(vp_dir,"data/raw/field_initial_conditions.csv",sep="")
  input_df_1 <- data.frame("SimEnd"=c("8/25/2015"))
  initial_conditions <- read.csv(initial_dir,stringsAsFactors=FALSE)
  write_vp_input_sites_c(input_df_1,vpdir_testin,initial_conditions, neonic_path = vpdir_neonic_prof)
  
  require(doParallel)
  if(Sys.info()[4]=="DZ2626UJMINUCCI") cores<-10 else cores<-detectCores-1
  if(cores == 1) registerDoSEQ() else{
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }
  expect_silent(run_vp_parallel_c(1,vpdir_exepath,vp_exe,vp_vrp,vpdir_testin,vpdir_testout,vpdir_testlogs,logs=T,debug=F))
  registerDoSEQ()
  
  expect_true(file.exists(paste(vpdir_testlogs,"log1_1.txt",sep="")))
  expect_true(file.exists(paste(vpdir_testlogs,"log1_10.txt",sep="")))
  expect_equal(file.size(paste(vpdir_testlogs,"log1_1.txt",sep="")),218)
  expect_equal(file.size(paste(vpdir_testlogs,"log1_10.txt",sep="")),218)
  
})

