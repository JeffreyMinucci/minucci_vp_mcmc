#Unit testing for 00run_vp_mcmc.R


context("Running vp_mcmc full algorithm")

test_that("returns full vp_mcmc object with valid results", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
    vrp_filename <- "testing_jeff.vrp"
  }
  
  vpdir_in <- paste(vpdir, "tests/input_test/", sep = "")
  vpdir_out <- paste(vpdir, "tests/output_test/", sep = "")
  vpdir_log <- paste(vpdir, "tests/logs_test/", sep = "")
  vpdir_exe <- paste(vpdir, "bin/", sep = "")
  vp_binary <- "VarroaPop.exe"
  vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")
  vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")
  vpdir_neonic_prof <- paste(vpdir, "data/processed/neonic_profiles/",sep="")
  vpdir_weather <- paste(vpdir, "data/external/weather/",sep="")
  dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                       exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                       weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)
  SimEnd <- "8/25/2015"
  koc <- 60
  kow <- 2
  
  static_names <- c("SimEnd","koc","kow")
  static_values <- c(SimEnd,koc,kow)
  static_list <- list(names = static_names, values = static_values)
  
  optimize_names <- c("ICQueenStrength","IPollenTrips","INectarTrips",
                      "ICForagerLifespan")
  bound_l <- c(1,4,4,4) #lower bondary of the domain for each parameter to be optimized
  bound_u <- c(5,30,48,16) #upper bondary of the domain for each parameter to be optimized
  optimize_list <- list(names = optimize_names, bound_l = bound_l, bound_u = bound_u)
  results <- new_vp_mcmc_c(vrp_filename = vrp_filename, nsims = 5, step_length = .5, vp_dir=vpdir, dir_structure = dir_structure, static_vars = static_list, 
                                optimize_vars = optimize_list, logs = T, verbose = F)
  expect_true(class(results) == "vp_mcmc_run")
  expect_equal(dim(results$param_trace),c(5,length(static_names)+length(optimize_names)))
  expect_equal(length(results$like_trace),nrow(results$param_trace))
  expect_true(is.double(results$accept_rate))
  expect_true(results$accept_rate <= 1 & results$accept_rate >=0)
  expect_true(!anyNA(results$param_trace))
  
  #check that pesticide contamination is showing up in first month
  vpdir_test_results <- paste(vp_dir, "tests/output_test/", sep = "")
  results <- read_output_c(1,vpdir_test_results)
  expect_true(any(results[1:30,21,1] > 0))  #nectar
  expect_true(any(results[1:30,21,10] > 0))
  expect_true(any(results[1:30,19,1] > 0)) #pollen
  expect_true(any(results[1:30,19,10] > 0))
  
})


test_that("can run with only one static var and one optimize var", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
    vrp_filename <- "testing_jeff.vrp"
  }
  
  vpdir_in <- paste(vpdir, "tests/input_test/", sep = "")
  vpdir_out <- paste(vpdir, "tests/output_test/", sep = "")
  vpdir_log <- paste(vpdir, "tests/logs_test/", sep = "")
  vpdir_exe <- paste(vpdir, "bin/", sep = "")
  vp_binary <- "VarroaPop.exe"
  vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")
  vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")
  vpdir_neonic_prof <- paste(vpdir, "data/processed/neonic_profiles/",sep="")
  vpdir_weather <- paste(vpdir, "data/external/weather/",sep="")
  dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                       exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                       weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)
  SimEnd <- "8/25/2015"
  koc <- 60
  kow <- 2
  
  static_names <- c("SimEnd")
  static_values <- c(SimEnd)
  static_list <- list(names = static_names, values = static_values)
  
  optimize_names <- c("ICQueenStrength")
  bound_l <- c(1) #lower bondary of the domain for each parameter to be optimized
  bound_u <- c(5) #upper bondary of the domain for each parameter to be optimized
  optimize_list <- list(names = optimize_names, bound_l = bound_l, bound_u = bound_u)
  results <- new_vp_mcmc_c(vrp_filename = vrp_filename, nsims = 5, step_length = .5, vp_dir=vpdir, dir_structure=dir_structure,static_vars = static_list, 
                           optimize_vars = optimize_list, logs = F, verbose = F)
  expect_true(class(results) == "vp_mcmc_run")
  expect_equal(dim(results$param_trace),c(5,length(static_names)+length(optimize_names)))
  expect_equal(length(results$like_trace),nrow(results$param_trace))
  expect_true(is.double(results$accept_rate))
  expect_true(results$accept_rate <= 1 & results$accept_rate >=0)
  expect_true(!anyNA(results$param_trace))
  
})

test_that("logs are properly created", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
    vrp_filename <- "testing_jeff.vrp"
  }
  
  vpdir_in <- paste(vpdir, "tests/input_test/", sep = "")
  vpdir_out <- paste(vpdir, "tests/output_test/", sep = "")
  vpdir_log <- paste(vpdir, "tests/logs_test/", sep = "")
  vpdir_exe <- paste(vpdir, "bin/", sep = "")
  vp_binary <- "VarroaPop.exe"
  vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")
  vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")
  vpdir_neonic_prof <- paste(vpdir, "data/processed/neonic_profiles/",sep="")
  vpdir_weather <- paste(vpdir, "data/external/weather/",sep="")
  dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                       exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                       weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)
  SimEnd <- "8/25/2015"
  koc <- 60
  kow <- 2
  
  static_names <- c("SimEnd")
  static_values <- c(SimEnd)
  static_list <- list(names = static_names, values = static_values)
  
  optimize_names <- c("ICQueenStrength")
  bound_l <- c(1) #lower bondary of the domain for each parameter to be optimized
  bound_u <- c(5) #upper bondary of the domain for each parameter to be optimized
  optimize_list <- list(names = optimize_names, bound_l = bound_l, bound_u = bound_u)
  results <- new_vp_mcmc_c(vrp_filename = vrp_filename,nsims = 5, step_length = .5, vp_dir=vpdir, dir_structure=dir_structure,static_vars = static_list, 
                           optimize_vars = optimize_list, logs = T, verbose = F)
  expect_true(file.exists(paste(dir_structure$log,"log1_1.txt",sep="")))
  expect_true(file.exists(paste(dir_structure$log,"log3_5.txt",sep="")))
  expect_true(file.exists(paste(dir_structure$log,"log5_10.txt",sep="")))
  
})


context("Extending a vp_mcmc run from previous run object")

test_that("returns full vp_mcmc object with valid results", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
    vrp_filename <- "testing_jeff.vrp"
  }
  
  vpdir_in <- paste(vpdir, "tests/input_test/", sep = "")
  vpdir_out <- paste(vpdir, "tests/output_test/", sep = "")
  vpdir_log <- paste(vpdir, "tests/logs_test/", sep = "")
  vpdir_exe <- paste(vpdir, "bin/", sep = "")
  vp_binary <- "VarroaPop.exe"
  vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")
  vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")
  vpdir_neonic_prof <- paste(vpdir, "data/processed/neonic_profiles/",sep="")
  vpdir_weather <- paste(vpdir, "data/external/weather/",sep="")
  dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                       exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                       weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)
  SimEnd <- "8/25/2015"
  koc <- 60
  kow <- 2
  
  static_names <- c("SimEnd","koc","kow")
  static_values <- c(SimEnd,koc,kow)
  static_list <- list(names = static_names, values = static_values)
  
  optimize_names <- c("ICQueenStrength","IPollenTrips","INectarTrips",
                      "ICForagerLifespan")
  bound_l <- c(1,4,4,4) #lower bondary of the domain for each parameter to be optimized
  bound_u <- c(5,30,48,16) #upper bondary of the domain for each parameter to be optimized
  optimize_list <- list(names = optimize_names, bound_l = bound_l, bound_u = bound_u)
  results <- new_vp_mcmc_c(vrp_filename = vrp_filename, nsims = 3, step_length = .5, vp_dir=vpdir, dir_structure = dir_structure, static_vars = static_list, 
                           optimize_vars = optimize_list, logs = F, verbose = F)
  results <- continue_vp_mcmc_c(3,results)
  expect_true(class(results) == "vp_mcmc_run")
  expect_equal(dim(results$param_trace),c(6,length(static_names)+length(optimize_names)))
  expect_equal(length(results$like_trace),nrow(results$param_trace))
  expect_true(is.double(results$accept_rate))
  expect_true(results$accept_rate <= 1 & results$accept_rate >=0)
  expect_true(!anyNA(results$param_trace))
  expect_equal(results$param_trace[1,"SimEnd"],results$param_trace[6,"SimEnd"])
})


