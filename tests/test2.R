#Unit testing for 02write_input.R


context("Writing VP input .txt files")

test_that("Txt files created and all given parameters included", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_testin <- paste(vp_dir, "tests/input_test/", sep = "")
  initial_dir <- paste(vp_dir,"data/raw/field_initial_conditions.csv",sep="")
  input_df_3 <- data.frame("SimEnd"=c("8/25/2015"),"two"=c(321),"three"=c(5),stringsAsFactors = F)
  initial_conditions <- read.csv(initial_dir,stringsAsFactors=FALSE)
  write_vp_input_sites_c(input_df_3,vpdir_testin,initial_conditions)
  result_1 <- read.table(paste(vpdir_testin,"input_mcmc_1.txt",sep=""),stringsAsFactors = F)
  result_10 <- read.table(paste(vpdir_testin,"input_mcmc_10.txt",sep=""),stringsAsFactors = F)
  expect_equal(nrow(result_1),7+ncol(input_df_3))
  expect_equal(result_1[8,1], "SimEnd=8/25/2015")
  expect_equal(result_10[8,1], "SimEnd=8/25/2015")
  expect_equal(result_1[10,1], "three=5")
  expect_equal(result_10[10,1], "three=5")
  
  input_df_1 <- data.frame("SimEnd"=c("8/25/2015"))
  initial_conditions <- read.csv(initial_dir,stringsAsFactors=FALSE)
  write_vp_input_sites_c(input_df_1,vpdir_testin,initial_conditions)
  result_1 <- read.table(paste(vpdir_testin,"input_mcmc_1.txt",sep=""),stringsAsFactors = F)
  result_10 <- read.table(paste(vpdir_testin,"input_mcmc_10.txt",sep=""),stringsAsFactors = F)
  expect_equal(nrow(result_1),7+ncol(input_df_1))
  expect_equal(result_1[8,1], "SimEnd=8/25/2015")
  expect_equal(result_10[8,1], "SimEnd=8/25/2015")

})

test_that("Initial conditions are written correctly for sites", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_testin <- paste(vp_dir, "tests/input_test/", sep = "")
  initial_dir <- paste(vp_dir,"data/raw/field_initial_conditions.csv",sep="")
  input_df_1 <- data.frame("SimEnd"=c("8/25/2015"))
  initial_conditions <- read.csv(initial_dir,stringsAsFactors=FALSE)
  write_vp_input_sites_c(input_df_1,vpdir_testin,initial_conditions)
  result_1 <- read.table(paste(vpdir_testin,"input_mcmc_1.txt",sep=""),stringsAsFactors = F)
  result_10 <- read.table(paste(vpdir_testin,"input_mcmc_10.txt",sep=""),stringsAsFactors = F)
  expect_equal(result_1[1,1], "ICWorkerAdults=18302.161")
  expect_equal(result_1[7,1], "SimStart=04/30/2015")
  expect_equal(result_10[1,1], "ICWorkerAdults=6590.366")
  expect_equal(result_10[7,1], "SimStart=05/01/2015")
  
})
