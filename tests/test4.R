#Unit testing for 04read_output.R


context("Executing VP via wrapper (10 runs)")

test_that("outputting array of correct dimensions", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_test_results <- paste(vp_dir, "tests/test_4_files/", sep = "")

  results <- read_output_c(1,vpdir_test_results)
  
  #expect_type(results, array)
  expect_true(is.array(results))
  expect_equal(dim(results),c(125,29,10))
  expect_equal(length(results[!is.na(results[,1,1]),1,1]),119)
  expect_equal(length(results[!is.na(results[,1,10]),1,10]),118)
  expect_equal(length(results[!is.na(results[,1,5]),1,5]),120)
  
})

test_that("giving correct numbers from results .txt files", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_test_results <- paste(vp_dir, "tests/test_4_files/", sep = "")
  
  results <- read_output_c(1,vpdir_test_results)
  
  #expect_type(results, array)
  expect_equal(results[1,1,1], 18302)
  expect_equal(results[2,29,1], 0)
  expect_equal(results[2,28,1], 11.500)
  expect_equal(results[119,3,1], 36841)
  expect_equal(results[1,1,10], 6590)
  expect_equal(results[2,29,10], 0)
  expect_equal(results[2,28,10], 12.500)
  expect_equal(results[118,3,10], 33741)
  
})


test_that("pesticide contamination values read properly", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_test_results <- paste(vp_dir, "tests/test_4_files/", sep = "")
  
  results <- read_output_c(1,vpdir_test_results)
  
  #expect_type(results, array)
  expect_equal(results[5,19,1], 0.001)
  expect_equal(results[9,21,1], 0.014)
  expect_equal(results[5,19,8], 0.006)
  expect_equal(results[9,21,8], 0.027)
  
})