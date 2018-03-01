#Unit testing for 04read_output.R


context("Executing VP via wrapper (10 runs)")

test_that("outputting arrayof correct dimensions", {
  if(Sys.info()[4]=="DZ2626UJMINUCCI"){
    vp_dir<-path.expand("d:/Git_files/minucci_vp_mcmc/")   #This needs to be set to your repo path
  }
  vpdir_test_results <- paste(vp_dir, "tests/results_test_files/", sep = "")

  results <- read_output_c(1,vpdir_test_results)
  
  #expect_type(results, array)
  expect_equal(dim(results),c(125,29,10))
  
})
