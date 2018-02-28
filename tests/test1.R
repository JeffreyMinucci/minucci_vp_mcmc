#Unit testing for 01parameterize_simulation.R


context("Initial parameterization")

test_that("returns 1 row dataframe with col for each parameter", {
  static_names_1 <- c("test1")
  static_vals_1 <- c(5)
  to_optimize_1 <- c("test_opt1")
  upper_1 <- c(100)
  lower_1 <- c(1)
  result_1 <- generate_vpstart(static_names_1,static_vals_1,to_optimize_1,lower_1,upper_1,verbose=F)
  expect_equal(dim(result_1),c(1,length(static_names_1)+length(to_optimize_1)))
  
  static_names_2 <- c("test1","test2","test3")
  static_vals_2 <- c(5,10,15)
  to_optimize_2 <- c("test_opt1","test_opt2","test_opt3")
  upper_2 <- c(100,1000,10000)
  lower_2 <- c(1,100,1000)
  result_2 <- generate_vpstart(static_names_2,static_vals_2,to_optimize_2,lower_2,upper_2,verbose=F)
  expect_equal(dim(result_2),c(1,length(static_names_2)+length(to_optimize_2)))
})


test_that("all input variables included in output", {
  static_names_1 <- c("test1")
  static_vals_1 <- c(5)
  to_optimize_1 <- c("test_opt1")
  upper_1 <- c(100)
  lower_1 <- c(1)
  result_1 <- generate_vpstart(static_names_1,static_vals_1,to_optimize_1,lower_1,upper_1,verbose=F)
  expect_true(all(c(static_names_1,to_optimize_1) %in% colnames(result_1)))
  
  static_names_2 <- c("test1","test2","test3")
  static_vals_2 <- c(5,10,15)
  to_optimize_2 <- c("test_opt1","test_opt2","test_opt3")
  upper_2 <- c(100,1000,10000)
  lower_2 <- c(1,100,1000)
  result_2 <- generate_vpstart(static_names_2,static_vals_2,to_optimize_2,lower_2,upper_2,verbose=F)
  expect_true(all(c(static_names_1,to_optimize_1) %in% colnames(result_2)))
})


test_that("randomly generated parameters are within bounds", {
  require(foreach)
  static_names <- c("test1","test2","test3")
  static_vals <- c(5,10,15)
  to_optimize <- c("test_opt1","test_opt2")
  upper <- c(1,1000)
  lower <- c(0,100)
  result <- foreach(i=c(1:1000),.combine=rbind) %do% {
    generate_vpstart(static_names,static_vals,to_optimize,lower,upper,verbose=F)
  }
  expect_true(all(c(all(result[,to_optimize[1]] > lower[1]),all(result[,to_optimize[2]] > lower[2]),
                    all(result[,to_optimize[1]] < upper[1]), all(result[,to_optimize[2]] < upper[2]))))
})


test_that("Throws error if upper bounds are not all greater than lower", {
  static_names <- c("test1","test2","test3")
  static_vals <- c(5,10,15)
  to_optimize <- c("test_opt1","test_opt2")
  upper <- c(1,1000)
  lower <- c(2,100)
  expect_error(generate_vpstart(static_names,static_vals,to_optimize,lower,upper,verbose=F))
})