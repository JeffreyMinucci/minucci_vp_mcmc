#Unit testing for 05likelihood.R


context("Computing likelihood for 10 sites and 3 dates")

test_that("output is a double", {
  test_data <- matrix(rep(10,30),ncol=3)
  test_preds <- matrix(rep(15,30),ncol=3)
  test_var <- 1
  like <- vp_loglik_sites_c(test_data,test_preds,test_var)
  expect_true(is.double(like))
})

test_that("log likelihood is calculated correctly for gaussian error", {
  test_data <- matrix(rep(10,30),ncol=3)
  test_preds <- matrix(rep(15,30),ncol=3)
  test_var <- 1
  like <- vp_loglik_sites_c(test_data,test_preds,test_var)
  expect_equal(like, -402.568155996)
  
  test_data <- matrix(rep(10,3),ncol=3)
  test_preds <- matrix(rep(10,3),ncol=3)
  test_var <- 0.1
  like <- vp_loglik_sites_c(test_data,test_preds,test_var)
  expect_equal(like, 0.69706203988)
})
