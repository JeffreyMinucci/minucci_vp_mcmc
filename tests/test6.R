#Unit testing for 06propose_mh_step.R


context("Generate new metropolis-Hastings proposal ")

test_that("returns a vector of doubles", {
  result <- metropolis_proposal_c(c(4,10,1000), c(.1,1,10),5)
  expect_true(is.double(result))
})

test_that("returns a vector with same length as input vectors", {
  result <- metropolis_proposal_c(c(4,10,1000), c(.1,1,10),5)
  expect_equal(length(result),3)
})

test_that("throws error if length of current and scale vectors are different", {
  expect_error(metropolis_proposal_c(c(4,10,1000), c(.1),5))
})

test_that("throws error if step is not a single value", {
  expect_error(metropolis_proposal_c(c(4,10,1000), c(.1,1,10),c(5,10)))
})

test_that("can handle a single parameter", {
  expect_silent(result <- metropolis_proposal_c(c(4), c(.1),5))
  expect_true(is.double(result))
  expect_equal(length(result),1)
})
