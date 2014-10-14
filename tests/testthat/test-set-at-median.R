
context("set_at_median")


test_that("set_at_median returns a list for a data frame input", {
  data(politics_and_need) 
  expect_warning(X_pred_list <- set_at_median(politics_and_need))
  expect_true(is.list(X_pred_list))
})

test_that("set_at_median returns a list for a named matrix input", {
  x <- cbind(rnorm(10), rnorm(10))
  colnames(x) <- c("one", "two")
  X_pred_list <- set_at_median(x)
  expect_true(is.list(X_pred_list))
})

test_that("set_at_median returns an error for an unnamed matrix input", {
  x <- cbind(rnorm(10), rnorm(10))
  expect_error(set_at_median(x))
})

test_that("set_at_median returns an error for a vector", {
  x <- rnorm(10)
  expect_error(set_at_median(x))
})

test_that("set_at_median returns an error for a scalar", {
  x <- 10
  expect_error(set_at_median(x))
})