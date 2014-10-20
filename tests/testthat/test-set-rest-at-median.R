
context("set_rest_at_median")


test_that("set_rest_at_median works", {
  data(politics_and_need) 
  d <- politics_and_need
  f <- oppose_expansion ~ gop_leg*percent_favorable_aca
  # use set_at_median
  X_pred_list <- set_at_median(f, d)
  x <- 0:100
  X_pred_list$percent_favorable_aca <- x
  X_pred_list2 <- set_rest_at_median(f, d, "percent_favorable_aca", x)
  expect_identical(X_pred_list, X_pred_list2)
})