
context("list_to_matrix")


test_that("list_to_matrix returns the correct matrix", {
  data(politics_and_need) 
  d <- politics_and_need
  f <- oppose_expansion ~ gop_leg*percent_favorable_aca
  X_pred_list <- set_at_median(f, d)
  X_pred_mat <- list_to_matrix(X_pred_list, f)
  expect_identical(colnames(X_pred_mat)[4], "gop_leg:percent_favorable_aca")
  X_pred_list$percent_favorable_aca <- 0:100
  X_pred_mat <- list_to_matrix(X_pred_list, f)
  expect_equal(nrow(X_pred_mat), 101)
  expect_equal(ncol(X_pred_mat), 4)
  })

test_that("list_to_matrix returns an error when not given an appropriately constrained list", {
  list0 <- list(1, 2, 3)
  list1 <- list(one = "a", two = c(2, 4))
  list2 <- list(one = c(1, 2), two = c(2, 4))
  expect_error(list_to_matrix(1))
  expect_error(list_to_matrix(list0))
  expect_error(list_to_matrix(list1))
  expect_error(list_to_matrix(list2))
})