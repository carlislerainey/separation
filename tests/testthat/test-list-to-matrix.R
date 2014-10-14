
context("list_to_matrix")


test_that("list_to_matrix returns the correct matrix", {
  list0 <- list(one = 1, two = c(2, 4))
  correct <- cbind(1, c(2, 4)); colnames(correct) <- names(list0)
  X_pred_mat <- list_to_matrix(list0) 
  expect_equal(X_pred_mat, correct)
  expect_false(is.null(colnames(X_pred_mat)), label = "No column names.")
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