
context("compact_hist")


test_that("compact_hist() returns no errors", {
  n <- 1000
  x <- 1 + rexp(n, rate = .05)
  compact_hist(x, xlab = "x", main = "1 + exp(0.05)")
  compact_hist(x, log_scale = TRUE, xlab = "x", main = "1 + exp(0.05) on log scale")
  compact_hist(x, n_breaks = 50, xlab = "x", main = "1 + exp(0.05) w/ 50 break points")
})

