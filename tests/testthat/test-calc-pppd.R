
context("calc_pppd")


test_that("calc_pppd returns a list that looks right", {
  data(politics_and_need)
  pr_sims <- rnorm(10000, sd = 1)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  pppd <- calc_pppd(f, data = politics_and_need, prior_sims = pr_sims, sep_var_name = "gop_governor")  
  expect_true(sum(!is.finite(pppd$pr0)) == 0, "check that pr0 is finite")
  expect_true(sum(!is.finite(pppd$pr1)) == 0, "check that pr1 is finite")
  expect_true(sum(!is.finite(pppd$fd)) == 0, "check that fd is finite")
  expect_true(sum(!is.finite(pppd$rr)) == 0, "check that rr is finite")
  expect_true(sum(pppd$fd <= 0) == 0, label = "check that the pppd makes sense for the politics and need data")
})

test_that("calc_pppd returns an error when given a variable that is not binary 0/1", {
  n <- 30
  s <- sample(c(0, 1, 2), size = n, replace = TRUE) 
  x <- rnorm(n)
  p <- plogis(x)
  y <- rbinom(n, 1, p)
  y[s == 1] <- 1
  d <- data.frame(y, s, x)
  expect_error(calc_pppd(y ~ s + x, data = d, sep_var_name = "s", prior_sims = rnorm(10)))
})

test_that("calc_pppd returns an when there is no separation", {
  n <- 30
  x <- rbinom(n, 1, .5)
  p <- plogis(x)
  y <- rbinom(n, 1, p)
  d <- data.frame(y, x)
  expect_error(calc_pppd(y ~ x, data = d, sep_var_name = "x", prior_sims = rnorm(10)))
})

test_that("calc_pppd returns an error when given a variable that is not binary 0/1", {
  # a function to generate separation of various types
  # - type 1 - 0's in upper left
  # - type 2 - 0's in lower left
  # - type 3 - 0's in upper right
  # - type 4 - 0's in lower right
  gen_sep_data <- function(type, n = 100) {
    s <- c(rep(0, n/2), rep(1, n/2))
    x <- rnorm(n)
    y <- rbinom(n, 1, plogis(x))
    if (type == 1) {
      y[s == 0] <- 1
    }
    if (type == 2) {
      y[s == 0] <- 0
    }
    if (type == 3) {
      y[s == 1] <- 1
    }
    if (type == 4) {
      y[s == 1] <- 0
    }   
    d <- data.frame(y, x, s)
    return(d)
  }
  prior_sims <- rnorm(1000)
  # check type 1
  d <- gen_sep_data(type = 1)
  pppd <- calc_pppd(y ~ s + x, data = d, prior_sims = prior_sims, sep_var_name = "s")
  expect_true(sum(pppd$fd >= 0) == 0, label = "check type 1")
  # check type 2
  d <- gen_sep_data(type = 2)
  pppd <- calc_pppd(y ~ s + x, data = d, prior_sims = prior_sims, sep_var_name = "s")
  expect_true(sum(pppd$fd <= 0) == 0, label = "check type 2")
  # check type 3
  d <- gen_sep_data(type = 3)
  pppd <- calc_pppd(y ~ s + x, data = d, prior_sims = prior_sims, sep_var_name = "s")
  expect_true(sum(pppd$fd <= 0) == 0, label = "check type 3")
  # check type 4
  d <- gen_sep_data(type = 4)
  pppd <- calc_pppd(y ~ s + x, data = d, prior_sims = prior_sims, sep_var_name = "s")
  expect_true(sum(pppd$fd >= 0) == 0, label = "check type 4")
})
