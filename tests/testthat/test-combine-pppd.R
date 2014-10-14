
context("combine_pppd")


test_that("combine_pppd() works properly", {
  data(politics_and_need)
  
  normal_2 <- rnorm(10000, sd = 2)
  normal_4 <- rnorm(10000, sd = 4)
  
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  
  pppd1 <- calc_pppd(f, data = politics_and_need, prior_sims = normal_2, sep_var_name = "gop_governor", 
                     prior_label = "Normal(0, 2)")
  pppd2 <- calc_pppd(f, data = politics_and_need, prior_sims = normal_4, sep_var_name = "gop_governor", 
                     prior_label = "Normal(0, 4)")
  pppds <- combine_pppd(pppd1, pppd2)
  expect_true(is.list(pppds))
  expect_named(pppds[[1]])
  expect_named(pppds[[2]])
})

