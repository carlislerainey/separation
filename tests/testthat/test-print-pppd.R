context("print.pppd")

test_that("calc_pppd generates no errors", {
  data(politics_and_need)
  pr_sims <- rnorm(10000, sd = 2)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  pppd <- calc_pppd(f, data = politics_and_need, prior_sims = pr_sims, sep_var_name = "gop_governor", 
                    prior_label = "Normal(0, 2)")  
  expect_output(print(pppd), "*")
    
  politics_and_need$dem_governor <- 1 - politics_and_need$gop_governor
  f <- oppose_expansion ~ dem_governor + percent_favorable_aca + percent_uninsured
  pppd <- calc_pppd(f, data = politics_and_need, prior_sims = pr_sims, sep_var_name = "dem_governor", 
                    prior_label = "Normal(0, 2)")  
  expect_output(print(pppd), "*")
})