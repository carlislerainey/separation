
context("plot.pppd")


test_that("plot.pppd() returns no errors", {
  data(politics_and_need)
  pr_sims <- rnorm(10000, sd = .5)
  politics_and_need$dem_governor <- 1 - politics_and_need$gop_governor
  f <- oppose_expansion ~ dem_governor + percent_favorable_aca + percent_uninsured
  pppd <- calc_pppd(f, data = politics_and_need, prior_sims = pr_sims, sep_var_name = "dem_governor", 
                    prior_label = "Normal(0, 2)", treat_one_low = TRUE)  
  par(mfrow = c(1, 1))
  plot(pppd)
  plot(pppd, qi_name = "fd")
  plot(pppd, counts = FALSE)
  plot(pppd, qi_name = "rr", log = TRUE)
  plot(pppd, lower = .2, upper = .4)
})

