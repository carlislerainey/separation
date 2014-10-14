
context("plot.pppds")


test_that("plot.pppds() returns no errors", {
  data(politics_and_need)
  
  normal_2 <- rnorm(10000, sd = 2)
  normal_4 <- rnorm(10000, sd = 4)
  
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  
  pppd1 <- calc_pppd(f, data = politics_and_need, prior_sims = normal_2, sep_var_name = "gop_governor", 
                     prior_label = "Normal(0, 2)")
  pppd2 <- calc_pppd(f, data = politics_and_need, prior_sims = normal_4, sep_var_name = "gop_governor", 
                     prior_label = "Normal(0, 4)")
  pppds <- combine_pppd(pppd1, pppd2, pppd1)
  
  par(oma = c(3, 3, 1, 1), mar = rep(1.5, 4))
  plot(pppds)
  plot(pppds, log_scale = TRUE, qi = "rr", plot_matrix_layout = c(2, 2))
  plot(pppds, qi = "fd", plot_matrix_layout = c(3, 2))
})

