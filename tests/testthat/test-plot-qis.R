
context("plot.qis()*")


test_that("the plot.qis() work as intended", {
  data(politics_and_need)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
    bal2012 + multiplier + percent_nonwhite + percent_metro
  
  post <- sim_post_normal(f, data = politics_and_need, 
                          sd = 1, sep_var = "gop_governor",
                          n_chains = 3,
                          n_sims = 100) 
  post2 <- sim_post_normal(f, data = politics_and_need, 
                           sd = 2, sep_var = "gop_governor",
                           n_chains = 3,
                           n_sims = 100) 
  post3 <- sim_post_normal(f, data = politics_and_need, 
                           sd = 3, sep_var = "gop_governor",
                           n_chains = 3,
                           n_sims = 100) 
  
  X_pred_list <- set_at_median(f, politics_and_need)
  X_pred_list$gop_governor <- 0
  qi <- calc_qi(post, X_pred_list)
  qi2 <- calc_qi(post2, X_pred_list)
  qi3 <- calc_qi(post3, X_pred_list)
  qis <- combine_qi(qi3, qi, qi2)
  #plot_qis_1(qis)
  plot(qis, plot_matrix_layout = c(2, 2))
  
  x_vals <- seq(0, 1, by = .01)
  X_pred_list <- set_rest_at_median(f, politics_and_need, "gop_governor", x_vals)
  qi <- calc_qi(post, X_pred_list)
  qi2 <- calc_qi(post2, X_pred_list)
  qi3 <- calc_qi(post3, X_pred_list)
  qis <- combine_qi(qi3, qi, qi2)
  #plot_qis_range(qis, x_vals)
  plot(qis = qis, x_vals = x_vals, plot_matrix_layout = c(2, 2))
})
