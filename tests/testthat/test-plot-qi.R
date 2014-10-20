
context("plot.qi")


test_that("the sim_post_*() functions return no errors when ran and printed", {
  data(politics_and_need)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca*percent_uninsured
  post <- sim_post_normal(f, data = politics_and_need, 
                          sd = 4.5, sep_var = "gop_governor",
                          n_chains = 3,
                          n_sims = 100, 
                          n_thin = 1,
                          tune = 1) 
  par(mfrow = c(1, 1))
  # a single pr
  X_pred_list <- set_at_median(f, politics_and_need)
  qi <- calc_qi(post, X_pred_list)
  plot(qi)
  # long list of pr
  X_pred_list <- set_at_median(f, politics_and_need)
  x <- 0:100
  X_pred_list$percent_favorable_aca <- x
  qi <- calc_qi(post, X_pred_list)
  plot(qi, x)
})
