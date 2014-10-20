
context("calc_qi*")


test_that("the sim_post_*() functions return no errors when ran and printed", {
  data(politics_and_need)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca*percent_uninsured
  post <- sim_post_normal(f, data = politics_and_need, 
                          sd = 4.5, sep_var = "gop_governor",
                          n_chains = 3,
                          n_sims = 100, 
                          n_thin = 1,
                          tune = 1) 
  # single pr
  X_pred_list <- set_at_median(f, politics_and_need)
  qi <- calc_qi(post, X_pred_list)
  expect_true(is.list(qi))
  # hpd
  qi <- calc_qi(post, X_pred_list, ci_type = "et")
  expect_true(is.list(qi))
  # rr (expect error)
  expect_error(calc_qi(post, X_pred_list, qi_name = "rr"))
  # rr for real
  X_pred_list$gop_governor <- c(0, 1)
  qi <- calc_qi(post, X_pred_list, qi_name = "rr")
  expect_true(is.list(qi))
  # change multiple variables
  X_pred_list$percent_favorable_aca <- 0:100
  expect_error(calc_qi(post, X_pred_list))
  # long list of pr
  X_pred_list <- set_at_median(f, politics_and_need)
  X_pred_list$percent_favorable_aca <- 0:100
  qi <- calc_qi(post, X_pred_list)
  expect_true(is.list(qi))
  expect_equal(length(qi$med), length(X_pred_list$percent_favorable_aca))
  expect_equal(length(qi$lwr), length(X_pred_list$percent_favorable_aca))
  expect_equal(length(qi$upr), length(X_pred_list$percent_favorable_aca))
  expect_equal(ncol(qi$qi_sims), length(X_pred_list$percent_favorable_aca))
  expect_equal(nrow(qi$qi_sims), nrow(post$mcmc))
})
