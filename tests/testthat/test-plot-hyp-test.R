
context("plot_hyp_test")


test_that("plot_hyp_test() returns no errors", {
  data(politics_and_need)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
    bal2012 + multiplier + percent_nonwhite + percent_metro
  post <- sim_post_normal(f, data = politics_and_need, 
                          sd = 4.5, sep_var = "gop_governor",
                          n_chains = 3,
                          n_sims = 100, 
                          n_thin = 1,
                          tune = 1) 
  post2 <- sim_post_normal(f, data = politics_and_need, 
                           sd = 9, sep_var = "gop_governor",
                           n_chains = 3,
                           n_sims = 100, 
                           n_thin = 1,
                           tune = 1) 
  
  posts <- combine_post(post, post2)
  plot_hyp_test(posts, "gop_governor", research_hyp = "+")
  plot_hyp_test(posts, "percent_favorable_aca", research_hyp = "+")
  plot_hyp_test(posts, "percent_uninsured", research_hyp = "-")
})

