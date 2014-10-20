
context("plot.post and plot.posts()")


test_that("the plot.post() and plot.posts() functions work", {
  data(politics_and_need)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca*percent_uninsured
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
  
  
  plot(post, "gop_governor")
  plot(posts, "gop_governor")
})
