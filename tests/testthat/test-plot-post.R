
context("plot.post")


test_that("the plot.post() function works", {
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
  plot(post, "gop_governor")
})
