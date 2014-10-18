
context("post_to_bugs")


test_that("post_to_bugs() creates a bugs object and plot.bugs() works as intended", {
  data(politics_and_need_rescaled)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  post <- sim_post_gelman(f, data = politics_and_need_rescaled, 
                          n_chains = 3,
                          n_burnin = 0,
                          n_sims = 10) 
  
  
  bugs <- post_to_bugs(post)
  expect_true(class(bugs) == "bugs")
  plot(bugs)
  print(bugs)
})
