
context("sim_post_star")


test_that("the sim_post_star() functions return no errors when ran and printed", {
  data(politics_and_need)
  data(politics_and_need_rescaled)
  f <- oppose_expansion ~ gop_governor + percent_favorable_aca + percent_uninsured
  post_gelman <- sim_post_gelman(f, data = politics_and_need_rescaled, 
                                 n_chains = 10,
                                 n_burnin = 0,
                                 n_sims = 100) 
  post_jeffreys <- sim_post_jeffreys(f, data = politics_and_need, 
                                     n_chains = 10,
                                     n_burnin = 0,
                                     n_sims = 100) 
  post_normal <- sim_post_normal(f, data = politics_and_need, sep_var = "gop_governor", 
                                 n_chains = 10,
                                 n_burnin = 0,
                                 n_sims = 100) 
  plot(post_jeffreys$mcmc_chains[, 2])
  plot(post_gelman$mcmc_chains[, 2])
  plot(post_normal$mcmc_chains[, 2])
  print(post_normal)
  print(post_jeffreys)
  print(post_gelman)
})
