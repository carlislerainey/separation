calc_acceptance_rate <- function(post) {
  n_chains <- post$fn_args$n_chains
  ar <- numeric(n_chains)
  for (i in 1:n_chains) {
    mcmc <- as.matrix(post$mcmc_chains[[i]])
    n_row <- nrow(mcmc)
    id <- numeric(n_row - 1)
    for (j in 1:(n_row - 1)) {
      id[j] <- identical(mcmc[j, ], mcmc[j + 1,  ])
    }
    ar[i] <- 1 - sum(id)/(n_row - 1)
  }
  return(ar)
}
