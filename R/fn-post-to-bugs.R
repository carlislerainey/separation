#' Convert an object of class "post" to an object of class "bugs"
#'@title Convert an object of class "post" to an object of class "bugs"
#'  
#'@description \code{post_to_bugs()} converts an object of class "post" to an 
#' object of class "bugs," which makes a few methods for monitoring convergence available.
#'  
#'  
#'@param post An object of class "post" created by one of the post_sim_*() functions.
#'
#'@export

post_to_bugs <- function(post) {
  if (class(post) != "post") {
    stop("Argument \"post\" must have class \"post.\" Was it created with one of the post_sim_*() functions?")
  }
  var_names <- colnames(post$mcmc)
  n_chains <- post$fn_args$n_chains
  mcmc_array <- array(NA, dim = c(post$fn_args$n_sims, 
                                  post$fn_args$n_chains, 
                                  ncol(post$mcmc)),
                      dimnames = list(NULL, NULL, var_names))
  for (i in 1:n_chains) {
    mcmc_array[, i, ] <- as.matrix(post$mcmc_chains[[i]])
  }
  bugs <- R2WinBUGS::as.bugs.array(mcmc_array)
}