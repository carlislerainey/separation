#'Print summary of the partial prior predictive distribution.
#'@title Print summary of the partial prior predictive distribution.
#'  
#'@description \code{print.post()} and \code{print.post_gelman()} print a summary for object of the class \code{post} and \code{post_gelman} created by the functions \code{sim_post_*()}.
#'
#'
#'@param post An object created by the functions \code{sim_post_*()} with class \code{post} or \code{post_gelman}.
#'
#'
#'@export

print.post <- print.post_gelman <- function(post) {
  cat("\n")
  cat(" Model:\t"); print(post$fn_args$formula)
  cat(" Prior:\t"); cat(post$prior); cat("\n\n")
  median <- apply(post$mcmc, 2, median)
  mean <- apply(post$mcmc, 2, mean)
  p05 <- apply(post$mcmc, 2, quantile, 0.05)
  p95 <- apply(post$mcmc, 2, quantile, 0.95)
  res <- cbind(mean, median, p05, p95)
  rownames(res) <- colnames(post$mcmc)
  colnames(res) <- c("mean", "median", "5th", "95th")
  print(round(res, 2))
  cat("----\n")
  cat("Burnin:\t"); cat(post$fn_args$n_burnin); cat("\n")
  cat("Sims:\t"); cat(post$fn_args$n_sims); cat("\n")
  cat("Chains:\t"); cat(post$fn_args$n_chains); cat("\n")
  cat("R-hat:\t"); cat(round(post$R_hat[[2]], 1)); cat("\n")
}

