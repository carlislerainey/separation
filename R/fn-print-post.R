#'Print summary of the partial prior predictive distribution.
#'@title Print summary of the partial prior predictive distribution.
#'  
#'@description \code{print.post()} prints a summary for objects of the class \code{post} created by the functions \code{sim_post_*()}.
#'
#'
#'@param post An object created by the functions \code{sim_post_*()} with class \code{post}.
#'@param digits The number of digits for printing.
#'@param prob A numeric scalar in the interval (0,1) giving the target probability content of the intervals.
#'
#'@export
#'
print.post <- function(post, digits = 2, prob = 0.9) {
  cat("\n")
  cat(" Model:\t"); print(post$fn_args$formula)
  cat(" Prior:\t"); cat(post$prior); cat("\n\n")
  fmt <- paste("%.", digits, "f", sep = "")
  mean <- sprintf(fmt, apply(post$mcmc, 2, mean))
  median <- sprintf(fmt, apply(post$mcmc, 2, median))
  mode <- sprintf(fmt, find_modes(post$mcmc))
  p_lo <- sprintf(fmt, apply(post$mcmc, 2, quantile, (1 - prob)/2))
  p_hi <- sprintf(fmt, apply(post$mcmc, 2, quantile, 1 - (1 - prob)/2))
  et_ci <- paste("[", p_lo, ", ", p_hi, "]", sep = "")
  et_ci_name <- paste(100*prob, "% et ci", sep = "")
  hpd <- coda::HPDinterval(coda::as.mcmc(post$mcmc), prob = prob)
  hpd_lo <- sprintf(fmt, hpd[, 1])
  hpd_hi <- sprintf(fmt, hpd[, 2])
  hpd_ci <- paste("[", hpd_lo, ", ", hpd_hi, "]", sep = "")
  hpd_ci_name <- paste(100*prob, "% hpd ci", sep = "")
  if (post$fn_args$n_chains > 1) {
    R_hat <- sprintf(fmt, post$R_hat[[1]][, 1])
  } else {
    R_hat <- "-"
  }
  ess <- sprintf(fmt, coda::effectiveSize(post$mcmc_chains))
  res <- cbind(mean, median, mode, et_ci, hpd_ci, R_hat, ess)
  accept <- sprintf("%.0f", mean(100*calc_acceptance_rate(post)))
  rownames(res) <- colnames(post$mcmc)
  colnames(res) <- c("mean", "median", "mode", et_ci_name, hpd_ci_name, "R-hat", "eff. n")
  print(noquote(res), right = TRUE)
  cat("----\n")
  cat("Burnin:\t"); cat(post$fn_args$n_burnin); cat("\n")
  cat("Sims:\t"); cat(post$fn_args$n_sims); cat("\n")
  cat("Accept:\t"); cat(accept); cat("%\n")
  cat("Chains:\t"); cat(post$fn_args$n_chains); cat("\n")
  if (post$fn_args$n_chains > 1) {
    cat("R-hat:\t"); cat(round(post$R_hat[[2]], 1)); cat("\n")
  } else {
    cat("R-hat:\t"); cat("-"); cat("\n")
  }
}