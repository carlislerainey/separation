#'Print summary of the partial prior predictive distribution.
#'@title Print summary of the partial prior predictive distribution.
#'  
#'@description \code{print.pppd()} prints a summary for object of the class pppd
#'
#'
#'@param pppd An object of class pppd.
#'@param probs A numeric vector of probabilities with values in [0,1] a which to compute the quantile.
#'@param round_to The number of decimal place to round the results.
#'
#'@export
print.pppd <- function(pppd, probs = c(.01, .05, .25, .5, .75, .95, .99), round_to = 3) {
  cat("\n")
  cat("Model:\t"); print(pppd$args$formula)
  cat(paste("Prior for ", pppd$args$sep_var_name, ":\t", sep = "")); cat(pppd$prior_label); cat("\n")
  cat("\n")
  
  cat("   percentile\t")
  qi_names <- c("pr", "fd", "rr")
  qi_long_names <- c("predicted probability", "first-difference", "risk-ratio")
  for (j in 1:length(qi_long_names)) {
    cat(qi_long_names[j]); cat("\t")
  }
  cat("\n")
  for (i in 1:length(probs)) {
    cat("   "); cat(paste(100*probs[i], "%\t\t", sep = ""))
    for (j in 1:length(qi_names)) {
      qi <- pppd[[qi_names[j]]]
      q <- quantile(qi, probs[i])
      q <- round(q, round_to)
      cat(q); cat("\t\t\t")
    }
    cat("\n")
  }
  cat("\n")
}