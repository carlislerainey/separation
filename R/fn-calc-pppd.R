
#'Calculate partial prior predictive distribution
#'@title Partial prior predictive distribution
#'  
#'@description \code{calc_pppd} returns the partial prior predictive
#'distribution for a logistic regression.
#'
#'
#'@param formula A formula for a logistic regression model.
#'@param data A data frame.
#'@param prior_sims Simulations from the desired prior distribution.
#'@param sep_var_name The name of the separating variable. 
#'@param treat_one_low Treat \code{sep_var_at} as the low value when computing QIs. Defaults to
#'  TRUE. If FALSE, the \code{sep_var_at} is treated as the high value.
#'@param X_pred_list A named list of values at which to set variables. The function
#'  \link{set_at_median()} facilitates creating this list.
#' @param prior_label A character naming the prior used.
#' 
#' @export

calc_pppd <- function(formula, data, prior_sims, sep_var_name,
                      treat_one_low = FALSE, X_pred_list = NULL,
                      prior_label = NULL) {
  # estimate model with maximum likelihood
  mle <- glm(formula = formula, data = data, family = "binomial", x = TRUE)
  # test that separating variable is binary 0/1
  s <- data[[sep_var_name]]
  s.vals <- sort(unique(s))
  if (length(s.vals) > 2) {
    stop("The variable ", sep_var_name, " must be binary.")
  }
  if (sum(s.vals == c(0, 1)) != 2) {
    stop("The variable ", sep_var_name, " must be binary and coded as 0 or 1.")
  }
  # find type of separation
  if (sum(table(mle$y, mle$x[, sep_var_name]) == 0) == 0) {
    stop("It seems that there is no separation--", sep_var_name, 
         " doesn't perfectly predict zeros or ones.")
  }
  type <- which(table(mle$y, mle$x[, sep_var_name]) == 0)
  # estimate b.hat.mle
  if (is.null(X_pred_list)) {
    # create matrix at which to calculate the baseline
    X_pred_list <- set_at_median(formula, data)
  }
  if (type == 1 | type == 2) {
    X_pred_list[[sep_var_name]] <- 1
  }
  if (type == 3 | type == 4) {
    X_pred_list[[sep_var_name]] <- 0
  }
  X_pred_mat <- list_to_matrix(X_pred_list, formula)
  # calculate the quantities of interest
  baseline <- predict(mle, newdata = data.frame(X_pred_mat))
  baseline_pr <- plogis(baseline)
  if (type == 1) {
    pr <- pr0 <- plogis(baseline + prior_sims[prior_sims > 0])
    pr1 <- plogis(baseline)
  }
  if (type == 2) {
    pr <- pr0 <- plogis(baseline + prior_sims[prior_sims < 0])
    pr1 <- plogis(baseline)
  }
  if (type == 3) {
    pr0 <- plogis(baseline)
    pr <- pr1 <- plogis(baseline + prior_sims[prior_sims > 0])
  }
  if (type == 4) {
    pr0 <- plogis(baseline)
    pr <- pr1 <- plogis(baseline + prior_sims[prior_sims < 0])
  }
  if (treat_one_low == FALSE) {
    fd <- pr1 - pr0
    rr <- pr1/pr0
  }
  if (treat_one_low == TRUE) {
    fd <- pr0 - pr1
    rr <- pr0/pr1
  }
  # check for infinite risk ratios.
  if (max(rr) == Inf) {
    message("\n", "Of the ", 
              length(prior_sims), 
              " prior simulations, ",
              sum(rr == Inf), " (", round(100*sum(rr == Inf)/length(prior_sims), 2), "%)",
              " produced risk-ratios of infinity.\nThe largest ", 
              ceiling(300*sum(rr == Inf)/length(prior_sims)), "% of simulations are being dropped.\n\n", sep = "")
    keep <- rr <= quantile(rr, 1 - ceiling(300*sum(rr == Inf)/length(prior_sims))/100)
    pr0 <- pr0[keep]
    pr1 <- pr1[keep]
    pr <- pr[keep]
    rr <- rr[keep]
    fd <- fd[keep]
    coef <- prior_sims[keep]
  }
  if (is.null(prior_label)) {
    prior_label <- "no label given"
  }
  args <- list(formula = formula, 
               data = data, 
               prior_sims = prior_sims, 
               sep_var_name = sep_var_name, 
               treat_one_low = treat_one_low, 
               X_pred_list = X_pred_list,
               prior_label = prior_label)
  # return simulations
  pppd <- list(prior_label = prior_label,
               pr0 = pr0,
              pr1 = pr1, 
              pr = pr,
              rr = rr,
              fd = fd,
              coef = coef,
              baseline_pr = baseline_pr,
              type = type,
              mle = mle,
              args = args)
  class(pppd) <- "pppd"
  return(pppd)
}


