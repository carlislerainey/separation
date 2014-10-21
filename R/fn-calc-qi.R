 #' @title Calculate a quantity of interest from objects of class "post."
#'   
#' @description \code{calc_qi()} calculates a quantity of interest from objects
#' of class "post."
#' 
#' @param post An object of class "post" created by one of the sim_post_*()
#'   functions.
#' @param X_pred_list A named list, where the names correspond to variable names
#'   and the values correspond to values at which to set the variables. Only one
#'   element of the list should have more than one value. If \code{qi_name =
#'   "rr"} or \code{qi_name = "rr"} (see below), then one variable should have
#'   exactly two values and the rest should have exactly one.
#' @param prob A numeric scalar in the interval (0,1) giving the target
#'   probability content of the intervals.
#' @param qi_name The name of the quantity of interest to calculate. Either
 #'   \code{"pr"} (predicted probability), \code{"rr"} (risk-ratio), or
 #'   \code{"fd"} (first-difference). Defaults to \code{"pr"}.
#' @param ci_type The type of confidence interval to compute. Either
#'   \code{"hpd"} (highest posterior density) or \code{"et"} (equal-tailed). 
#'   Defaults to \code{"hpd"}.
#'   
#' @details Researchers can use this function to convert posterior simulations
#' of the logistic regression coefficients to quantities of interest, such as
#' predicted probabilities, risk-ratios, and first-differences.
#' 
#' @references Rainey, Carlisle. "Dealing with Separation in Logistic Regression
#'   Model." Working paper. Available at
#'   \url{http://crain.co/papers/separation.pdf}.
#'   
#' @export

calc_qi <- function(post, X_pred_list, prob = 0.9, qi_name = "pr", ci_type = "hpd") {
  X_pred_mat <- list_to_matrix(X_pred_list, post$fn_args$formula)
  # predicted probability
  if (qi_name == "pr") {
    qi <- t(plogis(X_pred_mat%*%t(post$mcmc)))
  }
  # first difference
  if (qi_name == "fd" | qi_name == "rr") {
    if (nrow(X_pred_mat) != 2) {
      stop("To compute a first-difference or risk-ratio, the argument \"X_pred_mat\" must have exactly two rows.")
    }
    pr <- t(plogis(X_pred_mat%*%t(post$mcmc)))
    if (qi_name == "fd") { qi <- pr[, 2] - pr[, 1] }
    if (qi_name == "rr") { qi <- pr[, 2]/pr[, 1] }
  }
  # summary for equal-tailed ci
  if (ci_type == "et") {
    p_lo <- (1 - prob)/2
    p_hi <- 1 - (1 - prob)/2
    if (qi_name == "pr") {
      median_qi <- apply(qi, 2, median)
      q_qi <- apply(qi, 2, quantile, c(p_lo, p_hi))
      lwr <- q_qi[1, ]
      upr <- q_qi[2, ]
    }
    if (qi_name == "fd" | qi_name == "rr") {
      median_qi <- median(qi)
      q_qi <- quantile(qi, c(p_lo, p_hi))
      lwr <- q_qi[1]
      upr <- q_qi[2]
    }
  }
  # summary for hpd ci
  if (ci_type == "hpd") {
    if (qi_name == "pr") {
      median_qi <- apply(qi, 2, median)
      q_qi <- coda::HPDinterval(coda::as.mcmc(qi), prob = prob)
      lwr <- q_qi[, 1]
      upr <- q_qi[, 2]
    }
    if (qi_name == "fd" | qi_name == "rr") {
      median_qi <- median(qi)
      q_qi <- coda::HPDinterval(coda::as.mcmc(qi), prob)
      lwr <- q_qi[1]
      upr <- q_qi[2]
    }
  }
  qi <- list(med = median_qi,
             lwr = lwr,
             upr = upr,
             qi_sims = qi,
             prob = prob, 
             ci_type = ci_type)
  class(qi) <- "qi"
  return(qi)
}
