#' @title Plot a quantity of interest
#'   
#' @description \code{plot.post()} produces a density plot for a single
#'   coefficient from an object of class "post."
#' 
#' 
#' @param post An object of class "post" created by one of the \code{sim_post_*()} functions.
#' @param var_name The name of the variable to plot.
#' @param ci_type The type of confidence interval to compute. Either
#'   \code{"hpd"} (highest posterior density) or \code{"et"} (equal-tailed). 
#'   Defaults to \code{"hpd"}.
#' @param prob A numeric scalar in the interval (0,1) giving the target
#'   probability content of the intervals.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits (y1, y2) of the plot.
#' @param main The title of the plot.
#' @param ... Arguments passed to \code{eplot()} in the package \code{compactr}.
#'   
#' @export

plot.post <- function(post, var_name, ci_type = "hpd", prob = .9, main = NULL, xlim = NULL, ylim = NULL, ...) {
  coef <- post$mcmc[, var_name]
  dens <- density(coef, n = 5000)
  if (is.null(xlim)) {  xlim <- range(dens$x)  }
  if (is.null(ylim)) {  ylim <- range(dens$y)  }
  if (is.null(main)) { main <- post_inf$prior }
  compactr::eplot(xlim = xlim, ylim = ylim, main = main, ...)
  # add shaded region
  median_coef <- median(coef)
  if (ci_type == "hpd") {
    q_coef <- coda::HPDinterval(coda::as.mcmc(coef), prob)
    lwr <- q_coef[1]
    upr <- q_coef[2]
  }
  if (ci_type == "et") {
    p_lo <- (1 - prob)/2
    p_hi <- 1 - (1 - prob)/2
    q_coef <- quantile(coef, c(p_lo, p_hi))
    lwr <- q_coef[1]
    upr <- q_coef[2]
  }
  x <- dens$x[dens$x < upr & dens$x > lwr]
  y <- dens$y[dens$x < upr & dens$x > lwr]
  zeros <- rep(0, length(y))
  polygon(c(x, rev(x)), c(y, zeros), col = "grey80", lty = 0)
  # add density
  lines(dens, lwd = 2)  
}
