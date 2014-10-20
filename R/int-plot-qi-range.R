plot_qi_range <- function(qi, x, xlim = NULL, ylim = NULL, ...) {
  if (is.null(xlim)) {  xlim <- range(x)  }
  if (is.null(ylim)) {  ylim <- range(qi$lwr, qi$upr)  }
  compactr::eplot(xlim = xlim, ylim = ylim, ...)
  lines(x, qi$med, lty = 1, lwd = 2)
  lines(x, qi$lwr, lty = 3)
  lines(x, qi$upr, lty = 3)
}