plot_qi_1 <- function(qi, xlim = NULL, ylim = NULL, ...) {
  dens <- density(qi$qi_sims, n = 5000)
  if (is.null(xlim)) {  xlim <- range(dens$x)  }
  if (is.null(ylim)) {  ylim <- range(dens$y)  }
  compactr::eplot(xlim = xlim, ylim = ylim, ...)
  # add shaded region
  lo <- qi$lwr
  hi <- qi$upr
  x <- dens$x[dens$x < hi & dens$x > lo]
  y <- dens$y[dens$x < hi & dens$x > lo]
  zeros <- rep(0, length(y))
  polygon(c(x, rev(x)), c(y, zeros), col = "grey80", lty = 0)
  # add density
  lines(dens, lwd = 2)  
}