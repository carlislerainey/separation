#' @title Plot a quantity of interest
#'   
#' @description \code{plot.qi()} plots an object of class "qi," eithering giving a 
#' density plot of the quantity of interest is for a single qi (i.e., a single 
#' predicted probability, risk-ratio, or first-difference) or a plot of the 
#' predicted probability and credible intervals if the qi is a range of predicted
#' probabilities.
#' 
#' 
#' @param qi An object of class "qi" created by the \code{calc_qi()} function.
#' @param x A numeric variable with which the qi changes. Usually, this is the
#' element of \code{X_pred_list} given to \code{calc_qi()} that varies.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits (y1, y2) of the plot.
#' @param ... Arguments passed to \code{eplot()} in the package \code{compactr}.
#'   
#' @export

plot.qi <- function(qi, x = NULL, xlim = NULL, ylim = NULL, ...) {
  if (length(qi$med) == 1) {
    plot_qi_1(qi = qi, xlim = xlim, ylim = ylim, ...)
  }
  if (length(qi$med) > 1) {
    if (is.null(x)) {
      stop("The argument \"x\" is required in this case.")
    }
    plot_qi_range(qi = qi, x = x, xlim = xlim, ylim = ylim, ...)
  }
}