#'Plots quantities of interest
#'@title Plots quantities of interest using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{plot.qis()} plots quantities of interest using the compact axis notation from the 
#'  \code{compactr} package.
#'@param qis An object of class \code{pppds}, created by the function \link{combine_qi()}.
#'@param x_vals The x values across which the quantities of interest vary
#'  @param plot_matrix_layout A vector of the form \code{c(n_row, n_col)} to indicate the 
#'  number of rows and the number of columns in the plot matrix. Defaults to a single row.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits (y1, y2) of the plot.
#' @param ... Arguments passed to \code{eplot()} in the package \code{compactr}.
#'  
#'  @export

plot.qis <- function(qis, x_vals = NULL, 
                     plot_matrix_layout = NULL,
                     xlim = NULL, ylim = NULL, ...) {
  n_qis <- length(qis)
  # check type of qi (one or range)
  qi_length <- numeric(n_qis)
  for (i in 1:n_qis) {
    qi_length[i] <- length(qis[[i]]$med)
  }
  if (!is.null(x_vals)) {
    x_vals_gt_1 <- length(x_vals) > 1
    if (!x_vals_gt_1) {
      stop("If supplied, then \"x_vals\" must be a vector longer than one.")
    }
    lengths_match <- sum(qi_length == length(x_vals)) == n_qis
    if (!lengths_match) {
      stop("Either the lengths of quantities of interest don't match \"x_vals\" or each other.")
    }
    if (x_vals_gt_1 & lengths_match) {
      x_vals_range <- TRUE
    }
  } else {
    x_vals_range <- FALSE
  }
  if (x_vals_range == TRUE) {
    plot_qis_range(qis = qis,
                   x_vals = x_vals, 
                   plot_matrix_layout = plot_matrix_layout,
                   xlim = xlim,
                   ylim = ylim,
                   ...)
  }
  if (x_vals_range == FALSE) {
    plot_qis_1(qis = qis,
               plot_matrix_layout = plot_matrix_layout,
               xlim = xlim,
               ylim = ylim,
               ...)
  }
}