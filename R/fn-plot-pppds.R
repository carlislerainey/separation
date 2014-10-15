#'Plots a histograms using the compact axis notation from the \code{compactr} 
#'package.
#'@title Plots histograms using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{plot.pppds()} plots histograms using the compact axis 
#'  notation from the \code{compactr} package.
#'@param pppds An object of class \code{pppds}, created by the function \link{combine_pppd()}.
#'@param qi_name The name of the quantity of interest to plot. Defaults to \code{"pr"}. 
#'The other options are \code{"fd"} and \code{"rr"}.
#'\enumerate{ \item A single number giving the number of
#'  cells for the histogram. Defaults to 50. For examining the partial prior
#'  predictive distritution, too many breaks are preferable to too few.
#'  \item A character string naming an algorithm to
#'  compute the number of cells. See documentation for
#'  \code{hist()} for details and other algorithms. 
#'  \item A vector giving the
#'  breakpoints between histogram cells. 
#'  \item A function to compute the vector
#'  of breakpoints. 
#'  \item A function to compute the number of cells.
#'  }
#'  @param log_scale Logical; should the quantity of interest be plotted on the log scale?
#'    Defaults to \code{FALSE}. I recommend setting to \code{TRUE} if the distribution 
#'    has a long right tail, as can happen with risk-ratios in certain situations.
#'  @param upper The upper bound of quantity of interest to plot.
#'  @param lower The lower bound of quantity of interest to plot.
#'  @param plot_matrix_layout A vector of the form \code{c(n_row, n_col)} to indicate the 
#'  number of rows and the number of columns in the plot matrix. Defaults to a single row.
#'  @param arrow_ht The height of the information about truncation. Defaults to 
#'  0.2. The bottom of the plot is 0.0 and the top is 1.0.
#'  @param ... Arguments passed to \code{compact_hist()}.
#'  
#'  @export

plot.pppds <- function(pppds, qi_name = "pr", n_breaks = 50, log_scale = FALSE, 
                       upper = NULL, lower = NULL, plot_matrix_layout = NULL, 
                       arrow_ht = 0.2, ...) {
  # the number of pppds
  n_pppds <- length(pppds)
  # store all qis in a list
  qi <- list()
  for (i in 1:n_pppds) {
    qi[[i]] <- pppds[[i]][[qi_name]]
  }
  # find global lower
  if (is.null(lower)) {
    for (i in 1:n_pppds) {
      lower <- min(lower, min(qi[[i]]))
    }
  }
  # find global upper
  if (is.null(upper)) {
    for (i in 1:n_pppds) {
      upper <- max(upper, max(qi[[i]]))
    }
  }
  if (is.null(plot_matrix_layout)) {
    plot_matrix_layout <- c(1, n_pppds)
  }
  
  par(mfrow = plot_matrix_layout)
  for (i in 1:n_pppds) {
    plot(pppds[[i]], upper = upper, lower = lower,
         qi_name = qi_name, n_breaks = n_breaks, log_scale = log_scale, 
         arrow_ht = arrow_ht, ...)
    # plot any needed new axix
    n_row <- plot_matrix_layout[1]
    n_col <- plot_matrix_layout[2]
    n_spots <- n_col*n_row
    current_row <- par("mfg")[1]
    current_col <- par("mfg")[2]
    current_number <- (current_row - 1)*n_col + current_col
    if (n_spots - n_col > current_number - 1 &
          current_number + n_col > n_pppds) {
      compactr::addxaxis()
    }
  }
}

