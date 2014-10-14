#'Plots a histogram using the compact axis notation from the \code{compactr} 
#'package.
#'@title Plots a histogram using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{plot.pppd()} plots a histogram using the compact axis 
#'  notation from the \code{compactr} package.
#'@param pppd An object of class \code{pppd}, created by the function \link{calc_pppd()}.
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
#'  @param arrow_ht The height of the information about truncation. Defaults to 
#'  0.2. The bottom of the plot is 0.0 and the top is 1.0.
#'  @param ... Arguments passed to \code{compact_hist()}.
#'  
#'  @export


plot.pppd <- function(pppd, qi_name = "pr", n_breaks = 50, log_scale = FALSE, 
                      upper = NULL, lower = NULL, arrow_ht = 0.2, ...) {
qi <- pppd[[qi_name]]
# truncate qi and compute the number of truncated simulations ----------------
percent_qi_trunc_right <- NULL
if (!is.null(upper)) {
  percent_qi_trunc_right <- 100*sum(qi > upper)/length(qi)
  if (percent_qi_trunc_right > 0) {
    message("Your choice of upper bound truncates ", round(percent_qi_trunc_right), 
            "% of the simulations to the right. I'll try to include this information on the plot.")
  }
  if (percent_qi_trunc_right == 0) {
    percent_qi_trunc_right <- NULL
  }
  qi <- qi[qi < upper]
}
percent_qi_trunc_left <- NULL
if (!is.null(lower)) {
  percent_qi_trunc_left <- 100*sum(qi < lower)/length(qi)
  if (percent_qi_trunc_left > 0) {
    message("Your choice of lower bound truncates ", round(percent_qi_trunc_left), 
            "% of the simulations to the left. I'll try to include this information on the plot.")
  }
  if (percent_qi_trunc_left == 0) {
    percent_qi_trunc_left <- NULL
  }
  qi <- qi[qi > lower]
}  
# add in the long names of the quantities of interest ------------------------
qi_long_name <- list(
  rr = "Risk-Ratio",
  fd = "First-Difference",
  pr = "Predicted Probability")
# save options and update scipen so we don't plot scientific notation---------
old_options <- options()
options(scipen=10000)
# plot the histogram ---------------------------------------------------------
compact_hist(qi, n_breaks = n_breaks, log_scale = log_scale,
             xlab = qi_long_name[[qi_name]],
             main = pppd$prior_label)
# restore old options --------------------------------------------------------
options <- old_options
# add arrows and summary of truncation to plot -------------------------------
left <- par("usr")[1]
right <- par("usr")[2]
height <- par("usr")[4]
if (!is.null(percent_qi_trunc_right)) {
  if (percent_qi_trunc_right > 0) {
    add_trunc_arrow(from = left + 0.85*(right - left), 
                    to = left + 0.93*(right - left), 
                    ht = arrow_ht*height, 
                    p = percent_qi_trunc_right)
  }
}
if (!is.null(percent_qi_trunc_left)) {
  if (percent_qi_trunc_left > 0) {
    add_trunc_arrow(from = left + 0.15*(right - left), 
                    to = left + 0.07*(right - left), 
                    ht = arrow_ht*height, 
                    p = percent_qi_trunc_left)
  }
}
}

