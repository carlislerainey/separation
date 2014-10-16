#'Plots a histogram using the compact axis notation from the \code{compactr} 
#'package.
#'@title Plots a histogram using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{compact_hist()} plots a histogram using the compact axis 
#'  notation from the \code{compactr} package.
#'  
#'  
#'@param x A vector of values for which the histogram is desired.
#'@param xlab A label for the x axis. Defaults to no label.
#'@param ylab A label for the y axis. Defaults to "Counts" if \code{counts = 
#'  TRUE} and "Density" if \code{counts = FALSE}.
#'@param n_breaks one of: 
#'\enumerate{ \item A single number giving the number of
#'  cells for the histogram. 
#'  \item A character string naming an algorithm to
#'  compute the number of cells. Defaults to "Sturges." See documentation for
#'  \code{hist()} for details and other algorithms. 
#'  \item A vector giving the
#'  breakpoints between histogram cells. 
#'  \item A function to compute the vector
#'  of breakpoints. 
#'  \item A function to compute the number of cells.
#'  }
#'  @param ht The height of the y axis.
#'  @param xlim The limits of the x axis.
#'  @param counts Logical; should the heights be counts. If \code{FALSE}, then 
#'  probabilities are used instead.
#'  @param bar_color The color of the bars. Defaults to \code{"grey70"}.
#'  @param log_scale Logical; should the x axis be plotted on the log scale?
#'    Defaults to \code{FALSE}.
#'  @param plot Logical. Should the histgram be plotted? If FALSE, the histogram is not 
#'  plotted and the data for the histogram are returned.
#'  @param ... Arguments passed to \code{eplot()} in the package \code{compactr}.

compact_hist <- function(x, xlab = NULL, ylab = NULL, n_breaks = NULL, ht = NULL,
                         xlim = NULL, counts = TRUE, bar_color = "grey70", 
                         log_scale = FALSE, plot = TRUE, ...) {
  if (is.null(n_breaks)) {
    n_breaks <- "Sturges"
  }
  if (log_scale) {
    log <- "x"
    h <- hist(log(x), plot = FALSE, breaks = n_breaks)
    h$breaks <- exp(h$breaks)
  } else {
    log <- ""
    h <- hist(x, plot = FALSE, breaks = n_breaks)
  }
  if (is.null(ht)) {
    if (counts) {
      ht <- max(h$counts)
      if (is.null(ylab)) {
        ylab <- "Counts"
      }
    } else {
      ht <- max(h$density)
      if (is.null(ylab)) {
        ylab <- "Counts"
      }
    }
  }
  if (is.null(xlim)) {
    xlim <- range(h$breaks)
  }
  if (plot == TRUE) {
    compactr::eplot(xlim = xlim, 
                    ylim = c(0, ht), 
                    log = log, 
                    xlab = xlab,
                    ylab = ylab,
                    ...) 
    plot(h, freq = counts, add = TRUE,
         border = NA, col = bar_color)
  }
  if (plot == FALSE) {
    return(h)
  }
}