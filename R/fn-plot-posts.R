#'Plots densities of model coefficients
#'@title Plots densities of model coefficients using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{plot.posts()} plots densities of model coefficients using the compact axis notation from the 
#'  \code{compactr} package.
#'@param posts An object of class \code{pppds}, created by the function \link{combine_post()}.
#'@param var_name The name variable for the coefficient to plot.
#' @param ci_type The type of confidence interval to compute. Either
#'   \code{"hpd"} (highest posterior density) or \code{"et"} (equal-tailed). 
#'   Defaults to \code{"hpd"}.
#' @param prob A numeric scalar in the interval (0,1) giving the target
#'   probability content of the intervals.
#'  @param plot_matrix_layout A vector of the form \code{c(n_row, n_col)} to indicate the 
#'  number of rows and the number of columns in the plot matrix. Defaults to a single row.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits (y1, y2) of the plot.
#' @param ... Arguments passed to \code{eplot()} in the package \code{compactr}.
#'  
#'  @export

plot.posts <- function(posts, var_name, ci_type = "hpd", prob = .9, 
                       plot_matrix_layout = NULL,
                       xlim = NULL, ylim = NULL, ...) {
  n_posts <- length(posts)
  # set plot layout
  if (is.null(plot_matrix_layout)) {
    plot_matrix_layout <- c(1, n_posts)
  }
  if (is.null(xlim)) {
    min_x <- NULL
    max_x <- NULL
    for (i in 1:n_posts) {
      min_x <- min(min_x, posts[[i]]$mcmc[, var_name])
      max_x <- max(max_x, posts[[i]]$mcmc[, var_name])
    }
    xlim <- c(min_x, max_x)
  }
  if (is.null(ylim)) {
    min_y <- 0
    max_y <- NULL
    for (i in 1:n_posts) {
      max_y <- max(max_y, density(posts[[i]]$mcmc[, var_name], n = 5000)$y)
    }
    ylim <- c(min_y, max_y)
  }
  old_par <- par(no.readonly = TRUE)
  par(mfrow = plot_matrix_layout)
  for (i in 1:n_posts) {
    plot(posts[[i]], 
              var_name = var_name,
              ci_type = ci_type, 
              prob = prob,
              xlim = xlim,
              ylim = ylim)
    # plot any needed new axix
    n_row <- plot_matrix_layout[1]
    n_col <- plot_matrix_layout[2]
    n_spots <- n_col*n_row
    current_row <- par("mfg")[1]
    current_col <- par("mfg")[2]
    current_number <- (current_row - 1)*n_col + current_col
    if (n_spots - n_col > current_number - 1 &
          current_number + n_col > n_posts) {
      compactr::addxaxis()
    }
  }
  par(old_par)
}