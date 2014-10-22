plot_qis_range <- function(qis, x_vals, plot_matrix_layout = NULL,
                           xlim = NULL, ylim = NULL, ...) {
  n_qis <- length(qis)
  # check that all the ranges make sense
  qi_length <- numeric(n_qis)
  for (i in 1:n_qis) {
    qi_length[i] <- length(qis[[i]]$med)
  }
  lengths_match <- sum(qi_length == length(x_vals)) == n_qis
  if (!lengths_match) {
    stop("It seems that all the lengths of the quantities of interest don't match the length of \"x_vals.\"")
  }
  # set plot layout
  if (is.null(plot_matrix_layout)) {
    plot_matrix_layout <- c(1, n_qis)
  }
  if (is.null(xlim)) {
    min_x <- min(x_vals)
    max_x <- max(x_vals)
    xlim <- c(min_x, max_x)
  }
  if (is.null(ylim)) {
    min_y <- NULL
    max_y <- NULL
    for (i in 1:n_qis) {
      min_y <- min(min_y, min(qis[[i]]$lwr))
      max_y <- max(max_y, max(qis[[i]]$upr))
    }
    ylim <- c(min_y, max_y)
  }
  old_par <- par(no.readonly = TRUE)
  par(mfrow = plot_matrix_layout)
  for (i in 1:n_qis) {
    compactr::eplot(xlim = xlim,
         ylim = ylim,
         main = names(qis)[i],
         ...)
    lines(x_vals, qis[[i]]$med, lwd = 2)
    lines(x_vals, qis[[i]]$lwr, lty = 3)
    lines(x_vals, qis[[i]]$upr, lty = 3)
    
    # plot any needed new axis
    n_row <- plot_matrix_layout[1]
    n_col <- plot_matrix_layout[2]
    n_spots <- n_col*n_row
    current_row <- par("mfg")[1]
    current_col <- par("mfg")[2]
    current_number <- (current_row - 1)*n_col + current_col
    if (n_spots - n_col > current_number - 1 &
          current_number + n_col > n_qis) {
      compactr::addxaxis()
    }
  }
  par(old_par)
}