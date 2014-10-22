plot_qis_1 <- function(qis, x = NULL, plot_matrix_layout = NULL,
                       xlim = NULL, ylim = NULL, ...) {
  n_qis <- length(qis)
  # check that all the ranges make sense
  qi_length <- numeric(n_qis)
  for (i in 1:n_qis) {
    qi_length[i] <- length(qis[[i]]$med)
  }
  lengths_match <- sum(qi_length == 1) == n_qis
  if (!lengths_match) {
    stop("It seems that all the lengths of the quantities of interest don't match.")
  }
  # set plot layout
  if (is.null(plot_matrix_layout)) {
    plot_matrix_layout <- c(1, n_qis)
  }
  if (is.null(xlim)) {
    min_x <- NULL
    max_x <- NULL
    for (i in 1:n_qis) {
      min_x <- min(min_x, min(qis[[i]]$qi_sims))
      max_x <- max(max_x, max(qis[[i]]$qi_sims))
    }
    xlim <- c(min_x, max_x)
  }
  if (is.null(ylim)) {
    min_y <- 0
    max_y <- 1
    for (i in 1:n_qis) {
      max_y <- max(max_y, density(qis[[i]]$qi_sims, n = 5000)$y)
    }
    ylim <- c(min_y, max_y)
  }
  old_par <- par(no.readonly = TRUE)
  par(mfrow = plot_matrix_layout)
  for (i in 1:n_qis) {
    plot(qis[[i]],
         xlim = xlim,
         ylim = ylim,
         main = names(qis)[i],
         ...)
    # plot any needed new axix
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
