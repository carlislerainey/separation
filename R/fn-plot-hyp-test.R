#'Plots a histogram using the compact axis notation from the \code{compactr} 
#'package.
#'@title Plots a histogram using the compact axis notation from the 
#'  \code{compactr} package.
#'  
#'@description \code{plot_hyp_tests()} plots the probability of the research 
#'  hypothesis across several models the compact axis notation from the 
#'  \code{compactr} package.
#'@param posts An object of class \code{pppds}, created by the function
#'  \link{combine_post()}.
#'@param var_name The name of the variable of interest.
#'@param research_hyp The research hypothesis. Either that the coefficient for the variable of interest is
#'  positive (\code{"positive"}, \code{"p"}, or \code{"+"}) or negative (\code{"negative"}, \code{"n"}, or \code{"-"})
#'@param xlab The label for the x axis.
#'@param ... Arguments passed to \code{compact_hist()}.
#'  
#'  @export

plot_hyp_test <- function(posts, var_name, research_hyp, xlab = NULL, ...) {
  if (class(posts) != "posts") {
    stop("The argument \"posts\" must be of class \"posts\" created by the combined_posts() function.")
  }
  n_posts <- length(posts)
  p <- numeric(n_posts)
  if (research_hyp == "p" | research_hyp == "positive" | research_hyp == "+") {
    for (i in 1:n_posts) {
      coef <- posts[[i]]$mcmc[, var_name]
      p[i] <- mean(coef > 0)
    }
  }
  if (research_hyp == "n" | research_hyp == "negative" | research_hyp == "-") {
    for (i in 1:n_posts) {
      coef <- posts[[i]]$mcmc[, var_name]
      p[i] <- mean(coef < 0)
    }
  }
  if (is.null(xlab)) {
    xlab <- "Probability of Research Hypthesis"
  }
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(1, 1), mar = c(3.5, 1, 2, 1), oma = c(0, 0, 0, 0),
      xaxs = "i")
  compactr::eplot(xlim = c(0, 1), ylim = c(.8, n_posts + .5),
                  xlab = xlab, anny = FALSE, ...)
  for (i in 1:length(posts)) {
    lines(c(0, p[i]), c(i, i))
    points(p[i], i, pch = 19, cex = .7)
    text(0, i + .08*n_posts/2, posts[[i]]$prior, cex = .7, pos = 4)
    text(p[i], i + .08*n_posts/2, sprintf("%.2f", p[i]), cex = .7, pos = 2)
  }
  par(old_par)
}