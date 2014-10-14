#'Add an arrow to plots of object of class pppd.
#'@title Intended for internal use only
#'@description Intended for internal use only

add_trunc_arrow <- function(from, to, ht, p) {
  arrows(x0 = from, y0 = ht, x1 = to, length = .1)
  text <- paste(round(p), "% of\nsimulations", sep = "")
  if (round(p) == 0) {
    text <- "< 1% of\nsimulations"
  }
  text((from + to)/2, ht, text, pos = 3, cex = .7)
}