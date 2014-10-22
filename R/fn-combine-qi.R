#' Combine objects of class qi.
#'@title Combine objects of class qi.
#'@description Combine ojects of class qi.
#'@param ... Objects of class qi to be combined.
#'
#'@export

combine_qi <- function(...) {
  # combine arguments into list
  qis <- list(...)
  # check that all arguments have class qi
  for (i in 1:length(qis)) {
    if (class(qis[[i]]) != "qi") {
      stop("Arguments must be of class \"qi.\" Make sure that all the arguments were created by the function calc_qi().")
    }
  }
  for (i in 1:length(qis)) {
    names(qis)[i] <- qis[[i]]$fn_args$post$prior
  }
  class(qis) <- "qis"
  return(qis)
}