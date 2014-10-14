#' Combine objects of class pppd.
#'@title Combine objects of class pppd.
#'@description Combine ojects of class pppd.
#'@param ... Objects of class pppd to be combined.
#'
#'@export

combine_pppd <- function(...) {
  # combine arguments into list
  pppds <- list(...)
  # check that all arguments have class pppd
  for (i in 1:length(pppds)) {
    if (class(pppds[[i]]) != "pppd") {
      stop("Arguments must be of class \"pppd.\" Make sure that all the arguments were created by the function calc_pppd().")
    }
  }
  for (i in 1:length(pppds)) {
    names(pppds)[i] <- pppds[[i]]$prior_label
  }
  class(pppds) <- "pppds"
  return(pppds)
}