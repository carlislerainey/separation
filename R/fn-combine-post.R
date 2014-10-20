#' Combine objects of class post.
#'@title Combine objects of class post.
#'@description Combine ojects of class post.
#'@param ... Objects of class post to be combined.
#'
#'@export

combine_post <- function(...) {
  # combine arguments into list
  posts <- list(...)
  # check that all arguments have class post
  for (i in 1:length(posts)) {
    if (class(posts[[i]]) != "post") {
      stop("Arguments must be of class \"post.\" Make sure that all the arguments were created by one of the functions sim_post_*().")
    }
  }
  for (i in 1:length(posts)) {
    names(posts)[i] <- posts[[i]]$prior_label
  }
  class(posts) <- "posts"
  return(posts)
}