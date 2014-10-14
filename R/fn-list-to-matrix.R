#' Convert a list to a matrix
#'@title Convert a list to a matrix
#'  
#'@description \code{list_to_matrix} takes a list of scalars and possibly one 
#'  vector and returns a matrix. If the list contains only scalars, then 
#'  \code{list_to_matrix} returns a matrix with one row. If the list contains a
#'  vector, then \code{list_to_matrix} returns a matrix with the number of rows
#'  equal to the number of elements in the vector. This must be a named list and it
#'  must contain only one vector and the rest be scalars.
#'  
#'  
#'@param list A named list with scalars and up to one vector.
#'
#'@export


list_to_matrix <- function(list) {
  # check arguments
  if (is.list(list)) {
    if (is.null(names(list))) {
      stop("The argument \"list\" must be a *named* list. ",
           "It is a list, but doesn't have names.")
    }
  }
  else {
    stop("The argument \"list\" must be a list.")
  }
  # check that elements of list satisfy constraints
  n_elements <- length(list)
  length_elements <- numeric(n_elements)
  for (i in 1:n_elements) {
    if (!is.numeric(list[[i]])) {
      stop("The elements of the argument \"list\" must be numeric. ",
           "Print the argument and see what it looks like.")
    }
    if (!is.vector(list[[i]])) {
      stop("The elements of the argument \"list\" must be scalars or vectors. ",
           "Print the argument and see what it looks like.")
    }
    length_elements[i] <- length(list[[i]])
  }
  if (sum(length_elements > 1) > 1) {
    stop("Only one element of the argument \"list\" can be a vector. ",
         "Print the argument and see what it looks like.")
  }
  # build prediction matrix
  n_row <- max(length_elements)
  n_col <- length(list)
  X_pred_mat <- matrix(NA, nrow = n_row, ncol = n_col)
  for (i in 1:n_elements) {
    X_pred_mat[, i] <- list[[i]]
  }
  colnames(X_pred_mat) <- names(list)
  return(X_pred_mat)
} 