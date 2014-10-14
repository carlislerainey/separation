#'Set variables at their medians
#'@title Set variables at their medians
#'  
#'@description \code{set_at_median} returns a list of the median of every
#'  variables. This function is useful for generating matrices for computing
#'  quantities of interst, such as predicted probabilities, risk-ratios, and
#'  first-differences.
#'  
#'  
#'@param x Either a data frame or a matrix with column names.
#'
#'@export

set_at_median <- function(x) {
  if (!(is.matrix(x) | is.data.frame(x))) {
    stop("The argument \"x\" must be a data frame or matrix.")
  }
  if (is.matrix(x)) {
    if (is.null(colnames(x))) {
      stop("If the argument \"x\" is a matrix, then it must have column names.")
    }
  }
  X_pred_list <- list()
  if (is.matrix(x)) {  var_names <- colnames(x)  }
  if (is.data.frame(x)) {  var_names <- names(x)  }
  n_vars <- length(var_names)
  for (i in 1:n_vars) {
    if (is.numeric(x[, var_names[i]])) {
      X_pred_list[[var_names[i]]] <- median(x[, var_names[i]], na.rm = TRUE)
    }
    else {
      warning(paste("The variable \"", var_names[i], "\" is not numeric. ", 
                    "The function set_at_median() requires numeric variables. ",
                    "I'm just skipping this variable for now, but it might lead to failures later,",
                    sep = ""))
    }
  }
  return(X_pred_list)
}

