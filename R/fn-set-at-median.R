#'Set variables at their medians
#'@title Set variables at their medians
#'  
#'@description \code{set_at_median} returns a list of the median of every
#'  variables. This function is useful for generating matrices for computing
#'  quantities of interst, such as predicted probabilities, risk-ratios, and
#'  first-differences.
#'  
#'  
#'@param formula A model formula.
#'@param data A data frame.
#'
#'@export

set_at_median <- function(formula, data) {
  mf <- model.frame(formula, data)
  df <- mf[, -1]
  X_pred_list <- list()
  var_names <- names(df)
  n_vars <- length(var_names)
  for (i in 1:n_vars) {
    if (is.numeric(df[, var_names[i]])) {
      X_pred_list[[var_names[i]]] <- median(df[, var_names[i]], na.rm = TRUE)
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