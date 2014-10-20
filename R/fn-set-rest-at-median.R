#'Vary one variable and set the rest at their medians
#'@title Vary one variable and set the rest at their medians
#'  
#'@description \code{set_rest_at_median} returns a list of the median of every 
#'  variables with onve variable taking on a given range of values. This
#'  function is useful for generating matrices for computing quantities of
#'  interst, such as predicted probabilities, risk-ratios, and 
#'  first-differences.
#'  
#'  
#'@param formula A model formula.
#'@param data A data frame.
#'@param var_name The name of the variable to be varied.
#'@param var_values The values that \code{var_name} should take.
#'
#'@export

set_rest_at_median <- function(formula, data, var_name, var_values) {
  X_pred_list <- set_at_median(formula, data)
  if (!(var_name %in% names(X_pred_list))) {
    stop(var_name, " doesn't seem to be in the model.")
  }
  X_pred_list[[var_name]] <- var_values
  return(X_pred_list)
}