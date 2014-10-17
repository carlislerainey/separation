# internal function
log_post_gelman <- function(beta, scale_coef, scale_int, X, y) {
  beta <- Matrix::Matrix(beta)
  p <- plogis(matrix(X%*%beta))
  lp <- sum(y*log(p)) + sum((1-y)*log(1 - p)) + 
    log(dcauchy(beta[1], scale = scale_int)) + 
    sum(log(dcauchy(beta[2:length(beta)], scale = scale_coef)))
  return(lp)
}