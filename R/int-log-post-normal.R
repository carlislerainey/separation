# internal function
log_post_normal <- function(beta, which_var_sep, sd, X, y) {
  beta <- Matrix::Matrix(beta)
  p <- plogis(matrix(X%*%beta))
  loglik <- sum(y*log(p)) + sum((1-y)*log(1 - p))
  logprior <- log(dnorm(beta[which_var_sep], mean = 0, sd = sd))
  logpost <- loglik + logprior
  return(logpost)
}