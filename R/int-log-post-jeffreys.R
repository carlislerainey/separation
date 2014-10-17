# internal function
log_post_jeffreys <- function(beta, X, y) {
  beta <- Matrix::Matrix(beta)
  p <- plogis(matrix(X%*%beta))
  W <- Matrix::Matrix(0, nrow = length(p), ncol = length(p))
  diag(W) <- p*(1 - p)
  I <- Matrix::crossprod(X, W)%*%X
  lp <- sum(y*log(p)) + sum((1-y)*log(1 - p)) + 0.5*log(Matrix::det(I))
  return(lp)
}