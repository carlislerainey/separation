find_mode <- function(x) {
  dens <- density(x, n = 5000)
  mode <- dens$x[which(dens$y == max(dens$y))]
  mode <- median(mode)
  return(mode)
}

find_modes <- function(X) {
  modes <- apply(X, 2, find_mode)
  return(modes)
}
