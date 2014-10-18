find_mode <- function(x) {
  dens <- density(x)
  mode <- dens$x[which(dens$y == max(dens$y))]
  return(mode)
}

find_modes <- function(X) {
  modes <- apply(X, 2, find_mode)
  return(modes)
}
