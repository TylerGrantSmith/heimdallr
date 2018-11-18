generate_frequency <- function(x, grp, var) {
  x[,.N, by = c(grp, var)]
}

fast_density <- function(x, grp, var) {
  x <- generate_frequency(x, grp, var)
  setDT(x)[, {N_SUM = sum(N); .SD[, .(N, PROP = N / N_SUM), by = c(grp)]}, by = c(var)]
}
