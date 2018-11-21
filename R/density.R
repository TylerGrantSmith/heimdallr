
generate_frequency <- function(x, grp, var) {
  UseMethod("generate_frequency")
}

generate_frequency.tbl_lazy <- function(x, grp, var) {
  x %>%
    group_by(!!enquo(grp)) %>%
    count(!!enquo(var)) %>%
    rename(N = n) %>%
    as.data.table()
}

#' @importFrom data.table := .N
generate_frequency.default <- function(x, grp, var) {
  .N <- NULL
  data.table::setDT(x)[,.N, by = c(grp, var)]
}

fast_density <- function(x, grp, var) {
  x <- generate_frequency(x, grp, var)
  x[, {N_SUM = sum(N); .SD[, .(N, PROP = N / N_SUM), by = c(grp)]}, by = c(var)]
}
