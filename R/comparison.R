comparison <- function(subclass, ..., .prefix = "compare_") {
  structure(list(...),
            class = c(paste0(.prefix, subclass), "comparison"))
}

print.comparison <- function() {

}

tidy.comparison <- function() {

}

add_comparison <- function(m, comparison) {
  m$steps[[length(m$steps) + 1]] <- comparison
  m
}

compare_distribution <- function(
  m, ...,
  role = NA,
  initialized = FALSE,
  skip = FALSE,
  method = "jsd",
  id = rand_id("compare_distribution")) {
  add_comparison(m,
                 compare_distribution_new(
                   id = id,
                   role = role,
                   initialized = initialized,
                   method = method,
                   ...))
}

compare_distribution_new <- function(
  id,
  role,
  initialized,
  method,
  ...) {
  comparison(
    subclass = "distribution",
    id = id,
    role = role,
    initialized = initialized,
    method = method,
    fixed = fixed,
    lag = lag
  )
}

initialize.compare_distribution <- function(c) {
  c
}

print.compare_distribution <- function(...) {

}

execute.compare_distribution <- function(object, m) {
  m$comparisons[[object$id]] <-
    map2(m$densities,
         m$watch_vars,
        ~generate_jsd(.x,
                      .y,
                      m$group_var,
                      method = "shift",
                      type = "lag",
                      n = 1)) %>%
    set_names(m$watch_vars)
  m
}
