monitor <- function(x,...) {
  UseMethod("monitor")
}

monitor.default <- function(x, ...) {
  stop("`x` should be a data.frame, tibble, or data.table")
}

monitor.data.frame <-
  function(x,
           vars = NULL,
           by = NULL,
           ...) {

    if(is.null(vars)) {
      vars <- colnames(x)
    }

    if(any(table(vars)) > 1)
      stop("`vars` should have unique members", call. = FALSE)

    if(any(!(vars %in% colnames(x))))
      stop(paste0("The following elements of `vars` are not in `x`: ",
                  paste0(setdiff(vars,colnames(.x)),collapse=", ")),
           call. = FALSE)

    x <- x[, vars]

    var_info <- tibble::tibble(variable = vars)
    var_info$source <- "original"

    out <- list(
      var_info = var_info,
      term_info = var_info,
      by_var = NULL,
      comparisons = NULL,
      descriptions = NULL,
      steps = NULL,
      template = x,
      levels = NULL,
      retained = NA,
      initialized = F
    )

    class(out) <- "monitor"
    out
  }

is.monitor <- function(m) {
  "monitor" %in% class(m)
}

initialize <- function(m, ...) {
  UseMethod("initialize")
}

initialize.monitor <- function(m,...) {
  out <- m
  out$initialized <- T
  out
}

print.monitor <- function(m,...) {
  print(m$var_info)
}

get_data <- function(.tbl, .grp, .var, .flt, sort = T) {
  x <-
    .tbl %>%
    filter(!!!.flt) %>%
    mutate(var = !!.var, groupvar = !!.grp) %>%
    group_by(groupvar) %>%
    count(var) %>%
    collect() %>%
    ungroup() %>%
    as.data.table()

  gc()

  if(sort) { setorderv(x, c("groupvar","var")) }

  return(x)
}

execute.monitor <- function(m, ...) {
  get_data()

  for(i in seq_along(m$steps)) {
    execute(m$steps[[i]])
  }
}
