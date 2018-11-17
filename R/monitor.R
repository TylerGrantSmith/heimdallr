monitor <- function(x,...) {
  UseMethod("monitor")
}

monitor.default <- function(x, ...) {
  stop("`x` should be a data.frame, tibble, or data.table")
}

monitor.data.frame <-
  function(x,
           vars = NULL,
           group_var = NULL,
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

    x <- select(x, vars, group_var)

    var_info <- tibble::tibble(variable = vars)
    var_info$source <- "original"

    out <- list(
      var_info = var_info,
      group_var = group_var,
      vars = c(vars),
      steps = list(),
      comparisons = list(),
      data = x
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

execute <- function(object, ...) {
  UseMethod("execute")
}

execute.monitor <- function(m, ...) {
  for(i in seq_along(m$steps)) {
    m <- execute(m$steps[[i]], m)
  }
  m
}
