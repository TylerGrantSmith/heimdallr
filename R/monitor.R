monitor <- function(x,...) {
  UseMethod("monitor")
}

monitor.default <- function(x, ...) {
  stop("`x` should be a data.frame, tibble, or data.table")
}

monitor.data.frame <-
  function(x,
           watch_vars = NULL,
           group_var = NULL,
           ...) {

    if(is.null(watch_vars)) {
      watch_vars <- colnames(x)
    }

    if(is.null(group_var))
      group_var = purrr::pluck(colnames(x), 1)

    if(length(group_var) > 1 ||
       !is.character(group_var) ||
       !(group_var %in% colnames(x)))
      stop("`group_var` should be a column name")


    if(any(table(watch_vars)) > 1)
      stop("`vars` should have unique members", call. = FALSE)

    if(any(!(watch_vars %in% colnames(x))))
      stop(paste0("The following elements of `vars` are not in `x`: ",
                  paste0(setdiff(watch_vars, colnames(.x)), collapse = ", ")),
           call. = FALSE)

    if(group_var %in% watch_vars) {
      message(glue::glue("Removing {group_var} from watch_vars list"))
      watch_vars <- setdiff(watch_vars, group_var)
    }

    variables <- c(watch_vars, group_var)
    x <- select(x, one_of(variables))
    var_info <- tibble::tibble(variable = variables)
    var_info$source <- "original"
    var_info$role <- "watch"
    var_info$role[var_info$variable == group_var] <- "group"

    out <- list(
      data = x,
      var_info = var_info,
      watch_vars = watch_vars,
      group_var = group_var,
      steps = list(),
      comparisons = list(),
      distributions = list()
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
  out$densities <- map2(m$watch_vars, m$group_var, ~fast_density(m$data, .x, .y)) %>% set_names(m$watch_vars)
  out$initialized <- T
  out
}

print.monitor <- function(m,...) {
  print(m$var_info)
}

# get_data <- function(.tbl, .grp, .var, .flt, sort = T) {
#   x <-
#     .tbl %>%
#     filter(!!!.flt) %>%
#     mutate(var = !!.var, groupvar = !!.grp) %>%
#     group_by(groupvar) %>%
#     count(var) %>%
#     collect() %>%
#     ungroup() %>%
#     as.data.table()
#
#   gc()
#
#   if(sort) { setorderv(x, c("groupvar","var")) }
#
#   return(x)
# }

execute <- function(object, ...) {
  UseMethod("execute")
}

execute.monitor <- function(m, ...) {
  if(!m$initialized)
    stop("Initialize the monitor before executing")

  for(i in seq_along(m$steps)) {
    m <- execute(m$steps[[i]], m)
  }
  m
}
