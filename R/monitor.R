#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
monitor <- function(x,...) {
  UseMethod("monitor")
}

#' @export
monitor.default <- function(x, ...) {
  stop("`x` should be a data.frame, tibble, or data.table")
}


#' @rdname monitor
#'
#' @return
#' @export
#'
#' @examples
monitor.data.frame <-
  function(x,
           watch_vars = NULL,
           group_var = NULL,
           ...) {

    if(is.null(group_var)) {
      group_var = colnames(x)[[1]]
      warning(glue::glue("`group_var` missing.  Using {group_var} as grouping variable."), call. = F)
    }

    if(!(group_var %in% colnames(x))) {
      stop(glue::glue("`group_var` value {group_var} is not a valid column in `x`",call. = F))
    }

    if(is.null(watch_vars)) {
      watch_vars <- setdiff(colnames(x), group_var)
    }

    if(length(group_var) > 1 ||
       !is.character(group_var) ||
       !(group_var %in% colnames(x)))
      stop("`group_var` should be a column name")

    if(any(table(watch_vars)) > 1)
      stop("`vars` should have unique members", call. = FALSE)

    if(any(!(watch_vars %in% colnames(x))))
      stop(paste0("The following elements of `watch_vars` are not valid columns in `x`: ",
                  paste0(setdiff(watch_vars, colnames(.x)), collapse = ", ")),
           call. = FALSE)

    if(group_var %in% watch_vars) {
      message(glue::glue("Removing {group_var} from watch_vars list"))
      watch_vars <- setdiff(watch_vars, group_var)
    }

    all_var <- c(watch_vars, group_var)
    x <- dplyr::select(x, one_of(all_var))

    var_info <- dplyr::tibble(variable = all_var)
    var_info$source <- "original"
    var_info$role <- "watch"
    var_info$role[[which(var_info$variable == group_var)]] <- "group"

    out <- list(
      data = x,
      var_info = var_info,
      watch_vars = watch_vars,
      group_var = group_var,
      initialized = F,
      steps = list(),
      trackers = list()
    )

    class(out) <- "monitor"
    out
  }

#' @export
is.monitor <- function(m) {
  "monitor" %in% class(m)
}

#' Title
#'
#' @param m
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
initialize <- function(m, ...) {
  UseMethod("initialize")
}

#' @export
initialize.monitor <- function(m,...) {
  out <- m
  cat("Generating densities...")
  out$densities <- map2(m$watch_vars, m$group_var, ~fast_density(m$data, .x, .y)) %>% set_names(m$watch_vars)
  cat("Done\n")
  out$initialized <- T
  out
}

#' @export
print.monitor <- function(m,...) {
  cat(crayon::cyan("Monitoring:\n"))
  cat(crayon::cyan("Steps:\n"))
  for(i in seq_along(m$steps)) {
    cat(glue::glue_col("    {green {i}}: "))
    print(m$steps[[i]])
  }
  invisible(m)
}

#' Title
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
execute <- function(object, ...) {
  UseMethod("execute")
}

#' @export
execute.monitor <- function(m, ...) {
  if(!m$initialized)
    stop("Initialize the monitor before executing")

  for(i in seq_along(m$steps)) {
    m <- execute(m$steps[[i]], m)
  }
  m
}
