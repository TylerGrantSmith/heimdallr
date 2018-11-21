#' Title
#'
#' @param subclass
#' @param ...
#' @param .prefix
#'
#' @return
#' @export
#'
#' @examples
tracker <- function(subclass, ..., .prefix = "track_") {
  out <- structure(list(...), class = c(paste0(.prefix, subclass), "tracker"))
  out
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
print.tracker <- function() {

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tidy.tracker <- function() {

}

add_tracker <- function(m, tracker) {
  m$steps[[length(m$steps) + 1]] <- tracker
  m
}

#' Title
#'
#' @param m
#' @param ...
#' @param distance_method
#' @param method
#' @param options
#' @param focus
#' @param role
#' @param initialized
#' @param skip
#' @param id
#'
#' @return
#' @export
#'
#' @examples
track_distribution <- function(
  m, ...,
  distance_method = "jensen-shannon",
  method = "fix",
  options = list(),
  focus = m$watch_vars,
  role = NULL,
  initialized = FALSE,
  skip = FALSE,
  id = rand_id("track_distribution")) {



  m <- add_tracker(m,
                   track_distribution_new(
                     id = id,
                     role = role,
                     initialized = initialized,
                     distance_method = distance_method,
                     method = method,
                     options = options,
                     focus = focus))
  m
}

track_distribution_new <- function(
  id,
  role,
  initialized,
  distance_method,
  method,
  options,
  focus) {
  tracker(
    subclass = "distribution",
    id = id,
    role = role,
    initialized = initialized,
    distance_method = distance_method,
    method = method,
    options = options,
    focus = focus
  )
}

#' Title
#'
#' @param c
#'
#' @return
#' @export
#'
#' @examples
initialize.track_distribution <- function(c) {
  c
}

#' Title
#'
#' @param x
#' @param width
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.track_distribution <- function(x,
                                     width = max(20, options()$width - 35),
                                     ...) {
  cat(glue::glue("Track Distributional Change: {x$distance_method}, {stringr::str_to_title(x$method)}"))
  if(x$initialized) {
    cat(" [initialized]\n")
  }
  else {
    cat ("\n")
  }
  invisible(x)
}

#' Title
#'
#' @param object
#' @param m
#'
#' @return
#' @export
#'
#' @examples
execute.track_distribution <- function(object, m) {
  m$trackers[[object$id]] <-
    map2(m$densities,
         m$watch_vars,
         ~generate_jsd(.x,
                       .y,
                       m$group_var,
                       object$distance_method,
                       object$method,
                       object$options)) %>%
    setNames(m$watch_vars)
  m
}
