reporter <- function(subclass, ..., .prefix = "report_") {
  out <- structure(list(...), class = c(paste0(.prefix, subclass), "reporter"))
  out
}

print.reporter <- function() {

}

tidy.reporter <- function() {

}

add_reporter <- function(m, reporter) {
  m$steps[[length(m$steps) + 1]] <- reporter
  m
}

report_anomalies <- function(
  m, ...,
  method = c("stl", "twitter"),
  focus = m$watch_vars,
  role = NULL,
  initialized = FALSE,
  skip = FALSE,
  id = rand_id("report_anomalies")) {



  m <- add_reporter(m,
                    report_anomalies_new(
                      id = id,
                      role = role,
                      initialized = initialized,
                      method = method,
                      focus = focus))
  m
}

report_anomalies_new <- function(
  id,
  role,
  initialized,
  method,
  focus) {
  reporter(
    subclass = "anomalies",
    id = id,
    role = role,
    initialized = initialized,
    method = method,
    focus = focus
  )
}

initialize.report_anomalies <- function(c) {
  c
}

print.report_anomalies <- function(x,
                                   width = max(20, options()$width - 35),
                                   ...) {
  cat(glue::glue("Report Anomalies: {x$method}"))
  if(x$initialized) {
    cat(" [initialized]\n")
  }
  else {
    cat ("\n")
  }
  invisible(x)
}

library(foreach)

execute.report_anomalies <- function(object, m) {
  m$reports[[object$id]] <-
    foreach(t = m$trackers, .combine = 'c') %:%
    foreach(v = names(t), .combine = 'c') %do% {
      generate_anomalies(t[[v]], object$method)
    }
  m
}


generate_anomalies <- function(t, method) {
  t %>%
    as.tibble() %>%
    mutate(D_RECORD_CREATE = as.Date(D_RECORD_CREATE)) %>%
    anomalize::time_decompose(DISTANCE, method = method) %>%
    anomalize::anomalize(remainder, alpha = 0.1) %>%
    anomalize::time_recompose()
}
