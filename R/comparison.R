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
  m$comparisons[[length(m$comparisons)+1]] <- comparison
  m
}

compare_distribution <- function(
  m, ...,
  role = NA,
  initialized = FALSE,
  skip = FALSE,
  id = rand_id("compare_distribution"),
  method = "jsd",
  fixed = NULL,
  lag = NULL) {
  add_comparison(m,
                 compare_distribution_new(
                   role = role,
                   initialized = initialized,
                   method = args$method,
                   fixed = args$fixed,
                   lag = args$lag,
                   ...))
}

compare_distribution_new <- function(
  role,
  initialized,
  method,
  fixed,
  lag,
  ...) {
  comparison(
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

execute.compare_distribution <- function(...) {
  m$comparisons[[id]] <- results
}


generate_jsd <- function(x) {
  a <- x

  b <- a[,{d_sum = sum(n); .SD[, .(n = n,  prop = n / d_sum), by = var]}, by = grp]
  rm(a); gc()

  form <- as.formula(glue::glue("{var} ~ {grp}"))

  c <- dcast(data = b,
             formula = form,
             value.var = c(#"prop",
               "prop"),
             fun.aggregate =  mean,
             na.rm = T,
             fill = 0)
  rm(b); gc()


  c[, prior_fix := .SD, .SDcols = 2]

  d <- melt(data = c, id.vars = c("prior_fix",var), variable.name = grp)
  rm(c); gc()
  e <- copy(d)

  e[ ,prior_lag := shift(.(value),1L, fill = 0), by = var]

  jsd <- e[,list(JSD(as.matrix(rbind(prior_lag, value))),
                 JSD(as.matrix(rbind(prior_fix, value)))),
           by = grp]

  rm(e); gc()

  setnames(jsd, c(grp, "JSD_LAG", "JSD_FIX") )
  jsd[1, JSD_LAG := 0]
  jsd[ , JSD_LAG_DIFF := shift(x=JSD_LAG, n=1L, fill = 0, type = "lag")][ , JSD_LAG_DIFF := JSD_LAG - JSD_LAG_DIFF]
  jsd[ , JSD_FIX_DIFF := shift(x=JSD_FIX, n=1L, fill = 0, type = "lag")][ , JSD_FIX_DIFF := JSD_FIX - JSD_FIX_DIFF]
  jsd
}

