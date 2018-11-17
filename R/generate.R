generate_frequency <- function(x, grp, var) {
  x[,.N, by =c(grp,var)]
}

fast_density <- function(x, grp, var) {
  x <- generate_frequency(x, grp, var)
  setDT(x)[, {N_SUM = sum(N); .SD[, .(N, PROP = N / N_SUM), by = c(grp)]}, by = c(var)]
}

generate_jsd_fix <- function(.x, .grp_n, .var_n,
                             reference = NULL,
                             fun.aggregate = mean,
                             na.rm = T,
                             fill = 0) {
  .x[,reference:=.SD[,reference, with = F]]
  melt(data = .x, id.vars = c("reference",.var_n), variable.name = .grp_n)
}

generate_jsd_shift <- function(.x, .grp_n, .var_n,
                               type = "lag", n = 1,
                               fun.aggregate = mean,
                               na.rm = T,
                               fill = 0) {
  .x <-melt(data = .x, id.vars = .var_n, variable.name = .grp_n)
  .x[ ,reference := shift(.(value), n = n, type = type, fill = fill), by = .var_n]

}

generate_jsd <- function(.x, .grp, .var,
                         method = "fix",
                         ...,
                         fun.aggregate = mean,
                         na.rm = T,
                         fill = 0) {


  grp_n <- quo_name(.grp)
  var_n <- quo_name(.var)

  densities <- fast_density(x, grp_n, var_n)

  c <- dcast(data = densities,
             formula = as.formula(glue::glue("{var_n} ~ {grp_n}")),
             value.var = c("PROP"),
             fun.aggregate = mean,
             na.rm = T,
             fill = 0)

  c <- do.call(
    glue::glue("generate_jsd_{method}"),
    args =
      list(
        .x = c, .grp_n = grp_n, .var_n = var_n,
        fun.aggregate = fun.aggregate,
        na.rm = na.rm,
        fill = fill,
        ...))

  jsd <-
    suppressMessages(
      c[,.(FIX = philentropy::JSD(as.matrix(rbind(reference, value)))),
        by = c(quo_name(grp))])

  rm(c); gc()

  jsd[ , FIX_DIFF := shift(x = FIX, n=1L, fill = 0, type = "lag")][ , FIX_DIFF := FIX - FIX_DIFF]
  jsd
}
