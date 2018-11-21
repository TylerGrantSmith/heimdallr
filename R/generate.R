generate_jsd_fix <- function(.x, .grp, .var,
                             reference = NULL,
                             fun.aggregate = mean,
                             na.rm = T,
                             fill = 0) {
  if(is.null(reference)) {
    reference <- colnames(.x)[[2]]
    warning(glue::glue("`reference` missing.  Using {reference} as comparison"),call. = F)
  }
  .x[,reference:=.SD[,reference, with = F]]
  data.table::melt(data = .x, id.vars = c("reference", .var), variable.name = .grp)
}

generate_jsd_shift <- function(.x, .grp, .var,
                               type = "lag", n = 1,
                               fun.aggregate = mean,
                               na.rm = T,
                               fill = 0) {
  .x <- data.table::melt(data = .x, id.vars = .var, variable.name = .grp)
  .x[ ,reference := data.table::shift(.(value), n = n, type = type, fill = fill), by = .var]
  .x
}

generate_jsd <- function(.density, .var, .grp,
                         distance_method,
                         method,
                         options,
                         fun.aggregate = mean,
                         na.rm = T,
                         fill = 0) {

  wide <- data.table::dcast(
    data = .density,
    formula = as.formula(glue::glue("{.var} ~ {.grp}")),
    value.var = c("PROP"),
    fun.aggregate = mean,
    na.rm = T,
    fill = 0)

  jsd <- do.call(
    glue::glue("generate_jsd_{method}"),
    args =
      c(list(
        .x = wide, .grp = .grp, .var = .var,
        fun.aggregate = fun.aggregate,
        na.rm = na.rm,
        fill = fill),
        options))

  jsd <-
    suppressMessages(
      jsd[,.(DISTANCE = philentropy::distance(method = distance_method, as.matrix(rbind(reference, value)), test.na = F)),
          by = .grp])

  if(method == "shift")
    jsd[1,DISTANCE := 0]

  jsd[ , DIFF := data.table::shift(x = DISTANCE, n=1L, fill = 0, type = "lag")][ , DIFF := DISTANCE - DIFF]
  jsd
}
