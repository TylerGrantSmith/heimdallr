generate_jsd_fix <- function(.x, .grp, .var,
                             reference = NULL,
                             fun.aggregate = mean,
                             na.rm = T,
                             fill = 0) {
  .x[,reference:=.SD[,reference, with = F]]
  melt(data = .x, id.vars = c("reference",.var), variable.name = .grp)
}

generate_jsd_shift <- function(.x, .grp, .var,
                               type = "lag", n = 1,
                               fun.aggregate = mean,
                               na.rm = T,
                               fill = 0) {
  .x <- melt(data = .x, id.vars = .var, variable.name = .grp)
  .x[ ,reference := shift(.(value), n = n, type = type, fill = fill), by = .var]
  .x
}

generate_jsd <- function(.density, .var, .grp,
                         method = "fix",
                         ...,
                         fun.aggregate = mean,
                         na.rm = T,
                         fill = 0) {

  c <- dcast(data = .density,
             formula = as.formula(glue::glue("{.var} ~ {.grp}")),
             value.var = c("PROP"),
             fun.aggregate = mean,
             na.rm = T,
             fill = 0)

  jsd <- do.call(
    glue::glue("generate_jsd_{method}"),
    args =
      list(
        .x = c, .grp = .grp, .var = .var,
        fun.aggregate = fun.aggregate,
        na.rm = na.rm,
        fill = fill,
        ...))

  jsd <-
    suppressMessages(
      jsd[,.(JSD = philentropy::JSD(as.matrix(rbind(reference, value)))),
        by = .grp])

  if(method == "shift")
    jsd[1,JSD := 0]

  jsd[ , JSD_DIFF := shift(x = JSD, n=1L, fill = 0, type = "lag")][ , JSD_DIFF := JSD - JSD_DIFF]
  jsd
}
