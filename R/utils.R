rand_id <- function(x) {
  glue::glue("x_{glue::glue_collapse(sample(letters, 8, T))}")
}
