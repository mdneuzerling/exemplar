input_character_vector <- function(x) {
  stopifnot(is.character(x))
  paste0(
    "c(",
    paste(
      paste0('"', x, '"'),
      collapse = ", "),
    ")"
  )
}

input_POSIXct <- function(x) {
  stopifnot(is.POSIXct(x))
  tz <- attr(x, "tzone")
  if (tz == "") tz <- "UTC"
  as_utc_character <- format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
  glue::glue("as.POSIXct(\"{x}\", tz = \"{tz}\")")
}

comment_out <- function(comment) paste("#", comment)

maybe_comment <- function(x, comment = FALSE) if (comment) comment_out(x) else x
