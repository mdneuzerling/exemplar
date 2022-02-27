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
  stopifnot(inherits(x, "POSIXct"))
  tz <- attr(x, "tzone")
  if (tz == "") tz <- "UTC"
  as_utc_character <- format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
  glue::glue("as.POSIXct(\"{x}\", tz = \"{tz}\")")
}

comment_out <- function(comment) paste("#", comment)

maybe_comment <- function(code, comment = FALSE) {
  if (comment) comment_out(code) else code
}

preface <- function(code, remark) {
  # We use a width of 76 to allow for the comment text ("# ") and a possible
  # indent, each of which is 2 characters
  wrapped_remark <- unlist(
    strsplit(stringr::str_wrap(remark, width = 78), "\n")
  )
  commented_wrapped_remark <- paste("#", wrapped_remark)
  c(commented_wrapped_remark, code)
}

comment_with_remark <- function(code, remark) {
  preface(comment_out(code), remark)
}

maybe_comment_with_remark <- function(code, condition, remark) {
  if (!condition) code else comment_with_remark(code, remark)
}
