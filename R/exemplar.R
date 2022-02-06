#' Create a validation function from an exemplar
#'
#' @param x The object for which to create an exemplar
#' @param ... Additional arguments used when building the assertions. Currently
#'   unused.
#'
#' @export
exemplar <- function(x, ...) {
  # Deparse the input to x, and replace all punctuation with "_"
  validation_suffix <- gsub("[[:punct:]]", "_", deparse(substitute(x)))
  # If the input comes from a pipe, it will be just a . which is converted to _
  # In this case, fall back to a default "data"
  if (validation_suffix == "_") validation_suffix <- "data"

  styled <- styler::style_text(
    c(
      template_header(validation_suffix),
      assertions(x, ...),
      template_footer()
    )
  )

  print(styled)
  invisible(paste(styled, collapse = "\n"))
}

#' Generate character vector of assertions for a given object
#'
#' @param x The object for which to generate assertions.
#' @param data_name Name of data object to validate, as it would be called in
#'   the interior of the function. This defaults to "data", but it may make
#'   sense to use other names for individual assertions, eg. "data$column".
#' @param ... Arguments passed to class-specific functions.
#'
#' @export
assertions <- function(x, data_name, ...) UseMethod("assertions")

#' @export
assertions.default <- function(x, data_name = "data", ...) {
  correct_class <- input_character_vector(class(x))
  glue::glue("class({data_name}) == {correct_class}")
}
