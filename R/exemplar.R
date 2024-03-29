#' Create a validation function from an exemplar
#'
#' @description
#' This function will print a validation function that can be used to make sure
#' that a new object looks like the input (its _exemplar_).
#'
#' Some checks are commented out. This is because the exemplar does not meet the
#' criteria (eg. no duplicate values) or the checks are too specific to be used
#' by default (range checks). The intention is that users will modify the
#' validation functions to meet their needs before placing them in pipelines and
#' scripts.
#'
#' The functions produced by `exemplar` require at least R 3.5 (due to
#' improvements made to \verb{\link{stopifnot}} but otherwise requires no
#' dependencies. That is, `exemplar` generates functions that do not need
#' `exemplar` or any other packages to run.
#'
#' @param x The object to use as the exemplar of the validation function.
#' @param ... Additional arguments used when building the assertions. Currently
#'   this is only used to apply validation to only certain columns in a data
#'   frame. This uses `tidyselect` functions. Refer to `dplyr` package for
#'   more information.
#' @param .function_suffix By default the generated function will be named after
#'   the input, eg. `exemplar(mtcars)` will generate a function named
#'   `validate_mtcars`. This parameter allows overriding the suffix, eg.
#'   `exemplar(mtcars, .function_suffix = "my_data")` will generate a function
#'   named `validate_my_data`.
#' @param .enable_range_assertions Assertions for numeric columns/vectors will
#'   include range assertions, to ensure that any new data is within the range
#'   of the exemplar. These assertions will be commented out by default, unless
#'   the argument to this parameter is `TRUE`.
#' @param .enable_deviance_assertions Assertions for numeric columns/vectors
#'   will include deviance assertions, to ensure that any new data is within a
#'   number of standard deviations of the mean of the exemplar, as configured by
#'   `.allowed_deviance`. These assertions will be commented out by default,
#'   unless the argument to this parameter is `TRUE`.
#' @param .allowed_deviance Configures the number of standard deviations from
#'   the mean that new data is allowed to be. The deviance assertions are
#'   commented out by default unless `.enable_deviance_assertions` is set to
#'   `TRUE`. The `.allowed_deviance` defaults to 4, such that new data is
#'   within 4 standard deviations of the mean based on the statistical
#'   properties of the exemplar.
#'
#' @examples
#' exemplar(mtcars)
#' exemplar(mtcars$gear)
#' exemplar(mtcars, -cyl)
#' exemplar(mtcars, starts_with("d"))
#' exemplar(mtcars, .function_suffix = "my_data")
#'
#' @export
exemplar <- function(
  x,
  ...,
  .function_suffix = NULL,
  .enable_range_assertions = FALSE,
  .enable_deviance_assertions = FALSE,
  .allowed_deviance = 4
) {
  if (is.null(.function_suffix)) {
    # Guess a suitable function name from x, and replace all punctuation with "_"
    .function_suffix <- gsub("[[:punct:]]", "_", deparse(substitute(x)))
    .function_suffix <- gsub(" ", "", .function_suffix)
    # If the input comes from a pipe, it will be just a . which is converted
    # to an underscore. In this case, fall back to a default "data"
    if (.function_suffix == "_") .function_suffix <- "data"
  }

  styled <- styler::style_text(
    c(
      template_header(.function_suffix),
      assertions(
        x,
        data_name = "data",
        ...,
        .enable_range_assertions = .enable_range_assertions,
        .enable_deviance_assertions = .enable_deviance_assertions,
        .allowed_deviance = .allowed_deviance
      ),
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
#' @keywords internal
assertions.default <- function(x, data_name = "data", ...) {
  correct_class <- input_character_vector(class(x))
  glue::glue("class({data_name}) == {correct_class}")
}

template_header <- function(function_suffix) {
  c(
    paste0("validate_", function_suffix, " <- function(data) {"),
    "stopifnot(exprs = {"
  )
}

# in order:
# 1. close the expression block within stopifnot }
# 2. close the arguments to stopifnot )
# 3. return TRUE (or we would have errored)
# 4. close the overall validate function }
template_footer <- function() c("}", ")", "invisible(TRUE)", "}")
