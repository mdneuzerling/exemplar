expect_data_self_validates <- function(data, ...) {

  act <- quasi_label(rlang::enquo(data), arg = "data")

  act$validation_function <- withr::with_output_sink(
    nullfile(),
    eval(parse(text = exemplar(act$val, ..., .function_suffix = "data")))
  )

  act$error_message <- tryCatch(
    {
      act$validation_function(act$val)
      NA_character_
    },
    error = function(e) e$message
  )

  act$validates  <- is.na(act$error_message)

  expect(
    act$validates,
    sprintf(
      "%s does not satisfy its own validation function: %s",
      act$lab,
      act$error_message
    )
  )

  invisible(act$validates)
}

skip_if_data_unavailable <- function(data) {
  if (!is.null(data)) {
    return(invisible(TRUE))
  }

  data_set_name <- deparse(substitute(data))
  skip_message <- paste(data_set_name, "data is not available")
  skip(skip_message)
}
