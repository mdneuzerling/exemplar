#' @export
assertions.double <- function(x, data_name = "data") {
  min_value <- min(x, na.rm = TRUE)
  max_value <- max(x, na.rm = TRUE)
  c(
    glue::glue("is.double({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    generate_polarity_assertion(data_name, min_value, max_value),
    generate_range_assertions(data_name, min_value, max_value)
  )
}

#' @export
assertions.integer <- function(x, data_name = "data") {
  min_value <- min(x, na.rm = TRUE)
  max_value <- max(x, na.rm = TRUE)
  c(
    glue::glue("is.integer({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    generate_polarity_assertion(data_name, min_value, max_value),
    generate_range_assertions(data_name, min_value, max_value)
  )
}

#' @export
assertions.logical <- function(x, data_name = "data") {
  c(
    glue::glue("is.logical({data_name})"),
    generate_none_missing_assertion(x, data_name)
  )
}

#' @export
assertions.character <- function(x, data_name = "data") {
  c(
    glue::glue("is.character({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name)
  )
}

#' @export
assertions.POSIXct <- function(x, data_name = "data") {
  min_value <- input_POSIXct(min(x, na.rm = TRUE))
  c(
    glue::glue("is.POSIXct({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    "# Uncomment or modify the below range assertion if needed:",
    comment_out(glue::glue("{min_value} <= min({data_name})"))
  )
}

