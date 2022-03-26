#' @export
assertions.double <- function(
  x,
  data_name = "data",
  ...,
  .enable_range_assertions = FALSE,
  .enable_deviance_assertions = FALSE,
  .allowed_deviance = 4
) {
  min_value <- min(x, na.rm = TRUE)
  max_value <- max(x, na.rm = TRUE)
  c(
    glue::glue("is.double({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    generate_polarity_assertion(data_name, min_value, max_value),
    generate_range_assertions(
      data_name,
      min_value,
      max_value,
      enable = .enable_range_assertions
    ),
    generate_sd_assertions(
      x,
      data_name,
      enable = .enable_deviance_assertions,
      allowed_deviance = .allowed_deviance)
  )
}

#' @export
assertions.integer <- function(x, data_name = "data", ...) {
  min_value <- min(x, na.rm = TRUE)
  max_value <- max(x, na.rm = TRUE)
  c(
    glue::glue("is.integer({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    generate_polarity_assertion(data_name, min_value, max_value),
    generate_range_assertions(data_name, min_value, max_value),
    generate_sd_assertions(x, data_name)
  )
}

#' @export
assertions.logical <- function(x, data_name = "data", ...) {
  c(
    glue::glue("is.logical({data_name})"),
    generate_none_missing_assertion(x, data_name)
  )
}

#' @export
assertions.character <- function(x, data_name = "data", ...) {

  to_assert <- glue::glue("is.character({data_name})")

  if (!all(validUTF8(x))) {
    return(
      c(
        to_assert,
        comment_out(paste(
          "This character vector is not valid UTF-8, so further assertions have",
          "been skipped"
        ))
      )
    )
  }

  min_length <- min(sapply(x, nchar), na.rm = TRUE)
  max_length <- max(sapply(x, nchar), na.rm = TRUE)

  nchar_range_assertions <- c(
    "# Uncomment or modify the below range assertions if needed:",
    comment_out(glue::glue(
      "max(sapply({data_name}, nchar), na.rm = TRUE) <= {max_length}"
    )),
    comment_out(glue::glue(
      "{min_length} <= min(sapply({data_name}, nchar), na.rm = TRUE)"
    ))
  )

  c(
    to_assert,
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    nchar_range_assertions
  )
}

#' @export
assertions.POSIXct <- function(x, data_name = "data", ...) {
  min_value <- input_POSIXct(min(x, na.rm = TRUE))
  c(
    glue::glue("is.POSIXct({data_name})"),
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name),
    "# Uncomment or modify the below range assertion if needed:",
    comment_out(glue::glue("{min_value} <= min({data_name})"))
  )
}


#' @export
assertions.ordered <- function(x, data_name = "data", ...) {
  c(
    glue::glue("is.ordered({data_name})"),
    assertions.factor(x, data_name, ...)
  )
}

#' @export
assertions.factor <- function(x, data_name = "data", ...) {
  level_assertion <- if (is.ordered(x)) {
    expected_levels <- input_character_vector(levels(x))
    glue::glue("identical(levels({data_name}), {expected_levels})")
  } else {
    # in this case order doesn't matter so we sort before comparing
    expected_levels <- input_character_vector(sort(levels(x)))
    glue::glue("identical(sort(levels({data_name})), {expected_levels})")
  }

  c(
    glue::glue("is.factor({data_name})"),
    level_assertion,
    generate_none_missing_assertion(x, data_name),
    generate_uniqueness_assertion(x, data_name)
  )
}
