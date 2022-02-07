generate_polarity_assertion <- function(data_name, min_value, max_value) {
  if (min_value > 0) {
    glue::glue("min({data_name}, na.rm = TRUE) > 0 # all positive")
  } else if (min_value == 0) {
    glue::glue("min({data_name}, na.rm = TRUE) >= 0 # all non-negative")
  } else if (max_value < 0) {
    glue::glue("max({data_name}, na.rm = TRUE) < 0 # all negative")
  } else if (max_value == 0) {
    glue::glue("max({data_name}, na.rm = TRUE) <= 0 # all non-positive")
  } else {
    NULL # will be ignored in c()
  }
}

generate_none_missing_assertion <- function(x, data_name) {
  missing_found <- any(is.na(x) | is.null(x))
  to_assert <- maybe_comment(
    glue::glue("!any(is.na({data_name}) | is.null({data_name}))"),
    comment = missing_found
  )
  if (missing_found) {
    to_assert <- c(
      "# Missing values were detected so this assertion has been disabled:",
      to_assert
    )
  }
  to_assert
}

generate_uniqueness_assertion <- function(x, data_name) {
  duplicates_found <- any(duplicated(x))
  to_assert <- maybe_comment(
    glue::glue("!any(duplicated({data_name}))"),
    comment = duplicates_found
  )
  if (duplicates_found) {
    to_assert <- c(
      "# Duplicate values were detected so this assertion has been disabled:",
      to_assert
    )
  }
  to_assert
}

generate_range_assertions <- function(data_name, min_value, max_value) {
  c(
    "# Uncomment or modify the below range assertions if needed:",
    comment_out(glue::glue("max({data_name}, na.rm = TRUE) <= {max_value}")),
    comment_out(glue::glue("{min_value} <= min({data_name}, na.rm = TRUE)"))
  )
}

generate_sd_assertions <- function(x, data_name) {
  stopifnot(is.numeric(x))
  avg <- round(mean(x, na.rm = TRUE), 2)
  std_dev <- round(stats::sd(x, na.rm = TRUE), 2)
  c(
    "# Uncomment or modify the below deviance from mean assertions if needed.",
    glue::glue("# The mean is {avg} and the standard deviation is {std_dev}:"),
    comment_out(glue::glue("max({data_name}, na.rm = TRUE) <= {avg} + 4 * {std_dev}")),
    comment_out(glue::glue("{avg} - 4 * {std_dev} <= min({data_name}, na.rm = TRUE)"))
  )
}
