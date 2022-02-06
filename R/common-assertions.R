generate_polarity_assertion <- function(data_name, min_value, max_value) {
  if (min_value > 0) {
    glue::glue("min({data_name}) > 0 # all positive")
  } else if (min_value == 0) {
    glue::glue("min({data_name}) >= 0 # all non-negative")
  } else if (max_value < 0) {
    glue::glue("max({data_name}) < 0 # all negative")
  } else if (max_value == 0) {
    glue::glue("max({data_name}) <= 0 # all non-positive")
  } else {
    NULL # will be ignored in c()
  }
}

generate_none_missing_assertion <- function(x, data_name) {
  missing_found <- any(is.na(x))
  to_assert <- maybe_comment(
    glue::glue("all(!is.na({data_name}))"),
    comment = missing_found
  )
  if (missing_found) {
    to_assert <- c(
      "# The below data may contain missing values so this assertion has been commented out:",
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
      "# The below data may contain duplicates so this assertion has been commented out:",
      to_assert
    )
  }
  to_assert
}

generate_range_assertions <- function(data_name, min_value, max_value) {
  c(
    "# Uncomment or modify the below range assertions if needed:",
    comment_out(glue::glue("max({data_name}) <= {max_value}")),
    comment_out(glue::glue("{min_value} <= min({data_name})"))
  )
}

