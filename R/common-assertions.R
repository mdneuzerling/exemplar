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
  maybe_comment_with_remark(
    glue::glue("!any(is.na({data_name}) | is.null({data_name}))"),
    condition = missing_found,
    remark = "Missing values were detected so this assertion has been disabled:"
  )
}

generate_uniqueness_assertion <- function(x, data_name) {
  duplicates_found <- any(duplicated(x))
  maybe_comment_with_remark(
    glue::glue("!any(duplicated({data_name}))"),
    condition = duplicates_found,
    remark = "Duplicate values were detected so this assertion has been disabled:"
  )
}

generate_range_assertions <- function(
  data_name,
  min_value,
  max_value,
  enable = TRUE
) {
  to_assert <- c(
    maybe_comment(
      glue::glue("max({data_name}, na.rm = TRUE) <= {max_value}"),
      !enable
    ),
    maybe_comment(
      glue::glue("{min_value} <= min({data_name}, na.rm = TRUE)"),
      !enable
    )
  )
  preface(
    to_assert,
    "(Un)comment or modify the below range assertions if needed:"
  )
}

generate_sd_assertions <- function(
  x,
  data_name,
  allowed_deviance = 4,
  enable = TRUE
) {
  stopifnot(is.numeric(x))
  avg <- round(mean(x, na.rm = TRUE), 2)
  std_dev <- round(stats::sd(x, na.rm = TRUE), 2)
  to_assert <- c(
    maybe_comment(
      glue::glue(
        "max({data_name}, na.rm = TRUE) <= {avg} + {allowed_deviance} * {std_dev}"
      ),
      !enable
    ),
    maybe_comment(
      glue::glue(
        "{avg} - {allowed_deviance} * {std_dev} <= max({data_name}, na.rm = TRUE)"
      ),
      !enable
    )
  )
  preface(
    to_assert,
    c(
      "(Un)comment or modify the below deviance assertions if needed.",
      glue::glue("The mean is {avg} and the standard deviation is {std_dev}:")
    )
  )
}
