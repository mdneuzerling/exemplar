#' @export
assertions.data.frame <- function(x, data_name = "data", ...) {
  if (missing(...)) {
    selected_columns <- colnames(x)
  } else {
    selected <- tidyselect::eval_select(rlang::expr(c(...)), x)
    selected_columns <- names(selected)
  }

  to_assert <- glue::glue("is.data.frame({data_name})")

  # This assertion checks that all columns are present, in the correct order,
  # and that there are no other columns present. If we're selecting a subset of
  # columns from a larger data frame then this might not be true, so we include
  # the assertion but comment it out.
  column_assertion <- glue::glue(
    "colnames({data_name}) == {input_character_vector(selected_columns)}"
  )
  if (!identical(selected_columns, names(x))) {
    column_assertion <- c(
      "# The data is potentially being subsetted so this assertion has been disabled:",
      comment_out(column_assertion)
    )
  }

  to_assert <- c(to_assert, column_assertion)

  for (column in selected_columns) {
    to_assert <- c(
      to_assert,
      "", # new line
      glue::glue("\"{column}\" %in% colnames({data_name})"),
      assertions(x[[column]], data_name = glue::glue("data${column}"))
    )
  }

  to_assert
}
