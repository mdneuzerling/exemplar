
#' @export
assertions.data.frame <- function(
  x,
  data_name = "data") {

  selected <- x
  # selected <- tidyselect::eval_select(substitute(columns), x)
  selected_columns <- names(selected)

  to_assert <- c("is.data.frame(data)")

  # This assertion checks that all columns are present, in the correct order,
  # and that there are no other columns present. If we're selecting a subset of
  # columns from a larger data frame then this might not be true, so we include
  # the assertion but comment it out.
  all_columns_selected <- identical(selected_columns, names(x))
  to_assert <- append(
    to_assert,
    maybe_comment(
      glue::glue(
        "colnames({data_name}) == {input_character_vector(selected_columns)}"
      ),
      comment = !all_columns_selected
    )
  )

  to_assert
}
