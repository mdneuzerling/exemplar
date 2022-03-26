test_that("validations is skipped for non-UTF-8 characters", {
  not_utf_8 <- c("valid", "not valid \xa0", "valid")
  validation_function_text <- exemplar(not_utf_8)
  expect_true(
    grepl(
      "# This character vector is not valid UTF-8",
      validation_function_text
    )
  )
})
