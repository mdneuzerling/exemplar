test_that("validations are skipped for non-UTF-8 characters", {
  not_utf_8 <- c("valid", "not valid \xa0", "valid")
  validation_function_text <- exemplar(not_utf_8)
  expect_true(
    grepl(
      "# This character vector is not valid UTF-8",
      validation_function_text
    )
  )
})

test_that("deviance assertions are skipped for singleton vectors", {
  expect_true(
    grepl(
      "# Deviance assertions are skipped",
      exemplar(c(5.3))
    )
  )

  # also true when deviance assertions are enabled
  expect_true(
    grepl(
      "# Deviance assertions are skipped",
      exemplar(c(5.3), .enable_deviance_assertions = TRUE)
    )
  )
})
