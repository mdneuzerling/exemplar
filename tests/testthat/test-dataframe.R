test_that("Validation test: mtcars", {
  expect_data_self_validates(mtcars)
})

test_that("Validation test: diamonds", {
  expect_data_self_validates(ggplot2::diamonds)
})

test_that("Validation test: starwars", {
  expect_data_self_validates(dplyr::starwars)
})

test_that("Validation test: iris", {
  expect_data_self_validates(iris)
})

test_that("empty data fails unless explicitly allowed", {
  default_mtcars_exemplar <- withr::with_output_sink(
    nullfile(),
    eval(parse(text = exemplar(mtcars)))
  )

  mtcars_empty <- mtcars[FALSE,]

  expect_error(
    default_mtcars_exemplar(mtcars_empty),
    "nrow\\(data\\) > 0 is not TRUE"
  )

  allow_empty_mtcars_exemplar <- withr::with_output_sink(
    nullfile(),
    eval(parse(text = exemplar(mtcars, .check_non_empty = FALSE)))
  )

  # we can still expect warnings because min, max, etc. acting on empty vectors
  expect_error(
    suppressWarnings(allow_empty_mtcars_exemplar(mtcars_empty)),
    NA
  )
})
