test_that("Validation test: mtcars", {
  withr::with_output_sink(
    nullfile(),
    function_text <- exemplar(mtcars)
  )
  eval(parse(text = function_text))
  expect_true(validate_mtcars(mtcars))
})

test_that("Validation test: diamonds", {
  diamonds <- ggplot2::diamonds
  withr::with_output_sink(
    nullfile(),
    function_text <- exemplar(diamonds)
  )
  eval(parse(text = function_text))
  expect_true(validate_diamonds(diamonds))
})

test_that("Validation test: starwars", {
  starwars <- dplyr::starwars
  withr::with_output_sink(
    nullfile(),
    function_text <- exemplar(starwars)
  )
  eval(parse(text = function_text))
  expect_true(validate_starwars(starwars))
})

test_that("Validation test: iris", {
  withr::with_output_sink(
    nullfile(),
    function_text <- exemplar(iris,
                              .enable_deviance_assertions = TRUE,
                              .allowed_deviance =  2)
  )
  eval(parse(text = function_text))
  expect_true(validate_iris(iris))
})
