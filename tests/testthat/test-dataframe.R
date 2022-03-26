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
  withr::with_output_sink(
    nullfile(),
    function_text <- exemplar(iris,
                              .enable_deviance_assertions = TRUE,
                              .allowed_deviance =  2)
  )
  eval(parse(text = function_text))
  expect_true(validate_iris(iris))
})
