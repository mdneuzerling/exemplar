test_that("Validation test: mtcars", {
  expect_data_self_validates(mtcars)
})

test_that("Validation test: diamonds", {
  expect_data_self_validates(ggplot2::diamonds)
})

test_that("Validation test: starwars", {
  expect_data_self_validates(dplyr::starwars)
})
