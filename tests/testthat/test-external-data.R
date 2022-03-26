test_that("Validation test: legal_aid (external data)", {
  legal_aid_url <- paste0(
    "https://assets.publishing.service.gov.uk",
    "/government/uploads/system/uploads/attachment_data/file/1041147",
    "/legal-aid-statistics-main-data-jul-sep-2021.csv"
  )

  legal_aid <- tryCatch(
    readr::read_csv(legal_aid_url),
    error = function(e) NULL
  )

  skip_if_data_unavailable(legal_aid)

  expect_data_self_validates(legal_aid)
})

