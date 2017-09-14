test_that("Retreiving survey designs works", {
  skip_on_cran()
  destination <- getOption("RNHANES_destination", tempdir())

  nhanes_data <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE, cache = TRUE, destination = destination)

  test_that("Passes spot check", {
    design <- nhanes_survey_design(nhanes_data, weights_column = "WTSA2YR")
    expect_equal(typeof(design), "list")

    expected_length <- sum(!is.na(nhanes_data$WTSA2YR))
    expect_equal(nrow(design$variables), expected_length)
  })
})
