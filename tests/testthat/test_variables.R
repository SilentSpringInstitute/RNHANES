test_that("Downloading variable list and data file list works", {
  skip_on_cran()
  destination <- tempdir()

  test_that("nhanes_data_files works", {
    test_that("downloading all components works", {
      files <- nhanes_data_files(destination = destination)

      expect_true(file.exists(file.path(destination, "nhanes_data_files.csv")))
      expect_more_than(nrow(files), 0)
      expect_equivalent(unique(files$component), c("demographics", "dietary", "examination", "laboratory", "questionnaire"))

      # Clean up
      unlink(file.path(destination, "nhanes_data_files.csv"))
    })

    test_that("downloading one component works", {
      files <- nhanes_data_files(component = "laboratory", destination = destination)

      expect_true(file.exists(file.path(destination, "nhanes_data_files.csv")))
      expect_more_than(nrow(files), 0)
      expect_equivalent(unique(files$component), c("laboratory"))
    })
  })

  test_that("nhanes_variables works", {
    test_that("downloading the variable list works", {
      variables <- nhanes_variables(destination = destination)

      expect_true(file.exists(file.path(destination, "nhanes_variables.csv")))
      expect_more_than(nrow(variables), 0)

      # Clean up
      unlink(file.path(destination, "nhanes_variables.csv"))
    })
  })
})
