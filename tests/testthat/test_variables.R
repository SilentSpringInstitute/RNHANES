test_that("Downloading variable list and data file list works", {
  skip_on_cran()
  destination <- tempdir()

  test_that("nhanes_cycle_years returns correct cycles", {
    expect_equivalent(nhanes_cycle_years(), c("1999-2000",
                                       "2001-2002",
                                       "2003-2004",
                                       "2005-2006",
                                       "2007-2008",
                                       "2009-2010",
                                       "2011-2012",
                                       "2013-2014",
                                       "2015-2016"))
  })

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

      # Clean up
      unlink(file.path(destination, "nhanes_data_files.csv"))
    })

    test_that("throws error if given nonexistent component", {
      expect_error(nhanes_data_files(component = "nonexisting", destination = destination), "Invalid component")
    })

    test_that("throws error if given nonexistent destination", {
      expect_error(nhanes_data_files(destination = "nodir/test.file"), "Directory doesn't exist")
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

    test_that("it throws a warning if you provide a directory that doesn't exist", {
      expect_error(nhanes_variables(destination = "doesntexist/variables.csv"), "Directory doesn't exist")
    })
  })
})
