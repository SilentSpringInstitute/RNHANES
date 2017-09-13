test_that("Computing geometric means works", {
  skip_on_cran()
  destination <- getOption("RNHANES_destination", tempdir())

  nhanes_data <- nhanes_load_data("PFC", "2011-2012", demographics = TRUE, cache = TRUE, destination = destination)

  test_that("Passes spot check", {
    geometric_mean <- nhanes_geometric_mean(nhanes_data, column = "LBXPFNA", weights_column = "WTSA2YR")
    expect_equal(geometric_mean$value, 0.881, tolerance = 0.001)
    expect_equal(geometric_mean$value_lower, 0.8064, tolerance = 0.001)
    expect_equal(geometric_mean$value_upper, 0.9615, tolerance = 0.001)

    geometric_mean <- nhanes_geometric_mean(nhanes_data, column = "LBXPFOS", weights_column = "WTSA2YR")
    expect_equal(geometric_mean$value, 6.305715, tolerance = 0.001)
    expect_equal(geometric_mean$value_lower, 5.8667, tolerance = 0.001)
    expect_equal(geometric_mean$value_upper, 6.777, tolerance = 0.001)

    geometric_mean <- nhanes_geometric_mean(nhanes_data, column = "LBXPFOA", weights_column = "WTSA2YR")
    expect_equal(geometric_mean$value, 2.078, tolerance = 0.001)
    expect_equal(geometric_mean$value_lower, 1.9547, tolerance = 0.001)
    expect_equal(geometric_mean$value_upper, 2.2104, tolerance = 0.001)
  })

  test_that("Geometric means can be computed for multiple chemicals at a time", {
    geometric_means <- nhanes_geometric_mean(nhanes_data,
                                 column = c("LBXPFOA", "LBXPFOS"),
                                 weights_column = c("WTSA2YR", "WTSA2YR"))

    expect_equal(geometric_means$value, c(2.078, 6.304), tolerance = 0.001)
  })

  test_that("Geometric means can be computed for multiple chemicals at a time, without specifying weights", {
    geometric_means <- nhanes_geometric_mean(nhanes_data,
                                 column = c("LBXPFOA", "LBXPFOS"))


    expect_equal(geometric_means$value, c(2.078, 6.304), tolerance = 0.001)
  })

  #nhanes_data <- nhanes_load_data(c())

  test_that("Geometric means can be computed for chemicals in multiple files", {
    nhanes_data <- nhanes_load_data(c("PFC_F", "PFC_G"), c("2009-2010", "2011-2012"), demographics = TRUE, cache = TRUE, destination = destination)

    variables <- data.frame(
      file_name = c("PFC_F", "PFC_F", "PFC_G", "PFC_G"),
      cycle = c("2009-2010", "2009-2010", "2011-2012", "2011-2012"),
      column = c("LBXPFOA", "LBXPFOS", "LBXPFOA", "LBXPFOS"),
      stringsAsFactors = FALSE
    )

    geometric_means <- nhanes_geometric_mean(nhanes_data, column = variables)

    expect_equal(geometric_means$cycle, c("2009-2010", "2009-2010", "2011-2012", "2011-2012"))
    expect_equal(geometric_means$column, c("LBXPFOA", "LBXPFOS", "LBXPFOA", "LBXPFOS"))
    expect_equal(geometric_means$value, c(3.055, 9.22, 2.078, 6.305), tolerance = 0.01)
  })
})
