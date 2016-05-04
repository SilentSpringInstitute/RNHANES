test_that("Computing quantiles works", {
  skip_on_cran()
  destination <- getOption("RNHANES_destination", tempdir())

  nhanes_data <- nhanes_load_data("PFC", "2011-2012", demographics = TRUE, cache = TRUE, destination = destination)

  test_that("Passes spot check", {
    quantiles <- nhanes_quantile(nhanes_data, column = "LBXPFUA", comment_column = "LBDPFUAL", weights_column = "WTSA2YR", quantiles = c(0, 0.5, 0.75, 0.90, 0.95))

    expect_equal(quantiles$below_lod, c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equivalent(quantiles$value, c(0.07, 0.12, 0.22, 0.38, 0.62))

    quantiles <- nhanes_quantile(nhanes_data, "LBXPFOS", "LBDPFOSL", weights_column = "WTSA2YR", quantiles = c(0.5, 0.75, 0.9, 0.95))
    expect_equal(quantiles$below_lod, c(FALSE, FALSE, FALSE, FALSE))
    expect_equivalent(quantiles$value, c(6.53, 10.5, 15.7, 21.7))

    quantiles <- nhanes_quantile(nhanes_data, "LBXPFHP", "LBDPFHPL", weights_column = "WTSA2YR", quantiles = c(0.5, 0.75, 0.9, 0.95))
    expect_equal(quantiles$below_lod, c(TRUE, TRUE, FALSE, FALSE))
    expect_equivalent(quantiles$value, c(0.07, 0.07, 0.14, 0.22))
  })

  test_that("Quantiles can be computed for multiple chemicals at a time", {
    quantiles <- nhanes_quantile(nhanes_data,
                                 column = c("LBXPFUA", "LBXPFSA"),
                                 comment_column = c("LBDPFUAL", "LBDPFSAL"),
                                 weights_column = c("WTSA2YR", "WTSA2YR"),
                                 quantiles = c(0, 0.5, 0.75, 0.99, 1))

    expect_equivalent(quantiles$value, c(0.07, 0.12, 0.22, 1.45, 6.96, 0.07, 0.07, 0.07, 0.07, 0.62))
    expect_equal(quantiles$column, c(rep("LBXPFUA", 5), rep("LBXPFSA", 5)))
  })

  test_that("Quantiles can be computed for multiple chemicals at a time, without specifying weights", {
    quantiles <- nhanes_quantile(nhanes_data,
                                 column = c("LBXPFUA", "LBXPFSA"),
                                 comment_column = c("LBDPFUAL", "LBDPFSAL"),
                                 quantiles = c(0, 0.5, 0.75, 0.99, 1))

    expect_equivalent(quantiles$value, c(0.07, 0.12, 0.22, 1.45, 6.96, 0.07, 0.07, 0.07, 0.07, 0.62))
    expect_equal(quantiles$column, c(rep("LBXPFUA", 5), rep("LBXPFSA", 5)))
  })

  #nhanes_data <- nhanes_load_data(c())

  test_that("Quantiles can be computed for chemicals in multiple files", {
    nhanes_data <- nhanes_load_data(c("PFC", "PFC"), c("2009-2010", "2011-2012"), demographics = TRUE, cache = TRUE, destination = destination)

  })

})
