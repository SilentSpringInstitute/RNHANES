nhanes_data <- load_nhanes_data("PFC", "2011-2012", demographics = TRUE)

test_that("Passes spot check", {

  quantiles <- nhanes_quantile(nhanes_data, column = "LBXPFUA", weights_column = "WTSA2YR", quantiles = c(0, 0.5, 0.75, 0.99, 1))

  expect_equivalent(quantiles, c(0.07, 0.12, 0.22, 1.45, 6.96))
})
