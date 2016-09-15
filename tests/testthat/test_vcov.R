test_that("nhanes_vcov works correctly", {
  skip_on_cran()
  nhanes_data <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE)

  test_that("nhanes_vcov gives correct output for one data file", {
    covariance <- nhanes_vcov(nhanes_data, columns = c("LBXPFOA", "LBXPFOS"))

    expect_equivalent(covariance[1,1], 0.009254430)
    expect_equivalent(covariance[1,2], 0.002912777)
    expect_equivalent(covariance[2,1], 0.002912777)
    expect_equivalent(covariance[2,2], 0.128068849)
  })

  test_that("nhanes_vcov errors on incorrect column inputs", {
    expect_error(covariance <- nhanes_vcov(nhanes_data, columns = c("test", "test2")))
  })

  test_that("nhanes_vcov does not accept a list as input", {
    test_data <- list()

    expect_error(nhanes_vcov(test_data, columns = c("test1", "test2")))
  })
})
