test_that("it checks the cached file has the correct components", {
  temp <- tempfile()

  nhanes_data_files(components = "laboratory", destination = temp)

  expect_that(nhanes_data_files(components = "all", destination = temp), throws_error("The cached file doesn't have all the components you specified in this call"))
})
