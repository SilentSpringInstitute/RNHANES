test_that("Passes spot check on raw data", {
  dat <- nhanes_load_data("PFC", "2011-2012", demographics = TRUE)
  freqs <- nhanes_detection_frequency(dat, c("LBXPFUA", "LBXPFDO"), c("LBDPFUAL", "LBDPFDOL"))

  expect_equal(freqs$value, c(0.5926, 0.0908), tolerance = 1e-3)
})

test_that("Passes spot check on recoded data", {
  dat <- nhanes_load_data("PFC", "2011-2012", demographics = TRUE, recode = TRUE)
  freqs <- nhanes_detection_frequency(dat, c("LBXPFUA", "LBXPFDO"), c("LBDPFUAL", "LBDPFDOL"))

  expect_equal(freqs$value, c(0.5926, 0.0908), tolerance = 1e-3)
})
