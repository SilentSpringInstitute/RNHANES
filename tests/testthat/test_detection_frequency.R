test_that("Passes spot check on raw data", {
  dat <- load_nhanes_data("PFC", "2011-2012")
  freqs <- nhanes_detection_frequency(dat, c("LBDPFUAL", "LBDPFDOL"))

  expect_equal(freqs$LBDPFUAL, 0.6239496, tolerance = 1e-3)
  expect_equal(freqs$LBDPFDOL, 0.09401261, tolerance = 1e-3)
})

test_that("Passes spot check on recoded data", {
  dat <- load_nhanes_data("PFC", "2011-2012", recode = TRUE)
  freqs <- nhanes_detection_frequency(dat, c("LBDPFUAL", "LBDPFDOL"))

  expect_equal(freqs$LBDPFUAL, 0.6239496, tolerance = 1e-3)
  expect_equal(freqs$LBDPFDOL, 0.09401261, tolerance = 1e-3)
})
