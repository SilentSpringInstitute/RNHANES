test_that("Download helper functions work", {
  test_that("file_suffix works", {
    expect_equal(file_suffix("2001-2002"), "B")
    expect_equal(file_suffix("2003-2004"), "C")
    expect_equal(file_suffix("2013-2014"), "H")

    expect_equal(file_suffix(c("2001-2002", "2003-2004")), c("B", "C"))
  })

  test_that("demography_filename", {
    test_that("it works for one year at a time", {
      expect_equal(demography_filename("2007-2008"), "DEMO_E.XPT")
      expect_equal(demography_filename("1999-2000"), "DEMO.XPT")
    })

    test_that("it works for multiple years at a time", {
      expect_equal(demography_filename(c("1999-2000", "2007-2008")), c("DEMO.XPT", "DEMO_E.XPT"))
    })
  })

  test_that("validate_year", {
    test_that("validate_year works for one year", {
      expect_true(validate_year("2001-2002"))
      expect_true(validate_year("2005-2006"))
      expect_true(validate_year("2013-2014"))
      expect_true(validate_year("1999-2000"))

      expect_error(validate_year("2001"))
      expect_error(validate_year(2001))
      expect_error(validate_year("test"))
    })

    test_that("validate_year works for multiple years", {
      expect_true(validate_year("2001-2002", "1999-2000"))
      expect_error(validate_year(c("2001-2002", "1999")))
      expect_error(validate_year("1999", "test", 1))
    })
  })

  test_that("process_file_name", {
    test_that("it leaves file names with .XPT or .htm extensions unchaged", {
      expect_equal(process_file_name("EPH_E.XPT", "2007-2008"), "EPH_E.XPT")
    })

    test_that("it adds XPT as an extension by default", {
      expect_equal(process_file_name("EPH_E", "2007-2008"), "EPH_E.XPT")
    })

    test_that("it can add other extensions", {
      expect_equal(process_file_name("EPH_E", "2007-2008", extension = ".htm"), "EPH_E.htm")
    })

    test_that("it can add the correct suffix", {
      expect_equal(process_file_name("EPH", "2007-2008"), "EPH_E.XPT")
    })

    test_that("it throws a warning if there is a wrong suffix, but still adds extension", {
      expect_warning(val <- process_file_name("EPH_A", "2007-2008"), "The file name EPH_A is probably incorrect")
      expect_equal(val, "EPH_A.XPT")
    })

    test_that("it adds a suffix if the file name ends with an underscore", {
      expect_equal(process_file_name("EPH_", "2007-2008"), "EPH_E.XPT")
    })

    test_that("it can process multiple file names/years at once", {
      expect_equal(process_file_name(c("EPH", "PFC"), c("2007-2008", "2011-2012")), c("EPH_E.XPT", "PFC_G.XPT"))
    })

    test_that("it handles 1999-2000 cycle correctly", {
      test_that("it prints a message for 1999-2000 cycle", {
        expect_message(process_file_name("EPH", "1999-2000"), "Cycle 1999-2000 doesn't always follow the normal naming convention")
      })

      test_that("it will add an extension", {
        expect_equal(process_file_name("EPH", "1999-2000"), "EPH.XPT")
      })
    })
  })

})

# Test of nhanes_variables
test_that("Downloading files from NHANES works", {
  skip_on_cran()

  test_that("nhanes_variables", {
    test_that("it downloads the file", {
      skip_on_cran()

      destination = tempfile()

      dat <- nhanes_variables(destination = destination)

      expect_true(file.exists(destination))
      expect_more_than(nrow(dat), 100)
    })
  })


  # Test of nhanes_data_files
  test_that("nhanes_data_files", {
    test_that("it downloads the files", {
      skip_on_cran()

      destination = tempfile()

      dat <- nhanes_data_files(destination = destination)

      expect_true(file.exists(destination))
      expect_more_than(nrow(dat), 100)
    })

    test_that("it can download only one component", {
      skip_on_cran()

      dat <- nhanes_data_files(components = "laboratory")

      expect_that(unique(dat$component), equals(c("laboratory")))
    })

    test_that("it checks the cached file has the correct components", {
      skip_on_cran()

      temp <- tempfile()

      nhanes_data_files(components = "laboratory", destination = temp)

      expect_that(nhanes_data_files(components = "all", destination = temp), throws_error("The cached file doesn't have all the components you specified in this call"))
    })
  })


  # Test of nhanes_load_data
  test_that("nhanes_load_data", {
    destination <- tempdir()

    test_that("it will throw an error if the destination folder doesn't exist", {
      expect_error(nhanes_load_data("EPH", "2007-2008", destination = file.path(destination, "not_there")))
    })

    test_that("it can download a basic data file", {
      dat <- nhanes_load_data("EPH", "2007-2008", destination = destination, cache = TRUE)

      expect_true(file.exists(file.path(destination, "EPH_E.csv")))
      expect_equal(nrow(dat), 2718)
      expect_equivalent(names(dat), c("SEQN", "WTSB2YR", "URXUCR", "URX4TO", "URD4TOLC", "URXBP3", "URDBP3LC", "URXBPH", "URDBPHLC", "URXTRS",
                                      "URDTRSLC", "URXBUP", "URDBUPLC", "URXEPB", "URDEPBLC", "URXMPB", "URDMPBLC", "URXPPB", "URDPPBLC",
                                      "file_name", "cycle", "begin_year", "end_year"))

      unlink(file.path(destination, "EPH_E.csv"))
    })

    test_that("it can download a basic data file without caching", {
      dat <- nhanes_load_data("EPH", "2007-2008", destination = destination, cache = FALSE)

      expect_true(file.exists(file.path(destination, "EPH_E.XPT")))
      expect_equal(nrow(dat), 2718)
      expect_equivalent(names(dat), c("SEQN", "WTSB2YR", "URXUCR", "URX4TO", "URD4TOLC", "URXBP3", "URDBP3LC", "URXBPH", "URDBPHLC", "URXTRS",
                                      "URDTRSLC", "URXBUP", "URDBUPLC", "URXEPB", "URDEPBLC", "URXMPB", "URDMPBLC", "URXPPB", "URDPPBLC",
                                      "file_name", "cycle", "begin_year", "end_year"))

      unlink(file.path(destination, "EPH_E.XPT"))
    })

    test_that("it can download a file with demographics", {
      dat <- nhanes_load_data("EPH", "2007-2008", demographics = TRUE, destination = destination, cache = TRUE)

      expect_true(file.exists(file.path(destination, "EPH_E_demographics.csv")))
      expect_equal(nrow(dat), 2718)
      expect_equal(c("SDDSRVYR", "RIDAGEYR", "WTSB2YR") %in% names(dat), rep(TRUE, 3))

      # Clean up
      unlink(file.path(destination, "EPH_E.csv"))
      unlink(file.path(destination, "EPH_E_demographics.csv"))
      unlink(file.path(destination, "DEMO_E.csv"))
    })

    test_that("it can recode just data", {
      dat <- nhanes_load_data("EPH", "2007-2008", recode_data = TRUE, destination = destination, cache = TRUE)

      expect_true(file.exists(file.path(destination, "EPH_E_recoded_data.csv")))
      expect_equal(nrow(dat), 2718)
      expect_equivalent(names(dat), c("SEQN", "WTSB2YR", "URXUCR", "URX4TO", "URD4TOLC", "URXBP3", "URDBP3LC", "URXBPH", "URDBPHLC", "URXTRS",
                                      "URDTRSLC", "URXBUP", "URDBUPLC", "URXEPB", "URDEPBLC", "URXMPB", "URDMPBLC", "URXPPB", "URDPPBLC",
                                      "file_name", "cycle", "begin_year", "end_year"))

      expect_equivalent(unique(dat$URDPPBLC)[c(1, 3)], c("At or above the detection limit", "Below lower detection limit"))

      # Clean up
      unlink(file.path(destination, "EPH_E_recoded_data.csv"))
      unlink(file.path(destination, "EPH_E.csv"))
    })

    test_that("it can recode just demographics", {
      dat <- nhanes_load_data("EPH", "2007-2008", demographics = TRUE, recode_demographics = TRUE, destination = destination, cache = TRUE)

      expect_true(file.exists(file.path(destination, "EPH_E_demographics_recoded_demographics.csv")))
      expect_equal(nrow(dat), 2718)
      expect_equal(c("SDDSRVYR", "RIDAGEYR", "WTSB2YR") %in% names(dat), rep(TRUE, 3))

      # Make sure the data isn't recoded
      expect_equivalent(unique(dat$URDPPBLC), c(0, NA, 1))

      # Make sure the demographics have been recoded
      expect_equivalent(unique(dat$RIAGENDR), c("Female", "Male"))

      # Clean up
      unlink(file.path(destination, "EPH_E_demographics.csv"))
      unlink(file.path(destination, "DEMO_E.csv"))
      unlink(file.path(destination, "DEMO_E_description.csv"))
    })

    test_that("it can recode data and demographics", {
      dat <- nhanes_load_data("EPH", "2007-2008", demographics = TRUE, recode = TRUE, destination = destination, cache = TRUE)

      expect_true(file.exists(file.path(destination, "EPH_E_demographics_recoded.csv")))
      expect_equal(nrow(dat), 2718)
      expect_equal(c("SDDSRVYR", "RIDAGEYR", "WTSB2YR") %in% names(dat), rep(TRUE, 3))

      # Make sure the data is recoded
      expect_equivalent(unique(dat$URDPPBLC)[c(1, 3)], c("At or above the detection limit", "Below lower detection limit"))

      # Make sure the demographics have been recoded
      expect_equivalent(unique(dat$RIAGENDR), c("Female", "Male"))

      # Clean up
      unlink(file.path(destination, "EPH_E_demographics.csv"))
      unlink(file.path(destination, "DEMO_E.csv"))
      unlink(file.path(destination, "DEMO_E_description.csv"))
    })

    test_that("it can download multiple files from the same year", {
      dat <- nhanes_load_data(c("EPH", "PFC"), "2007-2008", destination = destination, cache = TRUE)

      expect_equal(length(dat), 2)
      expect_equal(nrow(dat$EPH), 2718)
      expect_equal(nrow(dat$PFC), 2294)

      expect_true(file.exists(file.path(destination, "PFC_E.csv")))
      expect_true(file.exists(file.path(destination, "EPH_E.csv")))

      expect_equal(dat$EPH$cycle[1], "2007-2008")
      expect_equal(dat$PFC$cycle[1], "2007-2008")

      # Clean up
      unlink(file.path(destination, "PFC_E.csv"))
      unlink(file.path(destination, "EPH_E.csv"))
    })

    test_that("it can download multiple files from different years", {
      dat <- nhanes_load_data(c("EPH", "PHTHTE"), c("2007-2008", "2011-2012"), destination = destination, cache = TRUE)

      expect_equal(length(dat), 2)
      expect_equal(nrow(dat$EPH), 2718)
      expect_equal(nrow(dat$PHTHTE), 2594)

      expect_true(file.exists(file.path(destination, "PHTHTE_G.csv")))
      expect_true(file.exists(file.path(destination, "EPH_E.csv")))

      expect_equal(dat$EPH$cycle[1], "2007-2008")
      expect_equal(dat$PHTHTE$cycle[1], "2011-2012")

      # Clean up
      unlink(file.path(destination, "PHTHTE_G.csv"))
      unlink(file.path(destination, "EPH_E.csv"))
    })

    test_that("it won't accept factors as inputs", {
      expect_error(nhanes_load_data(as.factor(c("EPH", "PFC")), "2007-2008"))
      expect_error(nhanes_load_data(c("EPH", "PFC"), as.factor(c("2007-2008", "2009-2010"))))
    })

    test_that("it will warn you if you're trying to download duplicate files", {
      expect_warning(nhanes_load_data(c("EPH", "EPH"), "2007-2008", allow_duplicate_files = TRUE))
    })

    test_that("it will deduplicate files by default", {
      dat <- nhanes_load_data(c("EPH", "EPH"), "2007-2008")

      expect_equal(length(dat), 1)
    })

    test_that("it doesn't need to have a destination specified", {
      dat <- nhanes_load_data("EPH", "2007-2008", cache = TRUE)

      expect_true(file.exists(file.path(tempdir(), "EPH_E.csv")))
      expect_more_than(nrow(dat), 0)

      # Clean up
      unlink(file.path(tempdir(), "EPH_E.csv"))
    })

    test_that("if destination not specified, checks option", {
      original <- getOption("RNHANES_destination")

      temp <- tempdir()
      test_destination <- file.path(temp, "test")
      dir.create(test_destination)

      options(RNHANES_destination = test_destination)

      dat <- nhanes_load_data("EPH", "2007-2008", cache = TRUE)

      expect_true(file.exists(file.path(test_destination, "EPH_E.csv")))

      # Clean up
      options(RNHANES_destination = original)
      unlink(file.path(test_destination, "EPH_E.csv"))
      unlink(test_destination, recursive = TRUE)
    })

    test_that("if cache not specified, checks option", {
      original <- getOption("RNHANES_cache")

      options(RNHANES_cache = FALSE)

      dat <- nhanes_load_data("EPH", "2007-2008", destination = destination)

      expect_true(file.exists(file.path(destination, "EPH_E.XPT")))
      expect_false(file.exists(file.path(destination, "EPH_E.csv")))

      # Clean up
      options(RNHANES_cache = original)
      unlink(file.path(destination, "EPH_E.XPT"))
    })
  })

  test_that("nhanes_load_demography_data", {
    test_that("if destination not specified, checks option", {
      original <- getOption("RNHANES_destination")

      temp <- tempdir()
      test_destination <- file.path(temp, "test")
      dir.create(test_destination)

      options(RNHANES_destination = test_destination)

      dat <- nhanes_load_demography_data("2007-2008", cache = TRUE)

      expect_true(file.exists(file.path(test_destination, "DEMO_E.csv")))

      # Clean up
      options(RNHANES_destination = original)
      unlink(file.path(test_destination, "DEMO_E.csv"))
      unlink(test_destination, recursive = TRUE)
    })

    test_that("if cache not specified, checks option", {
      original <- getOption("RNHANES_cache")

      options(RNHANES_cache = FALSE)

      dat <- nhanes_load_demography_data("2007-2008", destination = destination)

      expect_true(file.exists(file.path(destination, "DEMO_E.XPT")))
      expect_false(file.exists(file.path(destination, "DEMO_E.csv")))

      # Clean up
      options(RNHANES_cache = original)
      unlink(file.path(destination, "DEMO_E.XPT"))
    })
  })
})
