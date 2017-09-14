test_that("Computing sample size works", {
  skip_on_cran()
  destination <- getOption("RNHANES_destination", tempdir())

  nhanes_data <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE, cache = TRUE, destination = destination)

  test_that("Passes spot check", {
    sample_size <- nhanes_sample_size(nhanes_data, column = "LBXPFNA", comment_column = "LBDPFNAL", weights_column = "WTSA2YR")
    expect_equal(sample_size$value, 1904)

    sample_size <- nhanes_sample_size(nhanes_data, column = "LBXPFOS", comment_column = "LBDPFNAL", weights_column = "WTSA2YR")
    expect_equal(sample_size$value, 1904)

    sample_size <- nhanes_sample_size(nhanes_data, column = "LBXPFOA", comment_column = "LBDPFNAL", weights_column = "WTSA2YR")
    expect_equal(sample_size$value, 1904)
  })

  test_that("Geometric means can be computed for multiple chemicals at a time", {
    sample_sizes <- nhanes_sample_size(nhanes_data,
                                       column = c("LBXPFOA", "LBXPFOS"),
                                       comment_column = "LBDPFNAL",
                                       weights_column = c("WTSA2YR", "WTSA2YR"))

    expect_equal(sample_sizes$value, c(1904, 1904))
  })

  test_that("Geometric means can be computed for multiple chemicals at a time, without specifying weights", {
    sample_sizes <- nhanes_sample_size(nhanes_data,
                                       column = c("LBXPFOA", "LBXPFOS"),
                                       comment_column = "LBDPFNAL")


    expect_equal(sample_sizes$value, c(1904, 1904))
  })

  test_that("Geometric means can be computed for chemicals in multiple files", {
    nhanes_data <- nhanes_load_data(c("PFC_F", "PFC_G"), c("2009-2010", "2011-2012"), demographics = TRUE, cache = TRUE, destination = destination)

    variables <- data.frame(
      file_name = c("PFC_F", "PFC_F", "PFC_G", "PFC_G"),
      cycle = c("2009-2010", "2009-2010", "2011-2012", "2011-2012"),
      column = c("LBXPFOA", "LBXPFOS", "LBXPFOA", "LBXPFOS"),
      comment_column = c("LBDPFOAL", "LBDPFOSL", "LBDPFOAL", "LBDPFOSL"),
      stringsAsFactors = FALSE
    )

    sample_sizes <- nhanes_sample_size(nhanes_data, column = variables)

    expect_equal(sample_sizes$cycle, c("2009-2010", "2009-2010", "2011-2012", "2011-2012"))
    expect_equal(sample_sizes$column, c("LBXPFOA", "LBXPFOS", "LBXPFOA", "LBXPFOS"))
    expect_equal(sample_sizes$value, c(2233, 2233, 1904, 1904))
  })
})
