test_that("nhanes_search works", {
  skip_on_cran()

  destination = tempdir()

  vars <- nhanes_variables(destination = destination)
  files <- nhanes_data_files(destination = destination)

  test_that("nhanes_search on variables passes spot check", {
    results <- nhanes_search(vars, "mono-ethyl")

    expect_equal(results$variable_name, c("URDMEPLC", "URXMEP", "URXMEP", "URDMEPLC", "URXMEP", "URDMEPLC", "URXMEP", "URXMEP", "URDMEPLC", "URXMEP", "URDMEPLC", "URXMEP", "URDMEPLC", "URXMEP", "URDMEPLC", "URXMEP", "URDMEPLC", "URXMEP"))
    expect_equal(results$begin_year, c(2005, 2005, 1999, 2007, 2007, 2003, 2003, 2001, 2009, 2009, 2011, 2011, 2013, 2013, 2015, 2015, 2017, 2017))
  })

  test_that("nhanes_search on files passes spot check", {
    results <- nhanes_search(files, "Polyfluoroalkyl")

    expect_equal(results$data_file_name, c("PFAS_I", "SSPFSU_H", "PFAS_J", "SSPFAS_H", "PFAS_H", "SSPFAS_J", "SSPFAC_H", "PFC_E", "PFC_D", "L24PFC_C", "PFC_F", "PFC_G", "PFC_POOL"
))
    expect_equal(results$cycle, c("2015-2016", "2013-2014", "2017-2018", "2013-2014", "2013-2014", "2017-2018", "2013-2014", "2007-2008", "2005-2006", "2003-2004", "2009-2010", "2011-2012", "2001-2002"))
  })

  test_that("fuzzy search works on variables", {
    results <- nhanes_search(vars, "Perfluorooctanoic", fuzzy = TRUE, cycle == "2003-2004")

    expect_equal(nrow(results), 17)
  })

  test_that("fuzzy search works on files", {
    results <- nhanes_search(files, "fluoro", fuzzy = TRUE)

    expect_equal(nrow(results), 26)
    expect_equal(results$data_file_name, c("FLXCLN_G", "FLXCLN_H", "FLXCLN_I", "FLXCLN_F", NA, "SSANA_A", "SSANA2_G", "SSANA2_A", "FLDEP_H", "FLDEP_I", "FLDEW_H", "FLDEW_I",  "PFAS_I", "SSPFSU_H", "PFAS_J", "SSPFAS_H",
                                           "PFAS_H", "SSPFAS_J", "SSPFAC_H", "SSPFC_A", "PFC_E", "PFC_D", "L24PFC_C", "PFC_F", "PFC_G", "PFC_POOL"))
  })
})
