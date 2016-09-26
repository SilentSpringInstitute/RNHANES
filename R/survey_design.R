#' Build survey objects for NHANES data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param weights_column name of the weights column
#'
#' @return a survey design object
#'
#' @import survey
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' design <- nhanes_survey_design(dat, "WTSA2YR")
#'
#' svymean(~RIDAGEYR, design)
#'
#' svyglm(URXUHG ~ RIDAGEYR + RIAGENDR, design)
#' }
#'
#' @export
nhanes_survey_design <- function(nhanes_data, weights_column = "") {
  if(is.list(nhanes_data) && !is.data.frame(nhanes_data)) {
    return(lapply(nhanes_data, nhanes_survey_design, weights_column = weights_column))
  }

  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    return(des)
  }

  nhanes_analyze(fun, nhanes_data, "", comment_column = FALSE, weights_column)
}
