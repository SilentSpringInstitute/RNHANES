#' Compute detection frequencies of NHANES data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column names of the variables to compute detection frequencies for
#' @param comment_column comment column names of the variables to compute detection frequencies for
#' @param weights_column sample weight column
#' @return named vector of detection frequencies
#'
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' # Compute detection frequency
#' nhanes_detection_frequency(dat, c("URXUHG"), c("URDUHGLC"))
#' }
#'
#' @export
nhanes_detection_frequency <- function(nhanes_data, column, comment_column, weights_column = "") {

  # Check to see if we need to decode the comment column
  df <- nhanes_survey(svymean,
                      nhanes_data,
                      column = column,
                      comment_column = comment_column,
                      weights_column = weights_column,
                      analyze = "comments",
                      na.rm = TRUE)

  df$value = 1 - df$value
  df$name = "detection_frequency"

  return(df)
}
