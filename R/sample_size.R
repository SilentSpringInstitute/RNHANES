#' Compute the sample size of NHANES data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable for checking if computed quantiles are below the LOD
#' @param weights_column name of the weights column
#' @param filter logical expression used to subset the data
#'
#' @return a data frame
#'
#' @import survey
#' @importFrom dplyr first
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' nhanes_sample_size(dat, "URXUHG")
#' }
#'
#' @export
nhanes_sample_size <- function(nhanes_data, column, comment_column = "", weights_column = "", filter = NULL) {
  if(hasArg(filter) && substitute(filter) != "filter") {
    filter <- substitute(filter)
  }

  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    sample_size <- sum(!is.na(nhanes_data[, column]))

    ret <- data.frame(
      value            = sample_size,
      cycle            = first(nhanes_data$cycle),
      begin_year       = first(nhanes_data$begin_year),
      end_year         = first(nhanes_data$end_year),
      file_name        = first(nhanes_data$file_name),
      column           = unname(column),
      weights_column   = unname(weights_column),
      comment_column   = unname(comment_column),
      stringsAsFactors = FALSE
    )

    return(ret)
  }

  ret <- nhanes_analyze(fun, nhanes_data, column, comment_column, weights_column, filter = filter)

  ret$name = "sample size"

  return(ret)
}
