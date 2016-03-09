#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable
#' @param weights_column name of the weights column
#' @param quantiles numeric or vector numeric of quantiles to compute
#'
#' @return a data frame
#'
#' @import survey
#' @importFrom dplyr first
#'
#' @examples
#'
#' dat <- load_nhanes_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' # Compute 50th, 95th, and 99th quantiles
#'
#' @export

nhanes_survey <- function(survey_fun, nhanes_data, column, comment_column = "", weights_column = "", analyze = "values", callback = NULL, ...) {
  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    if(analyze == "values") {
      formula = ~nhanes_data[, column]
    } else if(analyze == "comments") {
      formula = ~nhanes_data[, comment_column]
    }

    res <- as.vector(survey_fun(
      formula,
      ...,
      design = des
    ))

    ret <- data.frame(
      value            = unname(res),
      cycle            = first(nhanes_data$cycle),
      begin_year       = first(nhanes_data$begin_year),
      end_year         = first(nhanes_data$end_year),
      file_name        = first(nhanes_data$file_name),
      column           = unname(column),
      weights_column   = unname(weights_column),
      comment_column   = unname(comment_column),
      stringsAsFactors = FALSE
    )

    if(is.function(callback)) {
      ret <- callback(nhanes_data, ret)
    }

    return(ret)
  }

  nhanes_analyze(fun, nhanes_data, column, comment_column, weights_column)
}
