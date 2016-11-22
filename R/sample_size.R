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
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' nhanes_sample_size(dat, "URXUHG", "URDUHGLC")
#' }
#'
#' @export
nhanes_sample_size <- function(nhanes_data, column, comment_column = "", weights_column = "", filter = NULL) {
  if(hasArg(filter) && substitute(filter) != "filter" && !exists(deparse(substitute(filter)), parent.frame())) {
    filter <- substitute(filter)
  }

  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    if(!is.null(filter)) {
      filter <- deparse(filter)
      output = eval(parse(text=filter), envir = nhanes_data)
      nhanes_data <- nhanes_data[output,]
    }

    sample_size <- sum(!is.na(nhanes_data[, column]))

    ret <- data.frame(
      value            = sample_size,
      cycle            = nhanes_data$cycle[1],
      begin_year       = nhanes_data$begin_year[1],
      end_year         = nhanes_data$end_year[1],
      file_name        = nhanes_data$file_name[1],
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
