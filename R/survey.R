#' Apply a function from the survey package to NHANES data
#'
#' @param survey_fun the survey package function (e.g. svyquantile or svymean)
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable
#' @param weights_column name of the weights column
#' @param filter logical expression used to subset the data
#' @param analyze one of "values" or "comments", whether to apply the survey function to the value or comment column.
#' @param callback optional function to execute on each row of the dataframe
#' @param ... other arguments to pass to the survey function
#'
#' @return a data frame
#'
#'
#' @details
#' This function provides a generic way to apply any function from the survey package to NHANES data.
#' RNHANES provides specific wrappers for computing quantiles (nhanes_quantile) and detection frequencies (nhanes_detection_frequency),
#' and this function provides a general way to use any survey function.
#'
#' @examples
#' \dontrun{
#' library(survey)
#'
#' nhanes_data <- nhanes_load_data("EPH", "2011-2012", demographics = TRUE)
#'
#' # Compute the mean of triclosan using the svymean function
#' nhanes_survey(svymean, nhanes_data, "URXTRS", "URDTRSLC", na.rm = TRUE)
#'
#' # Compute the variance using svyvar
#' nhanes_survey(svyvar, nhanes_data, "URXTRS", "URDTRSLC", na.rm = TRUE)
#'
#' }
#'
#' @export

nhanes_survey <- function(survey_fun, nhanes_data, column, comment_column = "", weights_column = "", filter = NULL, analyze = "values", callback = NULL, ...) {
  if(hasArg(filter) && substitute(filter) != "filter" && !exists(deparse(substitute(filter)), parent.frame())) {
    filter <- substitute(filter)
  }

  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    if(analyze == "values") {
      formula = make.formula(column)
    } else if(analyze == "comments") {
      formula = make.formula(comment_column)
    }

    if(analyze == "comments" && comment_column == FALSE) {
      res <- c(NA)
    }
    else {
    res <- as.vector(survey_fun(
      formula,
      ...,
      design = des
    ))
    }
    
    if(class(res) == "newsvyquantile") {
      res <- res[[1]]
    }

    ret <- data.frame(
      value            = as.vector(unname(res)),
      cycle            = nhanes_data$cycle[1],
      begin_year       = nhanes_data$begin_year[1],
      end_year         = nhanes_data$end_year[1],
      file_name        = nhanes_data$file_name[1],
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


  nhanes_analyze(fun, nhanes_data, column, comment_column, weights_column, filter = filter)
}
