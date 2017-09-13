#' Compute geometric means from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute geometric means for
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
#' nhanes_geometric_mean(dat, "URXUHG", "URDUHGLC", "WTSA2YR")
#' }
#'
#' @export
nhanes_geometric_mean <- function(nhanes_data, column, weights_column = "", filter = NULL) {
  if(hasArg(filter) && substitute(filter) != "filter" && !exists(deparse(substitute(filter)), parent.frame())) {
    filter <- substitute(filter)
  }

  # Check to see if any of the computed quantiles are <LOD
  callback <- function(dat, df) {


    return(df)
  }

  fun <- function(nhanes_data, column, comment_column, weights_column, des) {
    if(!is.null(filter)) {
      filter <- deparse(filter)
      output = eval(parse(text=filter), envir = nhanes_data)
      nhanes_data <- nhanes_data[output,]
    }

    f <- as.formula(paste0("~log(", column, ")"))
    logmean <- svymean(f, des, na.rm = TRUE)
    geometric_mean <- exp(logmean)[1]

    geometric_mean_lower = unname(exp(logmean[1] - 1.96 * sqrt(attr(logmean, "var"))))
    geometric_mean_upper = unname(exp(logmean[1] + 1.96 * sqrt(attr(logmean, "var"))))

    ret <- data.frame(
      value            = geometric_mean,
      value_lower      = geometric_mean_lower,
      value_upper      = geometric_mean_upper,
      cycle            = nhanes_data$cycle[1],
      begin_year       = nhanes_data$begin_year[1],
      end_year         = nhanes_data$end_year[1],
      file_name        = nhanes_data$file_name[1],
      column           = unname(column),
      weights_column   = unname(weights_column),
      comment_column   = unname(comment_column),
      stringsAsFactors = FALSE
    )

    rownames(ret) <- NULL

    return(ret)
  }
  ret <- nhanes_analyze(fun, nhanes_data, column, comment_column = FALSE, weights_column, filter = filter)
  ret$name <- "geometric mean"

  return(ret)
}
