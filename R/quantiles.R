#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable for checking if computed quantiles are below the LOD
#' @param weights_column name of the weights column
#' @param quantiles numeric or vector numeric of quantiles to compute
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
#' # Compute 50th, 95th, and 99th quantiles
#' nhanes_quantile(dat, "URXUHG", "WTSA2YR", c(0.5, 0.95, 0.99))
#' }
#'
#' @export
nhanes_quantile <- function(nhanes_data, column, comment_column = "", weights_column = "", quantiles = seq(0, 1, 0.25), filter = NULL) {
  if(hasArg(filter) && substitute(filter) != "filter") {
    filter <- substitute(filter)
  }

  # Check to see if any of the computed quantiles are <LOD
  callback <- function(dat, df) {
    # Figure out the percentage of nondetects
    nd_ratio <- sum(dat[,first(df$comment_column)] == 1, na.rm = TRUE) / sum(!is.na(dat[,first(df$comment_column)]))
    df$below_lod <- nd_ratio > quantiles
    return(df)
  }

  q <- nhanes_survey(svyquantile,
                nhanes_data,
                column,
                comment_column,
                weights_column,
                callback = callback,
                filter = filter,
                quantiles = quantiles,
                ci = F,
                na.rm = T,
                method = "constant",
                f = 1,
                interval.type="betaWald")
  q$quantile <- paste0(quantiles * 100, "%")
  q$name <- "quantile"

  return(q)
}
