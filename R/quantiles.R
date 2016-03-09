#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable for checking if computed quantiles are below the LOD
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
#' nhanes_quantile(dat, "URXUHG", "WTSA2YR", c(0.5, 0.95, 0.99))
#'
#' @export
nhanes_quantile <- function(nhanes_data, column, comment_column, weights_column = "", quantiles = seq(0, 1, 0.25)) {
  # Check to see if any of the computed quantiles are <LOD
  callback <- function(dat, df) {
    # Figure out the LOD (if it exists) by selecting values for which the comment column indicates a nondetect
    lod <- first(dat[, first(df$column)][is.na(dat[, first(df$comment_column)]) == FALSE & dat[, first(df$comment_column)] == 1])
    df$below_lod <- ifelse(df$value == lod, TRUE, FALSE)
    df$below_lod <- if(is.na(lod)) FALSE else df$below_lod
    return(df)
  }

  q <- nhanes_survey(svyquantile,
                nhanes_data,
                column,
                comment_column,
                weights_column,
                callback = callback,
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
