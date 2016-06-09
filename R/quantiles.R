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
  if(hasArg(filter) && substitute(filter) != "filter" && !exists(deparse(substitute(filter)), parent.frame())) {
    filter <- substitute(filter)
  }

  # Check to see if any of the computed quantiles are <LOD
  callback <- function(dat, df) {
    dl <- unique(lookup_dl(first(df$column), first(df$cycle)))

    if(length(dl) == 1) {
      df$below_lod <- df$value < dl
    } else if(length(dl) > 1) {
      warning("Multiple detection limits found")
    } else if(length(dl) == 0) {
      warning("No detection limit found from the summary tables. Falling back to inferring detection limit from the fill value.")

      inferred_dl <- dat[, first(df$column)][is.na(dat[, first(df$comment_column)]) == FALSE & dat[, first(df$comment_column)] == 1]

      if(length(unique(inferred_dl)) == 1) {
        inferred_dl <- first(inferred_dl)
        df$below_lod <- ifelse(df$value == lod, TRUE, FALSE)
        df$below_lod <- if(is.na(lod)) FALSE else df$below_lod
      }
      else {
        warning("Multiple detection limits were found. Falling back to computing detection frequency to infer if a quantile is below the limit of detection.")

        nd_quantiles <- nhanes_survey(svyquantile,
                                      dat,
                                      column = first(df$column),
                                      comment_column = first(df$comment_column),
                                      weights_column = first(df$weights_column),
                                      analyze = "comments",
                                      filter = filter,
                                      quantiles = 1 - quantiles,
                                      ci = F,
                                      na.rm = TRUE,
                                      method = "constant",
                                      f = 1,
                                      interval.type = "betaWald")

        df$below_lod <- nd_quantiles$value == 1
      }
    }

    return(df)
  }

  q <- nhanes_survey(svyquantile,
                     nhanes_data,
                     column,
                     comment_column,
                     weights_column,
                     filter = filter,
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
