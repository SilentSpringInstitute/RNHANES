#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable for checking if computed quantiles are below the LOD
#' @param weights_column name of the weights column
#' @param quantiles numeric or vector numeric of quantiles to compute
#' @param filter logical expression used to subset the data
#' @param ... additional arguments passed to svyquantile
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
#' # Compute 50th, 95th, and 99th quantiles
#' nhanes_quantile(dat, "URXUHG", "URDUHGLC", "WTSA2YR", c(0.5, 0.95, 0.99))
#' }
#'
#' @export
nhanes_quantile <- function(nhanes_data, column, comment_column = "", weights_column = "", quantiles = seq(0, 1, 0.25), filter = NULL, ...) {
  if(hasArg(filter) && substitute(filter) != "filter" && !exists(deparse(substitute(filter)), parent.frame())) {
    filter <- substitute(filter)
  }

  # Check to see if any of the computed quantiles are <LOD
  callback <- function(dat, df) {
    if(df$comment_column[1] == FALSE) {
      df$below_lod <- NA
    }
    else {
      dl <- unique(lookup_dl(df$column[1], df$cycle[1]))

      if(length(dl) == 1) {
        df$below_lod <- df$value < dl
      } else if(length(dl) > 1) {
        warning("Multiple detection limits found")
      } else if(length(dl) == 0) {
        warning("No detection limit found from the summary tables. Falling back to inferring detection limit from the fill value.")

        inferred_dl <- dat[, df$column[1]][is.na(dat[, df$comment_column[1]]) == FALSE & dat[, df$comment_column[1]] == 1]

        if(length(unique(inferred_dl)) == 1) {
          inferred_dl <- inferred_dl[1]
          df$below_lod <- ifelse(df$value == inferred_dl, TRUE, FALSE)
          df$below_lod <- if(is.na(inferred_dl)) FALSE else df$below_lod
        }
        else {
          warning("Multiple detection limits were found. Falling back to computing detection frequency to infer if a quantile is below the limit of detection.")

          nd_quantiles <- nhanes_survey(svyquantile,
                                        dat,
                                        column = df$column[1],
                                        comment_column = df$comment_column[1],
                                        weights_column = df$weights_column[1],
                                        analyze = "comments",
                                        filter = filter,
                                        quantiles = 1 - quantiles,
                                        ci = F,
                                        na.rm = TRUE,
                                        ...)

          df$below_lod <- nd_quantiles$value == 1
        }
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
                     ...)

  q$quantile <- paste0(quantiles * 100, "%")
  q$name <- "quantile"

  return(q)
}
