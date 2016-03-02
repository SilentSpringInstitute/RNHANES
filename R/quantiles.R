#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param weights_column name of the weights column
#' @param quantiles numeric or vector numeric of quantiles to compute
#'
#' @return vector of quantiles
#'
#' @import survey
#'
#' @examples
#'
#' dat <- load_nhanes_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' # Compute 50th, 95th, and 99th quantiles
#' nhanes_quantile(dat, "URXUHG", "WTSA2YR", c(0.5, 0.95, 0.99))
#'
#' @export
nhanes_quantile <- function(nhanes_data, column, weights_column, quantiles = seq(0, 1, 0.25)) {
  # Make sure demographics data is included
  if("SDMVPSU" %in% names(nhanes_data) == FALSE || "SDMVSTRA" %in% names(nhanes_data) == FALSE) {
    stop("nhanes_data doesn't include demographics data, which is needed to compute quantiles. Use load_nhanes_data(... demographics = TRUE) to download data with demographics.")
  }

  # Make sure the column and weights_column are actual columns
  if(column %in% names(nhanes_data) == FALSE) {
    stop("Column doesn't exist")
  }

  if(weights_column %in% names(nhanes_data) == FALSE) {
    stop("Weights column doesn't exist")
  }

  # Check to see if there are NAs in the weights
  na_count <- sum(is.na(nhanes_data[, weights_column]))

  if(na_count > 0) {
    warning("Warning: NAs in weights column. These samples will be removed.")

    nhanes_data <- nhanes_data[!is.na(nhanes_data[, weights_column]), ]
  }

  # Build the survey object
  des <- svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = T,
    weights = nhanes_data[, weights_column],
    data = nhanes_data
  )

  res <- as.vector(svyquantile(
    ~nhanes_data[, column],
    design = des,
    quantiles = quantiles,
    ci = F,
    na.rm = T,
    method = "constant",
    f = 1,
    interval.type="betaWald"
  ))

  names(res) <- paste0(quantiles, "%")

  return(res)
}
