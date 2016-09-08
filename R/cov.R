#' Compute the covariance between NHANES variables
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param weights_column name of the weights column
#' @param filter logical expression used to subset the data
#'
#' @return a data frame
#'
#' @import survey
#' @importFrom stats vcov
#' @importFrom stats as.formula
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE)
#'
#' nhanes_vcov(dat, c("LBXPFOA", "LBXPFOS"))
#' }
#'
#' @export
nhanes_vcov <- function(nhanes_data, column, weights_column = "", filter = "") {
  if(is.list(nhanes_data) && !is.data.frame(nhanes_data)) {
    stop("nhanes_vcov does not support multiple cycle years/files as input. Please supply a data frame.")
  }

  if(weights_column == "") {
    # If no weights column is specified, try to guess the correct one.
    # If a subsample weight is present, use that.
    weights_column <- guess_weights_column(names(nhanes_data))

    if(!weights_column %in% names(nhanes_data)) {
      stop("Could not find a weights column")
    }

    message(paste0("Weights column wasn't specified -- using ", weights_column, " for weights"))
  }

  nhanes_data <- remove_na_weights(nhanes_data, weights_column)

  des <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = T,
    weights = nhanes_data[, weights_column],
    data = nhanes_data
  )

  f <- as.formula(paste("~", paste(column, collapse = " + ")))

  vcov(svymean(f, des, na.rm = TRUE))
}
