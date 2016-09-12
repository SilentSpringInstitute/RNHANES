#' Plot a weighted histogram of an NHANES variable
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to plot
#' @param comment_column comment column of the variable to plot
#' @param weights_column name of the weights column
#' @param filter logical expression used to subset the data
#' @param transform transformation to apply to the column. Accepts any function name, for example: "log"
#' @param ... parameters passed through to svyhist function
#'
#' @return a data frame
#'
#' @import survey
#'
#' @examples
#'
#' \dontrun{
#' dat <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE)
#'
#' nhanes_hist(dat, "LBXPFOA")
#' }
#'
#' @export
nhanes_hist <- function(nhanes_data, column, comment_column, weights_column = "", filter = "", transform = "", ...) {
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

  if(transform != "") {
    column <- paste0(transform, "(", column, ")")
  }

  f <- as.formula(paste("~", paste(column, collapse = " + ")))

  svyhist(f, des, ...)
}
