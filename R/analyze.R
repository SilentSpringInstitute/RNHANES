#' Compute quantiles from NHANES weighted survey data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable
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
#'
#' @export

nhanes_analyze <- function(analysis_fun, nhanes_data, column, comment_column = "", weights_column = "") {
  # If `nhanes_data` is a list, the user is providing data from multiple cycles or multiple files.
  # `column` should be a data frame specifying the columns to use in each cycle.
  if(is.list(nhanes_data) && is.data.frame(nhanes_data) == FALSE) {
    if(is.data.frame(column) == FALSE) {
      stop("Second argument to nhanes_quantile should be a data frame")
    }

    dat <- lapply(nhanes_data, function(df) {
      rows <- subset(column, cycle == first(df$cycle))

      if("file_name" %in% names(column)) {
        rows <- subset(rows, file_name == first(df$file_name))
      }

      if(nrow(rows) > 0) {
        args <- list(analysis_fun, df, column = rows$column)

        args$weights_column = if("weights_column" %in% names(column)) rows$weights_column else ""
        args$comment_column = if("comment_column" %in% names(column)) rows$comment_column else ""

        return(do.call(nhanes_analyze, args))
      }
    })

    dat <- Reduce(rbind, dat)

    return(dat)

    # If `nhanes_data` is a data frame and column is a vector, then
    # we compute the quantiles for multiple columns in a data frame
  } else if(is.data.frame(nhanes_data) == TRUE && length(column) > 1) {
    dat <- apply(cbind(column, comment_column, weights_column), 1, function(row) {
      return(nhanes_analyze(analysis_fun,
                           nhanes_data,
                           column = row['column'],
                           weights_column = row['weights_column'],
                           comment_column = row['comment_column']))
    })

    dat <- Reduce(rbind, dat)

    return(dat)

  } else {
    # Make sure demographics data is included
    if("SDMVPSU" %in% names(nhanes_data) == FALSE || "SDMVSTRA" %in% names(nhanes_data) == FALSE) {
      stop("nhanes_data doesn't include demographics data, which is needed to compute quantiles. Use load_nhanes_data(... demographics = TRUE) to download data with demographics.")
    }

    if(weights_column == "") {
      weights_column <- names(nhanes_data)[grepl("WTS[A-Z]2YR", names(nhanes_data))]
      message(paste0("Weights column wasn't specified -- using ", weights_column, " for weights"))
    }

    if(weights_column %in% names(nhanes_data) == FALSE) {
      stop("Weights column doesn't exist")
    }

    if(comment_column %in% names(nhanes_data) == FALSE) {
      stop(paste0("Comment column ", comment_column, " doesn't exist"))
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

    # Decode comment column if necessary
    nhanes_data[, comment_column] = ifelse(nhanes_data[, comment_column] == "Below lower detection limit", 1, nhanes_data[,comment_column])
    nhanes_data[, comment_column] = ifelse(nhanes_data[, comment_column] == "At or above lower detection limit", 0, nhanes_data[,comment_column])

    args <- list(nhanes_data, column, comment_column, weights_column, des)

    ret <- analysis_fun(nhanes_data, column, comment_column, weights_column, des)

    return(ret)
  }
}
