#' Compute quantiles from NHANES weighted survey data
#'
#' @param analysis_fun function to use to analyze each variable
#' @param nhanes_data data frame containing NHANES data
#' @param column column name of the variable to compute quantiles for
#' @param comment_column comment column name of the variable
#' @param weights_column name of the weights column
#' @param filter logical expression used to subset the data
#'
#' @return a data frame
#'
#' @import survey
#'
nhanes_analyze <- function(analysis_fun, nhanes_data, column, comment_column = "", weights_column = "", filter = NULL) {
  # Workaround
  # without this, R CMD CHECK will complain about file_name being used in the subset call because it
  # looks like a global variable
  # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  file_name <- NULL

  # If `nhanes_data` is a list, the user is providing data from multiple cycles or multiple files.
  # `column` should be a data frame specifying the columns to use in each cycle.
  if(is.list(nhanes_data) && is.data.frame(nhanes_data) == FALSE && is.data.frame(column)) {
    dat <- lapply(nhanes_data, function(df) {
      rows <- column[column$cycle == df$cycle[1],]

      if("file_name" %in% names(column)) {
        rows <- rows[rows$file_name == df$file_name[1],]
      }

      if(nrow(rows) > 0) {
        args <- list(analysis_fun, df, column = rows$column)

        args$weights_column = if("weights_column" %in% names(column)) rows$weights_column else ""
        args$comment_column = if("comment_column" %in% names(column)) rows$comment_column else ""
        args$filter = filter

        return(do.call(nhanes_analyze, args))
      }
    })

    dat <- Reduce(rbind, dat)

    return(dat)

    # If `nhanes_data` is a data frame and column is a vector, then
    # we compute the quantiles for multiple columns in a data frame
  } else if(is.list(nhanes_data) && is.data.frame(nhanes_data) == FALSE && is.vector(column)) {
    stop(paste("To analyze data from multiple files, please provide a data frame that indicates which columns to analyze in each file.",
               "The data frame should have the following columns: 'file_name', 'cycle', 'column', 'comment_column', 'weights_column' (optional)"))
  } else if(is.data.frame(nhanes_data) == TRUE && length(column) > 1) {
    dat <- apply(cbind(column, comment_column, weights_column), 1, function(row) {
      return(nhanes_analyze(analysis_fun,
                           nhanes_data,
                           column = row['column'],
                           weights_column = row['weights_column'],
                           comment_column = row['comment_column'],
                           filter = filter))
    })

    dat <- Reduce(rbind, dat)

    return(dat)

  } else {
    # Make sure demographics data is included
    if("SDMVPSU" %in% names(nhanes_data) == FALSE || "SDMVSTRA" %in% names(nhanes_data) == FALSE) {
      stop("nhanes_data doesn't include demographics data, which is needed for computing summary statistics using the survey weights. Use load_nhanes_data(... demographics = TRUE) to download data with demographics.")
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

    if(weights_column %in% names(nhanes_data) == FALSE) {
      stop("Weights column doesn't exist")
    }

    if(comment_column != FALSE && comment_column %in% names(nhanes_data) == FALSE) {
      stop(paste0("Comment column ", comment_column, " doesn't exist"))
    }

    nhanes_data <- remove_na_weights(nhanes_data, weights_column)

    # Decode comment column if necessary
    if(comment_column != FALSE) {
      nhanes_data[, comment_column] = ifelse(nhanes_data[, comment_column] == "Below lower detection limit", 1, nhanes_data[,comment_column])
      nhanes_data[, comment_column] = ifelse(nhanes_data[, comment_column] == "At or above the detection limit", 0, nhanes_data[,comment_column])
      nhanes_data[, comment_column] = as.numeric(nhanes_data[, comment_column])
    }

    # Build the survey object
    des <- svydesign(
      ids = ~SDMVPSU,
      strata = ~SDMVSTRA,
      nest = T,
      weights = nhanes_data[, weights_column],
      data = nhanes_data
    )

    # if subset is defined, subset the survey object
    if(!is.null(filter)) {
      filter <- deparse(filter)
      output = eval(parse(text=filter), envir = des$variables)
      des <- des[output,]
    }


    ret <- analysis_fun(nhanes_data, column, comment_column, weights_column, des)

    return(ret)
  }
}
