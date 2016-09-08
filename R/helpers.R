# If no weights column is specified, try to guess the correct one.
# If a subsample weight is present, use that.
guess_weights_column <- function(cols) {
  guess <- cols[grepl("WTS[A-Z]+2YR?", cols)]
  weights_column = FALSE

  if(length(guess) > 0) {
    if(length(guess) > 1) {
      warning(paste("Multiple choices for a weights column -", paste(guess, collapse = ", ")))
    }
    weights_column = guess[1]
  }
  else {
    if("WTINT2YR" %in% cols) {
      weights_column <- "WTINT2YR"
    }
    else if("WTMEC2YR" %in% cols) {
      # Otherwise, use the full sample weights.
      weights_column <- "WTMEC2YR"
    }
    else {
      stop("Could not find a weights column. You can specify a weights column manually using the `weights` argument.")
    }
  }

  return(weights_column)
}

remove_na_weights <- function(nhanes_data, weights_column) {
  # Check to see if there are NAs in the weights
  na_count <- sum(is.na(nhanes_data[, weights_column]))

  if(na_count > 0) {
    message("Note: There are NAs in the weights column. These rows will be removed.")

    nhanes_data <- nhanes_data[!is.na(nhanes_data[, weights_column]), ]
  }

  return(nhanes_data)
}
