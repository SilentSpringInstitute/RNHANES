#' Computes detection frequencies on a set of columns
#' @importFrom dplyr summarise_each
#' @importFrom dplyr funs
detection_frequency <- function(col) {
  if(is.data.frame(col)) {
    return(col %>% summarise_each(funs(detection_frequency(.))))
  }
  else {
    return(sum(col == 0, na.rm = TRUE) / sum(!is.na(col)))
  }
}

#' Compute detection frequencies of NHANES data
#'
#' @param nhanes_data data frame containing NHANES data
#' @param columns comment column names of the variables to compute detection frequencies for
#'
#' @return named vector of detection frequencies
#'
#'
#' @examples
#'
#' dat <- load_nhanes_data("UHG_G", "2011-2012", demographics = TRUE)
#'
#' # Compute detection frequency
#' nhanes_detection_frequency(dat, c("URXUHG"))
#'
#' @export
nhanes_detection_frequency <- function(nhanes_data, comment_columns) {
  # if the comment column was recoded, it will have 'Below lower detection limit', 'At or above the detection limit', or NAs
  # convert these back to 1s and 0s
  cols <- apply(nhanes_data[, comment_columns], 2, function(column) {
    ifelse(is.na(column), NA,
           ifelse(column == "Below lower detection limit", 1,
                  ifelse(column == "At or above the detection limit", 0, column)))
  }) %>%
    as.data.frame()

  # Make sure we have a comment column, and not a data column
  # comment columns should only have 1s, 0s, or and NAs
  correct_vals <- sum(cols != 1 & cols != 0, na.rm = TRUE) == 0
  if(correct_vals == FALSE) {
    stop("nhanes_detection_frequency was given a column that doesn't seem to be a comment column.")
  }

  cols <- apply(cols, 2, as.numeric) %>%
    as.data.frame()

  cols %>%
    detection_frequency() %>%
    return()
}
