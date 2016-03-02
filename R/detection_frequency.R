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
#' @return detection frequency
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
  # Make sure we have a comment column, and not a data column
  # comment columns should only have 1s, 0s, or and NAs
  correct_vals <- sum(nhanes_data[, comment_columns] != 1 & nhanes_data[, comment_columns] != 0, na.rm = TRUE) == 0
  if(correct_vals == FALSE) {
    stop("nhanes_detection_frequency was given a column that doesn't seem to be a comment column.")
  }

  nhanes_data[, comment_columns] %>%
    detection_frequency() %>%
    t() %>%
    unname() %>%
    as.vector() %>%
    return()
}
