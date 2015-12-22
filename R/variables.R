#' Load the NHANES comprehensive variable list
#'
#' @param destination where to save the variable list
#' @param cache whether to cache the downloaded variable list so it doesn't have to be redownloaded every time
#'
#' @import rvest
#' @export
nhanes_variables <- function(destination = tempfile(), cache = TRUE) {
  if(!dir.exists(dirname(destination))) {
    stop(paste0("Directory doesn't exist: ", dirname(destination)))
  }

  if(cache == FALSE || (cache == TRUE && !file.exists(destination))) {
    url <- "http://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx"

    message(paste0("Downloading NHANES variable list to ", destination));

    download.file(url, destination, method='auto', mode='wb')
  }

  # Parse the table and unpack the data frame
  dat <- read_html(destination) %>% html_table()
  dat <- dat[[1]]

  # Rename the columns to not have any spaces
  names(dat) <- gsub(" ", "", names(dat))

  # Add a cycle column
  dat$Cycle <- paste(dat$BeginYear, dat$EndYear, sep = "-")

  return(dat)
}
