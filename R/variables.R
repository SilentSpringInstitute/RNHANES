#' Load the NHANES comprehensive variable list
#'
#' @param destination where to save the variable list
#' @param cache whether to cache the downloaded variable list so it doesn't have to be re-downloaded every time
#'
#' @import rvest
#' @importFrom xml2 read_html
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

  # Extract units (when available) into a separate column
  # TODO: think about how to fix this, right now it works but it picks up a lot of non-unit things
  units <- regmatches(dat$VariableDescription, gregexpr("\\((.*?)\\)", dat$VariableDescription))
  units[sapply(units, length) == 0] <- NA # Replace blanks with NAs
  units[dat$Component != 'Laboratory' & dat$Component != 'Examination'] <- NA # Replace anything picked up in the non-lab variables as NAs
  units <- Map(function(x) tail(x, 1), units)
  units <- unlist(units)
  units <- Map(function(x) substr(x, 2, nchar(x) - 1), units)


  dat$Unit <- unlist(units)


  return(dat)
}
