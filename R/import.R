demography_filename <- function(year) {
  validate_year(year)

  if(year == '1999-2000') {
    return("DEMO.XPT")
  }

  suffix = switch(year,
                  '2001-2002' = 'B',
                  '2003-2004' = 'C',
                  '2005-2006' = 'D',
                  '2007-2008' = 'E',
                  '2009-2010' = 'F',
                  '2011-2012' = 'G',
                  '2013-2014' = 'H'
  )

  return(paste0("DEMO_", suffix, ".XPT"))
}

validate_year <- function(year, throw_error = TRUE) {
  valid <- switch(year,
                  '1999-2000' = TRUE,
                  '2001-2002' = TRUE,
                  '2003-2004' = TRUE,
                  '2005-2006' = TRUE,
                  '2007-2008' = TRUE,
                  '2009-2010' = TRUE,
                  '2011-2012' = TRUE,
                  '2013-2014' = TRUE,
                  FALSE)

  if(throw_error == TRUE && valid == FALSE) {
    stop(paste0("Invalid year: ", year))
  }


  return(valid)
}

download_nhanes_file <- function(file_name, year, destination = tempdir(), overwrite = FALSE) {
  validate_year(year)

  if(!dir.exists(destination)) {
    stop(paste0("Directory doesn't exist: ", destination))
  }

  if(grepl(".XPT", file_name) == FALSE) {
    file_name <- paste0(file_name, ".XPT")
  }

  destination <- file.path(destination, file_name)

  if(overwrite == FALSE) {
    if(file.exists(destination) == TRUE) {
      return(destination)
    }
  }

  url <- paste0("http://wwwn.cdc.gov/Nchs/Nhanes/", year, '/', file_name)

  message(paste0("Downloading ", file_name, " to ", destination));

  download.file(url, destination, method='auto', mode='wb')

  return(destination)
}

merge.data.with.demographics <- function(nhanes.demo, nhanes.lab) {
  # Merge demography and lab results
  nhanes <- merge(nhanes.demo, nhanes.lab, by="SEQN", all=F)

  return(nhanes)
}

#' Download NHANES data files.
#'
#' @param file_name NHANES file name e.g. ("EPH_E")
#' @param year NHANES cycle year (e.g. "2007-2008")
#' @param destination directory to download the files to
#' @param merge_demographics auto merge demographics data into the dataset
#' @param overwrite whether to overwrite the file if it already exists
#' @examples
#' load_nhanes_data("UHG_G", "2011-2012")
#'
#' load_nhanes_data("HDL_E", "2007-2008", destination = "/tmp", overwrite = TRUE) # Download to /tmp directory and overwrite the file if it already exists
#' @export
#' @importFrom foreign read.xport
load_nhanes_data <- function(file_name, year, destination = tempdir(), demographics = FALSE, overwrite = FALSE) {
  validate_year(year)

  full_path <- download_nhanes_file(file_name, year, destination, overwrite = overwrite)
  dat <- read.xport(full_path)

  if(demographics == TRUE) {
    # Make sure we can merge in the data (this isn't possible when you have pooled samples)

    if("SEQN" %in% names(dat) == FALSE) {
      warning(paste0("Demographics data can't be merged with ", file_name, " because it doesn't have a SEQN column. Maybe it has pooled samples?"))
    }

    demography_data <- load_nhanes_demography_data(year, destination = destination, overwrite = overwrite)

    dat <- merge.data.with.demographics(demography_data, dat)
  }

  dat$Cycle = year

  return(dat)
}

#' Download NHANES demography files for a specific cycle.
#'
#' @param year NHANES cycle year (e.g. "2011-2012")
#' @param destination directory to download the file to
#' @param overwrite sets whether to overwrite the file if it already exists
#' @examples
#' load_nhanes_demography_data("2011-2012")
#'
#' @export
#' @importFrom foreign read.xport
load_nhanes_demography_data <- function(year, destination = tempdir(), overwrite = FALSE) {
  validate_year(year)

  full_path <- download_nhanes_file(demography_filename(year), year, destination, overwrite = overwrite)
  dat <- read.xport(full_path)

  dat$Cycle = year

  return(dat)
}
