#' Translates cycle years into the correct demography filename suffix,
#' e.g. '2001-2002' returns 'B'
#'
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

#' Check that the year is in the correct format
#' e.g. '2001-2002' is correct and returns TRUE,
#' '2001' is not correct and returns FALSE
#'
#'
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

#' Download an NHANES data file from a given cycle
#'
#'
download_nhanes_file <- function(file_name, year, destination = tempdir(), overwrite = FALSE) {
  validate_year(year)

  if(!dir.exists(destination)) {
    stop(paste0("Directory doesn't exist: ", destination))
  }

  # If no extension is specified, the default is to assume it is an XPT file
  if(grepl(".XPT", file_name) == FALSE && grepl(".htm", file_name) == FALSE) {
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

load_nhanes_description <- function(file_name, year, destination = tempdir(), cache = FALSE) {
  validate_year(year)

  if(grepl('.htm', file_name) == FALSE) {
    file_name <- paste0(file_name, '.htm')
  }

  full_path <- download_nhanes_file(file_name, year, destination, overwrite = cache)

  html <- read_html(full_path)

  description <- list()

  # Loop through each variable
  sections <- html %>%
    html_nodes('.pagebreak')

  for(section in sections) {
    var_name <- section %>%
      html_node("h3") %>%
      html_attr('id')

    if(is.na(var_name)) {
      var_name <- section %>%
        html_node("h3") %>%
        html_node("a") %>%
        html_attr('name')
    }

    if(var_name != "SEQN") {
      values <- section %>%
        html_node("table") %>%
        html_table()

      description[[var_name]] <- list(name = var_name, values = values)

    }
    else {
      description[[var_name]] <- list(name = var_name)
    }

  }

  return(description)
}

recode_nhanes_data <- function(nhanes_data, nhanes_description) {
  vars <- colnames(nhanes_data)

  # Figure out which columns in the nhanes data have a match in the description
  cols <- which(names(nhanes_data) %in% names(nhanes_description))

  # Skip the "SEQN" variable
  cols <- cols[names(nhanes_data)[cols] != "SEQN"]

  for(col in cols) {
    var <- vars[col]

    for(row in seq_along(nhanes_data[, col])) {
      val <- nhanes_data[row, col]

      if(is.na(val)) {
        next
      }

      values_table <- nhanes_description[[var]]$values

      replacement_value <- values_table[values_table[,"Code or Value"] == val, "Value Description"]

      if(length(replacement_value) > 0) {
        nhanes_data[row, col] <- replacement_value
      }
    }
  }
  return(nhanes_data)
}

#' Download NHANES data files.
#'
#' @param file_name NHANES file name e.g. ("EPH_E")
#' @param year NHANES cycle year (e.g. "2007-2008")
#' @param destination directory to download the files to
#' @param merge_demographics auto merge demographics data into the dataset
#' @param overwrite whether to overwrite the file if it already exists
#' @param recode whether to recode the data and demographics (overrides other parameters)
#' @param recode_data whether to recode just the data
#' @param recode_demographics whether to recode just the demographics
#'
#' @examples
#' load_nhanes_data("UHG_G", "2011-2012")
#'
#' load_nhanes_data("HDL_E", "2007-2008", destination = "/tmp", overwrite = TRUE) # Download to /tmp directory and overwrite the file if it already exists
#' @export
#' @importFrom foreign read.xport
load_nhanes_data <- function(file_name, year, destination = tempdir(), demographics = FALSE, overwrite = FALSE, recode = FALSE, recode_data = FALSE, recode_demographics = FALSE) {
  validate_year(year)

  full_path <- download_nhanes_file(file_name, year, destination, overwrite = overwrite)
  dat <- read.xport(full_path)

  dat$Cycle = year

  if(recode == TRUE || recode_data == TRUE) {
    nhanes_description <- load_nhanes_description(file_name, year)
    dat <- recode_nhanes_data(dat, nhanes_description)
  }

  if(demographics == TRUE) {
    # Make sure we can merge in the data (this isn't possible when you have pooled samples)

    if("SEQN" %in% names(dat) == FALSE) {
      warning(paste0("Demographics data can't be merged with ", file_name, " because it doesn't have a SEQN column. Maybe it has pooled samples?"))
    }

    demography_data <- load_nhanes_demography_data(year, destination = destination, overwrite = overwrite)

    if(recode == TRUE || recode_demographics == TRUE) {
      demographics_description_file_name <- gsub(".XPT", ".htm", demography_filename(year))
      demographics_description <- load_nhanes_description(demographics_description_file_name, year)

      demography_data <- recode_nhanes_data(demography_data, demographics_description)
    }

    dat <- merge.data.with.demographics(demography_data, dat)
  }

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
