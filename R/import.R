file_suffix <- function(year) {
  if(length(year) > 1) {
    Map(file_suffix, year) %>%
      unlist() %>%
      unname() %>%
      return()
  }
  else {
    suffix = switch(year,
                    '2001-2002' = 'B',
                    '2003-2004' = 'C',
                    '2005-2006' = 'D',
                    '2007-2008' = 'E',
                    '2009-2010' = 'F',
                    '2011-2012' = 'G',
                    '2013-2014' = 'H'
    )

    return(suffix)
  }
}

#' Translates cycle years into the correct demography filename suffix,
#' e.g. '2001-2002' returns 'B'
#'
demography_filename <- function(year) {
  validate_year(year)

  if(length(year) > 1) {
    Map(demography_filename, year) %>%
      unlist() %>%
      unname() %>%
      return()
  }
  else {
    if(year == '1999-2000') {
      return("DEMO.XPT")
    }

    suffix = file_suffix(year)

    return(paste0("DEMO_", suffix, ".XPT"))
  }
}

#' Check that the year is in the correct format
#' e.g. '2001-2002' is correct and returns TRUE,
#' '2001' is not correct and returns FALSE
#'
#' @param year the year or years to validate
#' @param throw_error whether to throw an error if the year is invalid
#'
validate_year <- function(year, throw_error = TRUE) {
  if(length(year) > 1) {
    Map(validate_year, year, throw_error = throw_error) %>%
      unlist() %>%
      unname() %>%
      return()
  }
  else {
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
}

#' Processes a file name to make sure it is valid and has the correct suffix and extension
#' File names with an extension (e.g. ".XPT") are not altered
#'
process_file_name <- function(file_name, year, extension = ".XPT") {
  validate_year(year)

  if(length(file_name) > 1 || length(year) > 1) {
    # TODO: should map also pass extension?
    Map(process_file_name, file_name, year) %>%
      unlist() %>%
      unname() %>%
      return()
  }
  else {
    ext <- substr(file_name, nchar(file_name) - 3, nchar(file_name))
    if(ext == ".XPT" || ext == ".htm") {
      return(file_name)
    }

    if(year == "1999-2000") {
      message("Cycle 1999-2000 doesn't always follow the normal naming conventions, so skipping the file suffix check.")

      if(ext != extension) {
        file_name <- paste0(file_name, extension)
      }

      return(file_name)
    }

    # Check for a suffix
    valid_suffixes <- c("B", "C", "D", "E", "F", "G", "H")
    valid_suffix <- file_suffix(year)

    # If it already has the right suffix, just tack on the extension
    if( substr(file_name, nchar(file_name), nchar(file_name)) == valid_suffix) {
      file_name = paste0(file_name, extension)
    }

    # If it has a suffix that is incorrect throw a warning
    else if(substr(file_name, nchar(file_name) - 1, nchar(file_name) - 1) == "_") {
      suffix <- substring(file_name, nchar(file_name), nchar(file_name))
      warning(paste0("The file name ", file_name, " is probably incorrect -- check your '_", suffix, "' suffix, the right one is '_", valid_suffix, "' for the ", year, " cycle"))

      # Add the extension anyway
      file_name <- paste0(file_name, extension)
    }

    # Otherwise, add the correct suffix for the provided year
    # Edge case if they have an underscore on the end but no suffix
    else if(substr(file_name, nchar(file_name), nchar(file_name)) == "_") {
      file_name = paste0(file_name, file_suffix(year), extension)
    }

    else {
      file_name = paste0(file_name, "_", file_suffix(year), extension)
    }

    return(file_name)
  }
}

#' Download an NHANES data file from a given cycle
#'
#'
download_nhanes_file <- function(file_name, year, destination = tempdir(), cache = TRUE) {
  validate_year(year)

  if(!dir.exists(destination)) {
    stop(paste0("Directory doesn't exist: ", destination))
  }

  # Process the file name to add the correct suffix if necessary
  file_name <- process_file_name(file_name, year)

  #
  # If one file name is given and multiple years, try to download the same
  # file name for each year.
  #
  # If more than one file name is given and only one year, download the files
  # in that year.
  #
  if(length(file_name) > 1 || length(year) > 1) {
    Map(download_nhanes_file, file_name, year,  destination = destination, cache = cache) %>%
      unlist() %>%
      unname() %>%
      return()
  }

  year_suffix <- gsub("-", "_", year)
  dest_file_name <- paste0(year_suffix, "_", file_name)
  destination <- file.path(destination, file_name)

  if(cache == TRUE && file.exists(destination) == TRUE) {
    return(destination)
  }

  url <- paste0("http://wwwn.cdc.gov/Nchs/Nhanes/", year, '/', file_name)

  message(paste0("Downloading ", file_name, " to ", destination));

  download.file(url, destination, method='auto', mode='wb')

  return(destination)
}

merge.data.with.demographics <- function(nhanes.demo, nhanes.lab) {
  # Merge demography and lab results
  nhanes <- merge(nhanes.demo, nhanes.lab, by=c("SEQN", "cycle"), all=F)

  return(nhanes)
}

load_nhanes_description <- function(file_name, year, destination = tempdir(), cache = FALSE) {
  validate_year(year)

  file_name <- process_file_name(file_name, year, ".htm")

  full_path <- download_nhanes_file(file_name, year, destination, cache = cache)

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
#' @param file_name NHANES file name (e.g. "EPH") or a vector of filenames (e.g c("EPH", "GHB"))
#' @param year NHANES cycle year (e.g. "2007-2008") or a vector of cycle years
#' @param destination directory to download the files to
#' @param merge_demographics auto merge demographics data into the dataset
#' @param cache whether to cache the file to disk
#' @param recode whether to recode the data and demographics (overrides other parameters)
#' @param recode_data whether to recode just the data
#' @param recode_demographics whether to recode just the demographics
#' @param allow_duplicate_files how to handle a request that has duplicate file names/cycle years. By default duplicates will be removed.
#'
#' @return if file_name or year is a vector, returns a list containing a data frame for each file_name. If file_name and year are both singletons, then a data frame is returned.
#'
#' @details If you supply vectors for both file_name and year, then the vectors are paired and each file_name/year pair is downloaded.
#' For example, file_name = c("EPH, GHB"), year = c("2009-2010", "2011-2012") will download "EPH_F.XPT" and "EPH_G.XPT".
#' In other words, the function does not download every possible combination of file_name and year.
#'
#' You can specify file names in several formats. In order of specificity:
#' You can supply the complete filename: "EPH_F.XPT"
#' You can supply the filename without an extension: "EPH_F"
#' You can supply the filename without a suffix: "EPH", year = "2009-2010"
#'
#' If you are loading the same file across multiple years, you must supply the filename without a suffix so
#' that the correct suffix for each year can be used.
#'
#' This function returns either a list or a data frame. If you load multiple files, the return value will always be a list. This is because
#' the columns may not match in between files.
#' If you load one file, the result will be a data frame.
#'
#' @examples
#' nhanes_load_data("UHG", "2011-2012")
#'
#' nhanes_load_data("HDL_E", "2007-2008", destination = "/tmp", cache = FALSE) # Download to /tmp directory and overwrite the file if it already exists
#' @export
#' @importFrom foreign read.xport
nhanes_load_data <- function(file_name, year, destination = tempdir(), demographics = FALSE, cache = TRUE, recode = FALSE, recode_data = FALSE, recode_demographics = FALSE, allow_duplicate_files = FALSE) {
  validate_year(year)

  if(length(file_name) > 1 || length(year) > 1) {
    if(is.factor(file_name)) {
      stop("file_name is a factor -- convert it to a character vector before using nhanes_load_data.")
    }

    if(is.factor(year)) {
      stop("year is a factor -- convert it to a character vector before using nhanes_load_data.")
    }

    file_years <- as.data.frame(cbind(file_name, year), stringsAsFactors = FALSE)
    if(nrow(file_years) > nrow(file_years[!duplicated(file_years),])) {
      if(allow_duplicate_files == TRUE) {
        warning("The file names and cycle years provided have duplicates -- this will download the same file multiple times.")
      }
      else {
        file_years <- file_years[!duplicated(file_years),]
        file_name = file_years[,'file_name']
        year = file_years[,'year']
      }
    }

    res_list <- Map(nhanes_load_data,
        file_name,
        year,
        destination = destination,
        demographics = demographics,
        cache = cache,
        recode = recode,
        recode_data = recode_data,
        recode_demographics = recode_demographics)

    return(res_list)
  }

  full_path <- download_nhanes_file(file_name, year, destination, cache = cache)
  dat <- read.xport(full_path)

  dat$file_name = file_name
  dat$cycle = year
  dat$begin_year = as.numeric(substr(year, 1, 4))
  dat$end_year = as.numeric(substr(year, 6, 9))

  if(recode == TRUE || recode_data == TRUE) {
    nhanes_description <- load_nhanes_description(file_name, year)
    dat <- recode_nhanes_data(dat, nhanes_description)
  }

  if(demographics == TRUE) {
    # Make sure we can merge in the data (this isn't possible when you have pooled samples)

    if("SEQN" %in% names(dat) == FALSE) {
      warning(paste0("Demographics data can't be merged with ", file_name, " because it doesn't have a SEQN column. Maybe it has pooled samples?"))
    }

    demography_data <- nhanes_load_demography_data(year, destination = destination, cache = cache)

    if(recode == TRUE || recode_demographics == TRUE) {
      demographics_description_csv_name <- file.path(destination, gsub(".XPT", "_recoded.csv", demography_filename(year)))

      if(cache == TRUE && file.exists(demographics_description_csv_name)) {
        demography_data <- read.csv(demographics_description_csv_name, stringsAsFactors = FALSE)
      } else {
        demographics_description_file_name <- gsub(".XPT", ".htm", demography_filename(year))
        demographics_description <- load_nhanes_description(demographics_description_file_name, year, destination, cache)
        demography_data <- recode_nhanes_data(demography_data, demographics_description)

        if(cache == TRUE) {
          write.csv(demography_data, demographics_description_csv_name, row.names = FALSE)
        }
      }
    }

    dat <- merge.data.with.demographics(demography_data, dat)
  }

  return(dat)
}

#' Download NHANES demography files for a specific cycle.
#'
#' @param year NHANES cycle year (e.g. "2011-2012")
#' @param destination directory to download the file to
#' @param cache whether load the file if it already exists on disk
#' @examples
#' nhanes_load_demography_data("2011-2012")
#'
#' @export
#' @importFrom foreign read.xport
nhanes_load_demography_data <- function(year, destination = tempdir(), cache = FALSE) {
  validate_year(year)

  full_path <- download_nhanes_file(demography_filename(year), year, destination, cache = cache)
  dat <- read.xport(full_path)

  dat$cycle = year

  return(dat)
}
