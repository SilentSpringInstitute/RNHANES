#' Returns the NHANES file suffix for the given year
#'
#' @param year NHANES cycle year (e.g. "2001-2002")
#'
#' @return suffix character (e.g. "B" or "C")
file_suffix <- function(year) {
  if(length(year) > 1) {
    Map(file_suffix, year) %>%
      unlist() %>%
      unname() %>%
      return()
  }
  else {
    suffix = switch(as.character(year),
                    '2001-2002' = 'B',
                    '2003-2004' = 'C',
                    '2005-2006' = 'D',
                    '2007-2008' = 'E',
                    '2009-2010' = 'F',
                    '2011-2012' = 'G',
                    '2013-2014' = 'H',
                    '2015-2016' = 'I',
                    '2017-2018' = 'J',
                    '2019-2020' = 'K_R',
                    '2021-2023' = 'L'
    )

    return(suffix)
  }
}

#' Translates cycle years into the correct demography filename suffix,
#' e.g. '2001-2002' returns 'B'
#'
#' @param year NHANES cycle, e.g. "2001-2002"
#'
#' @return suffix character e.g. "B"
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

    if(year == '2017-2020') {
      return("P_DEMO.XPT")
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
    valid <- grep("^[0-9]{4}-[0-9]{4}$", as.character(year)) == 1

    if(throw_error == TRUE && valid == FALSE) {
      stop(paste0("Invalid year: ", year))
    }

    return(valid)
  }
}

#' Processes a file name to make sure it is valid and has the correct suffix and extension
#' File names with an extension (e.g. ".XPT") are not altered
#'
#' @param file_name name of the file
#' @param year NHANES cycle year
#' @param extension file extension
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
    pre <- substr(file_name, 1, 2)
    if(ext == ".XPT" || ext == ".htm") {
      return(file_name)
    }

    if(year == "1999-2000") {
      message("Cycle 1999-2000 doesn't always follow the normal naming conventions, so skipping the file suffix check.")

      if(ext != extension) {
        return(paste0(file_name, extension))
      }
    }

    if(grepl("_A", file_name)) {
        message("Files with the suffix A outside of the 1999-2000 cycle typically don't follow the normal naming conventions, so skipping the file suffix check.")

      if(ext != extension) {
        return(paste0(file_name, extension))
      }
    }

    if(year == "2017-2020") {
      message("Cycle 2017-2020 appends prefix instead of suffix, so skipping the file suffix check.")
      
      if(ext != extension) {
        if(pre != "P_") {
          file_name = paste0("P_", file_name)
        }
        return(paste0(file_name, extension))
      }
    }
    # Check for a suffix
    valid_suffix <- file_suffix(year)

    # If it already has the right suffix, just tack on the extension
    if( substr(file_name, nchar(file_name) - 1, nchar(file_name)) == paste0("_", valid_suffix)) {
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
#' @param file_name file name
#' @param year NHANES cycle
#' @param destination directory to download the file into
#' @param cache whether to cache the file
#' @param method download method passed to download.file
#'
#' @return path to the downloaded file

download_nhanes_file <- function(file_name, year, destination = tempdir(), cache = TRUE, method = "auto") {
  validate_year(year)

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }

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
    Map(download_nhanes_file, file_name, year,  destination = destination, cache = cache, method = method) %>%
      unlist() %>%
      unname() %>%
      return()
  }

  year_path <- str_split_i(year, "-", 1)
  if(grepl("_A", file_name)) {
    year_path = "1999"
  }

  #dest_file_name <- paste0(year_suffix, "_", file_name)
  destination <- file.path(destination, file_name)

  if(cache == TRUE && file.exists(destination) == TRUE) {
    return(destination)
  }

  url <- paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", year_path, '/DataFiles/', file_name)

  message(paste0("Downloading ", file_name, " to ", destination));

  download.file(url, destination, method=method, mode='wb')

  return(destination)
}

merge.data.with.demographics <- function(nhanes.demo, nhanes.lab) {
  # Merge demography and lab results
  nhanes <- merge(nhanes.demo, nhanes.lab, by=c("SEQN", "cycle"), all=F)

  return(nhanes)
}

#' Download an NHANES description file
#'
#' @param file_name file name
#' @param year NHANES cycle
#' @param destination directory to download the file into
#' @param cache whether to cache the file
#' @param method download method passed to download.file
#'
#' @return data frame containing the file description
load_nhanes_description <- function(file_name, year, destination = tempdir(), cache = FALSE, method = 'auto') {
  validate_year(year)

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }

  file_name <- process_file_name(file_name, year, ".htm")

  full_path <- download_nhanes_file(file_name, year, destination, cache = cache, method = method)

  html <- read_html(full_path)

  # Loop through each variable
  sections <- html %>%
    html_nodes('.pagebreak')

  description <- data.frame(var_name = numeric(0),
                            code = numeric(0),
                            value_description = numeric(0))

  description <- lapply(sections, function(section) {
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
      table <- section %>%
        html_node("table")
      
      if(!is.na(table)){
          values <- html_table(table)
          values$var_name = toupper(var_name) # Ensure variable name is always uppercase
          return(values)
        }
    }
  })

  description <- do.call(rbind, description)

  if(cache == TRUE) {
    write.csv(description, paste0(substr(full_path, 1, nchar(full_path) - 4), "_description.csv"), row.names = FALSE)
    unlink(full_path)
  }

  return(description)
}

recode_nhanes_data <- function(nhanes_data, nhanes_description) {  
  # Return without changes if there is no information in the codebook
  if(is.null(nhanes_description)){
    return(nhanes_data)
  }
  
  for (row in 1:nrow(nhanes_description)){
    val <- nhanes_description[row, ][["Code or Value"]]
    var_name <- nhanes_description[row, ][["var_name"]]
    descr <- nhanes_description[row, ][["Value Description"]]
    if(var_name %in% names(nhanes_data) & !is.na(val)){
      # Don't attempt re-coding unless the variable is listed in the data and the value is not NA
      nhanes_data[!is.na(nhanes_data[var_name]) & (nhanes_data[var_name] == val), var_name] <- descr
    }
  }
  return(nhanes_data)
}

get_cache_file_path <- function(file_name, year, destination, demographics = FALSE, recode = FALSE, recode_data = FALSE, recode_demographics = FALSE) {
  # Build the file name that the data will be stored to
  file_name_base <- process_file_name(file_name, year, extension = "")

  if(demographics == TRUE) {
    file_name_base <- paste0(file_name_base, "_demographics")
  }

  if(recode == TRUE || (recode_data == TRUE && recode_demographics == TRUE)) {
    file_name_base <- paste0(file_name_base, "_recoded")
  }
  else if(recode_data == TRUE) {
    file_name_base <- paste0(file_name_base, "_recoded_data")
  }
  else if(recode_demographics == TRUE) {
    file_name_base <- paste0(file_name_base, "_recoded_demographics")
  }

  cache_file_name <- paste0(file_name_base, ".csv")
  cache_file_path <- file.path(destination, cache_file_name)

  return(cache_file_path)
}

#' Download NHANES data files.
#'
#' @param file_name NHANES file name (e.g. "EPH") or a vector of filenames (e.g c("EPH", "GHB"))
#' @param year NHANES cycle year (e.g. "2007-2008") or a vector of cycle years
#' @param destination directory to download the files to
#' @param demographics include  demographics data into the dataset
#' @param cache whether to cache the file to disk
#' @param recode whether to recode the data and demographics (overrides other parameters)
#' @param recode_data whether to recode just the data
#' @param recode_demographics whether to recode just the demographics
#' @param allow_duplicate_files how to handle a request that has duplicate file names/cycle years. By default duplicates will be removed.
#' @param method download method passed to download.file
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
#'
#' \dontrun{
#'
#' nhanes_load_data("UHG", "2011-2012")
#'
#' # Load data with demographics
#' nhanes_load_data("UHG", "2011-2012", demographics = TRUE)
#'
#' # Download to /tmp directory and overwrite the file if it already exists
#' nhanes_load_data("HDL_E", "2007-2008", destination = "/tmp", cache = FALSE)
#' }
#'
#' @importFrom foreign read.xport
#' @export
nhanes_load_data <- function(file_name, year, destination = tempdir(), demographics = FALSE, cache = TRUE, recode = FALSE, recode_data = FALSE, recode_demographics = FALSE, allow_duplicate_files = FALSE, method = 'auto') {
  validate_year(year)

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }


  # Handle multiple files
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
        recode_demographics = recode_demographics,
        method = method)

    return(res_list)
  }


  cache_file_path <- get_cache_file_path(file_name, year, destination, demographics, recode, recode_data, recode_demographics)

  # First, check to see if there is an exact cache match
  if(cache == TRUE && file.exists(cache_file_path)) {
    dat <- read.csv(cache_file_path, stringsAsFactors = FALSE)
  }
  else {
    # Even if there wasn't an exact cache match, there may be a match
    # for the data file without demographics or recoding.
    base_data_cache_path <- get_cache_file_path(file_name, year, destination)
    if(cache == TRUE && file.exists(base_data_cache_path)) {
      dat <- read.csv(base_data_cache_path, stringsAsFactors = FALSE)
    } else {

      # Download the base data file from NHANES
      full_path <- download_nhanes_file(file_name, year, destination, cache = cache, method = method)
      dat <- read.xport(full_path)

      dat$file_name = gsub("\\.XPT|\\.htm", "", process_file_name(file_name, year))
      dat$cycle = year
      dat$begin_year = as.numeric(substr(year, 1, 4))
      dat$end_year = as.numeric(substr(year, 6, 9))

      # If caching is enabled, save the data as a CSV (smaller file size)
      if(cache == TRUE) {
        write.csv(dat, base_data_cache_path, row.names = TRUE)
        unlink(full_path)
      }
    }

    # If we are recoding the data, we need to download the description file
    if(recode == TRUE || recode_data == TRUE) {
      nhanes_description <- load_nhanes_description(file_name, year, destination = destination, cache = cache)
      dat <- recode_nhanes_data(dat, nhanes_description)
    }

    if(demographics == TRUE) {

      # Make sure we can merge in the data (this isn't possible when you have pooled samples)
      if("SEQN" %in% names(dat) == FALSE) {
        warning(paste0("Demographics data can't be merged with ", file_name, " because it doesn't have a SEQN column. Maybe it has pooled samples?"))
      }

      # Caching is built in to the demography_data function, so we don't have to worry about it
      demography_data <- nhanes_load_demography_data(year, destination = destination, cache = cache, method = method)

      # Handle recoding data
      if(recode == TRUE || recode_demographics == TRUE) {

        # Check to see if there is a cached copy of the recoded demographics
        demography_file <- demography_filename(year)
        demographics_description_csv_name <- file.path(destination, paste0(substr(demography_file, 1, nchar(demography_file) - 4), "_recoded.csv"))

        if(cache == TRUE && file.exists(demographics_description_csv_name)) {
          demography_data <- read.csv(demographics_description_csv_name, stringsAsFactors = FALSE)
        } else {
          # If not, we have to download the description html and parse it
          demographics_description_file_name <- gsub(".XPT", ".htm", demography_filename(year))
          demographics_description <- load_nhanes_description(demographics_description_file_name, year, destination, cache, method = method)
          demography_data <- recode_nhanes_data(demography_data, demographics_description)

          if(cache == TRUE) {
            write.csv(demography_data, demographics_description_csv_name, row.names = FALSE)
          }
        }
      }

      dat <- merge.data.with.demographics(demography_data, dat)
    }

    # If caching enabled, cache the final dataset
    if(cache == TRUE) {
      write.csv(dat, cache_file_path, row.names = FALSE)
    }
  }

  return(dat)
}

#' Download NHANES demography files for a specific cycle.
#'
#' @param year NHANES cycle year (e.g. "2011-2012")
#' @param destination directory to download the file to
#' @param cache whether load the file if it already exists on disk
#' @param method download method passed to download.file
#'
#' @examples
#'
#' \dontrun{
#' nhanes_load_demography_data("2011-2012")
#' }
#'
#' @export
#' @importFrom foreign read.xport
nhanes_load_demography_data <- function(year, destination = tempdir(), cache = FALSE, method = 'auto') {
  validate_year(year)

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }

  file_name <- process_file_name(demography_filename(year), year)
  full_path <- file.path(destination, file_name)
  csv_path <- paste0(substr(full_path, 1, nchar(full_path) - 3), "csv")

  if(cache == TRUE && file.exists(csv_path)) {
    dat <- read.csv(csv_path, stringsAsFactors = FALSE)
  }
  else {
    full_path <- download_nhanes_file(demography_filename(year), year, destination, cache = cache, method = method)
    dat <- read.xport(full_path)

    if(cache == TRUE) {
      message(paste0("Caching CSV to ", csv_path))
      write.csv(dat, csv_path, row.names = FALSE)
      unlink(full_path)
    }
  }

  dat$cycle = year

  return(dat)
}