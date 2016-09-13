#' List the valid NHANES cycle years
#'
#' @return vector of NHANES cycle years
#'
#'
#' @export
nhanes_cycle_years <- function() {
  return(c("1999-2000",
           "2001-2002",
           "2003-2004",
           "2005-2006",
           "2007-2008",
           "2009-2010",
           "2011-2012",
           "2013-2014",
           "2015-2016"))
}

#' Helper function for nhanes_data_files function
#'
#' @param component one of demographics", "dietary", "examination", "laboratory", "questionnaire"
#' @param destination download destination
#'
#' @import rvest
#' @importFrom xml2 read_html
#'
#' @return dat
parse_data_files_page <- function(component, destination = tempfile()) {
  url <- paste0("http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=", component)
  message(paste0("Downloading NHANES data file list to ", destination));

  download.file(url, destination, method='auto', mode='wb')

  dat <- read_html(destination) %>% html_table()
  dat <- dat[[2]]
  dat$Component = component

  names(dat) <- gsub(" ", "", names(dat))

  unlink(destination)

  return(dat)
}

#' List the NHANES data files
#'
#' @param components one of "all", "demographics", "dietary", "examination", "laboratory", "questionnaire"
#' @param destination destinatino to save the file lists
#' @param cache whether to cache the downloaded file lists so they don't have to be re-downloaded every time
#'
#' @return data frame of NHANES data files available to download
#'
#' @import rvest
#' @importFrom xml2 read_html
#'
#' @examples
#' \dontrun{
#'
#' # Download a data frame of all the NHANES data files
#' files <- nhanes_data_files()
#'
#' # Download a data frame of just the laboratory files
#' lab_files <- nhanes_data_files(component = "laboratory")
#'
#' }
#'
#' @export
nhanes_data_files <- function(components = "all", destination = tempfile(), cache = TRUE) {

  all_components <- c("demographics", "dietary", "examination", "laboratory", "questionnaire")
  components = tolower(components)

  if(components == "all") {
    components = all_components
  } else {
    if(sum(!components %in% all_components) > 0) {
      stop("Invalid component given to nhanes_data_files. Acceptable values are 'demographics', 'dietary', 'examination', 'laboratory', and 'questionnaire'")
    }
  }

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }

  if(dir.exists(destination)) {
    destination <- file.path(destination, "nhanes_data_files.csv")
  } else if(!dir.exists(dirname(destination))) {
    stop(paste0("Directory doesn't exist: ", dirname(destination)))
  }

  if(cache == TRUE && file.exists(destination)) {
    dat <- read.csv(destination, stringsAsFactors = FALSE)

    if(sum(!components %in% dat$component) > 0) {
      stop("The cached file doesn't have all the components you specified in this call to nhanes_data_files. Either delete the file in order to redownload the new components you want, or choose a new destination to download to.")
    }
  } else {

    dat <- lapply(components, parse_data_files_page)
    dat <- Reduce(rbind, dat)

    names(dat) <- c("cycle", "data_file_description", "doc_file", "data_file", "date_published", "component")

    dat$confidential <- dat$data_file == "RDC Only"

    m <- regexec("(.+) Data \\[(.+), ([0-9\\.]+ [A-Z]+)\\]", dat$data_file)
    matches <- regmatches(dat$data_file, m) %>%
      lapply(function(item) if(length(item) == 0) c(NA, NA, NA) else item[2:4])
    matches <- Reduce(rbind, matches) %>% as.data.frame(stringsAsFactors = FALSE)
    names(matches) <- c("data_file_name", "file_type", "data_file_size")
    rownames(matches) <- c()

    dat <- cbind(dat, matches)

    write.csv(dat, destination, row.names = FALSE)
  }

  attr(dat, 'rnhanes') <- 'nhanes_files'
  return(dat)
}

#' Helper function for nhanes_variable function
#'
#' @param component one of "Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire"
#' @param destination download destination
#'
#' @import rvest
#' @importFrom xml2 read_html
#'
#' @return dat
parse_variable_list <- function(component, destination = tempfile()) {
  url <- paste0("http://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=", component)

  download.file(url, destination, method='auto', mode='wb')

  # Parse the table and unpack the data frame
  dat <- read_html(destination, encoding = "UTF-8")
  dat <- html_table(dat)
  dat <- dat[[2]]

  # Rename the columns
  names(dat) <- gsub(" ", "_", names(dat))
  names(dat) <- tolower(names(dat))

  dat$end_year <- dat$endyear
  dat <- dat[, names(dat) != "endyear"]

  # Add a cycle column
  dat$cycle <- paste(dat$begin_year, dat$end_year, sep = "-")

  # Extract units (when available) into a separate column
  # TODO: think about how to fix this, right now it works but it picks up a lot of non-unit things
  units <- regmatches(dat$variable_description, gregexpr("\\((.*?)\\)", dat$variable_description))
  units[sapply(units, length) == 0] <- NA # Replace blanks with NAs
  units[dat$component != 'Laboratory' & dat$component != 'Examination'] <- NA # Replace anything picked up in the non-lab variables as NAs
  units <- Map(function(x) tail(x, 1), units)
  units <- unlist(units)
  units <- Map(function(x) substr(x, 2, nchar(x) - 1), units)

  dat$unit <- unlist(units)

  dat$component = tolower(dat$component)

  return(dat)
}

#' Load the NHANES comprehensive variable list
#'
#' @param components one of "all", "demographics", "dietary", "examination", "laboratory", "questionnaire"
#' @param destination where to save the variable list
#' @param cache whether to cache the downloaded variable list so it doesn't have to be re-downloaded every time
#'
#' Helper function for nhanes_variables function
#'
#' @import rvest
#' @importFrom xml2 read_html
#'
#' @return dat
#'
#' @examples
#' \dontrun{
#'
#' # Download the comprehensive NHANES variable list
#' variables <- nhanes_variables()
#'
#' # Download the variable list and cache it in a specific file
#' variables <- nhanes_variables(destination = "./nhanes_data")
#'
#' }
#'
#' @import rvest
#' @importFrom xml2 read_html
#' @importFrom utils download.file
#' @export
nhanes_variables <- function(components = "all", destination = tempfile(), cache = TRUE) {
  all_components <- c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire")
  components = tolower(components)

  if(components == "all") {
    components = all_components
  } else {
    if(sum(!components %in% all_components) > 0) {
      stop("Invalid component given to nhanes_data_files. Acceptable values are 'demographics', 'dietary', 'examination', 'laboratory', and 'questionnaire'")
    }
  }

  if(!dir.exists(dirname(destination))) {
    stop(paste0("Directory doesn't exist: ", dirname(destination)))
  }

  if(missing(destination)) {
    destination <- getOption("RNHANES_destination", destination)
  }

  if(missing(cache)) {
    cache <- getOption("RNHANES_cache", cache)
  }

  destination_csv <- destination

  if(dir.exists(destination)) {
    destination_csv <- file.path(destination, "nhanes_variables.csv")
  }

  if(cache == TRUE && file.exists(destination_csv)) {
    dat <- read.csv(destination_csv, stringsAsFactors = FALSE)
  } else {
    dat <- lapply(components, parse_variable_list)
    dat <- Reduce(rbind, dat)

    if(cache == TRUE) {
      write.csv(dat, file = destination_csv, row.names = FALSE)
    }
  }
  attr(dat, 'rnhanes') <- 'nhanes_variables'
  return(dat)
}

#' Search the results from nhanes_variables or nhanes_data_files
#'
#' @param nhanes_data nhanes variable list, from nhanes_variables function, or data file list, from nhanes_data_files
#' @param query regular expression search query
#' @param ... additional arguments to pass to dplyr::filter
#' @param fuzzy whether to use fuzzy string matching for search (based on edit distances)
#' @param ignore_case whether search query is case-sensitive
#' @param max_distance parameter for tuning fuzzy string matching, 0-1
#'
#' @return data frame filtered by search query
#'
#' @examples
#'
#' \dontrun{
#' nhanes_files <- nhanes_data_files()
#'
#' # Search for data files about pesticides
#' nhanes_search(nhanes_files, "pesticides")
#' }
#'
#' @importFrom dplyr filter
#' @export
nhanes_search <- function(nhanes_data, query, ..., fuzzy = FALSE, ignore_case = TRUE, max_distance = 0.2) {
  nhanes_attribute <- attr(nhanes_data, 'rnhanes')

  # Workaround
  # Without this, R CMD CHECK will throw a note about how there is no visible binding for
  # global variable because these column names are used in filter with non standard evaluation
  # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  variable_description <- file_name <- data_file_description <- variable_name <- data_file <- NULL

  if(is.null(nhanes_attribute)) {
    stop("nhanes_search only works with data loaded with the RNHANES package")
  } else if(query == "") {
    result <- nhanes_data %>% filter(...)
  } else if(nhanes_attribute == 'nhanes_files') {
    if(fuzzy) {
      result <- nhanes_data %>%
        filter(agrepl(query, data_file_description, ignore.case = ignore_case, max.distance = list(all = max_distance)) | agrepl(query, data_file, ignore.case = ignore_case, max.distance = list(all = max_distance)), ...)
    } else {
      result <- nhanes_data %>%
        filter(grepl(query, data_file_description, ignore.case = ignore_case) | grepl(query, data_file, ignore.case = ignore_case), ...)
    }
  } else if(nhanes_attribute == 'nhanes_variables') {
    if(fuzzy) {
      result <- nhanes_data %>%
        filter(agrepl(query, variable_description, ignore.case = ignore_case, max.distance = list(all = max_distance)) | agrepl(query, variable_name, ignore.case = ignore_case, max.distance = list(all = max_distance)), ...)
    } else {
      result <- nhanes_data %>%
        filter(grepl(query, variable_description, ignore.case = ignore_case) | grepl(query, variable_name, ignore.case = ignore_case), ...)
    }
  }

  return(result)
}
