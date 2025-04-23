utils::globalVariables("where")
#' Read an NJTR-1 data file that was previously downloaded. This function will read a .zip or .txt file in the format published by the NJDOT and return a data frame with cleaned data.
#'
#'
#' @param file path to a file downloaded from NJDOT
#' @param geo Logical, whether to filter only to geotagged cases (default = FALSE)
#' @return tibble of cleaned & parsed NJTR-1 data
#' @importFrom readr parse_number
#' @importFrom tools file_ext
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_trim
#' @importFrom stringi stri_enc_toutf8
#' @importFrom tidyselect vars_select_helpers
#' @examples
#' read_njtr1(system.file("extdata/testdata/Ocean2019Accidents.zip", package = "njtr1"))
#' @export
read_njtr1 <- function(file, geo = FALSE) {

  # Get the filename from the path
  name <- basename(file)

  # Detect which type of data file is being read in from the file name
  if (grepl("Accidents", name)) {
    type <- "Accidents"
  } else if (grepl("Drivers", name)) {
    type <- "Drivers"
  } else if (grepl("Pedestrians", name)) {
    type <- "Pedestrians"
  } else if (grepl("Occupants", name)) {
    type <- "Occupants"
  } else if (grepl("Vehicles", name)) {
    type <- "Vehicles"
  } else {
    stop("Unable to detect file type. Please provide an unmodified file downloaded from NJDOT.")
  }

  # Extract the year from the file path
  year <- readr::parse_number(name)

  # Convert Year to integer
  year <- as.integer(year)

  # Set column names based on year selected to match NJDOT schema
  if (year >= 2017 & year <= 2020) {
    # Set parameters for download using input to function
    fields <- utils::read.csv(paste0(system.file("extdata", package = "njtr1"), "/fields/2017/", type, ".csv"), header = FALSE)
  } else if (year <= 2016 & year >= 2001) {
    fields <- utils::read.csv(paste0(system.file("extdata", package = "njtr1"), "/fields/2001/", type, ".csv"), header = FALSE)
  } else if (year > 2020) {
    stop("Invalid year: No data for years past 2020 is currently available")
  }


  # Handle ZIP files
  if (tools::file_ext(file) == "zip") {

    # create a temporary directory and file for downloading the data
    zip_file <- file
    td <- tempdir()

    # Get name of first file in the ZIP archive
    fname <- utils::unzip(zip_file, list = TRUE)$Name[1]

    # unzip the file to the temporary directory
    utils::unzip(zip_file, files = fname, exdir = td, overwrite = TRUE)

    # Store path of extracted file
    fpath <- file.path(td, fname)

    # Read in the file
    data <- utils::read.csv(fpath, header = FALSE, row.names = NULL, quote = "")
  } else if (tools::file_ext(file) == "txt") {
    data <- utils::read.csv(file, header = FALSE, row.names = NULL, quote = "")
  }


  # Add field names after reading in data
  names(data) <- fields$V1

  # Parse dates
  if (type == "Accidents") {
    suppressWarnings(data$crash_date <- lubridate::mdy(data$crash_date))
    # If geo is TRUE, only return geotagged cases
    if (geo == TRUE) {
      data <- data[complete.cases(data[, 46:47]), ]
    }
  } else if (type == "Pedestrians") {
    suppressWarnings(data$date_of_birth <- lubridate::mdy(data$date_of_birth))
  } else if (type == "Drivers") {
    data$driver_dob <- suppressWarnings(lubridate::mdy(data$driver_dob))
  }
  # Clean any empty columns
  keep.cols <- names(data) %in% c(NA)
  data_clean <- data[!keep.cols]

  # Convert all characters to UTF-8, Remove excess whitespace in every character column
  data_clean <- data_clean %>%
    dplyr::mutate(dplyr::across(where(is.character), stringi::stri_enc_toutf8)) %>%
    dplyr::mutate(dplyr::across(where(is.character), trimws))
    

  return(data_clean)
}

#' Download New Jersey car accident records for a given year. Returns a data frame of car crash based upon the table type and year specified.
#'
#'
#' @param year Year to download crash data for
#' @param type The table of NJDOT crash data to download
#' @param county Specify a specific county to download data for (default = all)
#' @param geo Logical, whether to filter only to geotagged cases (default = FALSE)
#' @return data.frame of all reported accidents for the year
#' @importFrom httr http_error
#' @importFrom lubridate mdy
#' @importFrom stats complete.cases
#' @examples
#' get_njtr1(year = 2019, type = "Pedestrians")
#' @export
get_njtr1 <- function(year, type, county = "all", geo = FALSE) {

  # Validate input for table selection
  tables <- c("Accidents", "Drivers", "Pedestrians", "Occupants", "Vehicles")

  if (type %in% tables == FALSE) {
    stop(paste(
      "Invalid table type entered. Please choose from one of the following:",
      "\n", "Accidents, Drivers, Pedestrians, Occupants, Vehicles"
    ))
  }

  # Convert Year to integer
  year <- as.integer(year)

  # Set base URL for data downloads
  base_url <- "https://www.state.nj.us/transportation/refdata/accident/"

  # Determine URL prefix

  counties <-
    c(
      "all",
      "Atlantic",
      "Bergen",
      "Burlington",
      "Camden",
      "Cape May",
      "Cumberland",
      "Essex",
      "Gloucester",
      "Hudson",
      "Hunterdon",
      "Mercer",
      "Monmouth",
      "Ocean",
      "Passaic",
      "Salem",
      "Somerset",
      "Sussex",
      "Union",
      "Warren",
      "Morris",
      "Middlesex"
    )

  if (county == "all") {
    prefix <- "NewJersey"
  } else if (county %in% counties & county != "all") {
    prefix <- county
  } else {
    stop("Please specify a valid county name to download data.")
  }


  # Set column names based on year selected to match NJDOT schema
  if (year >= 2017 & year <= 2020) {
    # Set parameters for download using input to function
    fields <- utils::read.csv(paste0(system.file("extdata", package = "njtr1"), "/fields/2017/", type, ".csv"), header = FALSE)
    file_name <- paste0(base_url, as.character(year), "/", prefix, year, type, ".zip")
  } else if (year <= 2016 & year >= 2001) {
    fields <- utils::read.csv(paste0(system.file("extdata", package = "njtr1"), "/fields/2001/", type, ".csv"), header = FALSE)
    file_name <- paste0(base_url, as.character(year), "/", prefix, year, type, ".zip")
  } else if (year > 2020) {
    stop("Invalid year: No data for years past 2020 is currently available")
  }

  # create a temporary directory and file for downloading the data
  zip_file <- tempfile(fileext = ".zip")
  td <- tempdir()

  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error(file_name)) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njtr1: downloading data")
    utils::download.file(file_name, zip_file, mode = "wb")
  }

  # Get name of first file in the ZIP archive
  fname <- utils::unzip(zip_file, list = TRUE)$Name[1]

  # unzip the file to the temporary directory
  utils::unzip(zip_file, files = fname, exdir = td, overwrite = TRUE)

  # Store path of extracted file
  fpath <- file.path(td, fname)

  # Read in the file
  data <- read_njtr1(fpath)


  return(data)
}
