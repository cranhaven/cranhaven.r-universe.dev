#' reads a csv from a url with a csv
#'
#' @param file_url A URL to a CSV file.
#' @return A data frame with the first few rows of the CSV file.
#' @importFrom magrittr %>%
#' @importFrom utils read.csv

read_csv <- function(file_url){
  tryCatch(read.csv(url(file_url)),error = function(e) data.frame()) %>%
    suppressWarnings()
}

create_season <- function(years) {
  # Convert input to a vector if it isn't already
  years <- as.numeric(years)

  # Create the season strings
  seasons <- paste(years, substr(years + 1, 3, 4), sep = "-")

  return(seasons)
}


