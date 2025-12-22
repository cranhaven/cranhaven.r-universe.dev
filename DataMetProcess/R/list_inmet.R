#' @title
#' List of data available at INMET by year
#'
#' @description
#' Collects the available files for the year and returns a list containing: 1) a table containing the addresses of each file inside the zip for later extraction by the down_inmet() function, 2) Yearther structured table with the information available in the file name (e,g, city, station code, year, date of start and end date) and 3) the address of the zip file.
#'
#' @param year year for download in the INMET database
#' @param filename string containing the path and name of the file with the extension ".zip", if NULL (default) it will be saved in a temporary file
#'
#' @return
#' List containing: 1) a table containing the addresses of each file inside the zip for later extraction by the unzip() function of the utils package, 2) Yearther structured table with the information available in the file name (e,g, city, station code, year, date of start and end date) and 3) the address of the zip file.
#'
#'
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import utils
#'
#' @examples
#'
#' file.down <- tempfile()
#' file.save <- tempfile()
#'
#' info.inmet <-
#'   DataMetProcess::list_inmet(year="2000",file.down)
#'
#' unzip.file <-
#'   utils::unzip(
#'     zipfile = file.down, #or info.inmet$Saved
#'     exdir = file.save
#'   )
#'
#' unzip.file
#'
#'

list_inmet <- function(
    year=NULL,
    filename = NULL
){
  # Initialize variables
  Name <- Origin <- End.Date <- Start.Date <- delete <- City <- NULL

  # If a filename is provided, use it
  if(!base::is.null(filename)){
    temp <- filename
  }

  # Check if temp exists; if not, create a temporary file
  if(!base::exists("temp")){
    temp <- base::tempfile()
  }

  # Download the zip file containing the data for the specified year
  tryCatch({
    utils::download.file(
      url = base::paste0(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
        year,
        ".zip"
      ),
      destfile = temp,
      method = "auto",
      cacheOK = FALSE
    )
  }, error = function(e){
    stop(
      paste0(
        "Error downloading data for year ", year, ".\n",
        "Possible cause: download timeout exceeded.\n",
        "Suggested solution: increase the maximum download time, e.g.:\n",
        "options(timeout = 1000)\n\n",
        "Original error message: ", e$message
      ),
      call. = FALSE
    )
  })

  # List the contents of the zip file
  df <- utils::unzip(zipfile = temp, list = TRUE)[1]

  # Separate the first column into "Year" and "Origin" based on "/"
  suppressWarnings(
    df2 <- tidyr::separate(df, Name,
                           c("Year", "Origin"),
                           sep = c("/")
    )
  )

  # If "Origin" column contains NA values after the first few rows, set "Year" to year
  if(base::all(base::is.na(df2$Origin[4:base::nrow(df2)]))){
    df2$Origin <- df2$Year
    df2$Year <- year
  }

  # Separate "Origin" column into multiple columns based on "_"
  suppressWarnings(
    df2 <- tidyr::separate(
      df2,
      Origin,
      c("Origin", "Region", "State", "Id", "City", "Start.Date", "delete", "End.Date"),
      sep = c("_")
    )
  )

  # Remove the first row of the data frame
  df2 <- dplyr::filter(df2, !dplyr::row_number() == 1)

  # Convert "Start.Date" and "End.Date" columns to Date type and remove ".CSV" from "End.Date"
  df2 <- dplyr::mutate(df2,
                       End.Date = base::as.Date(base::sub(".CSV", "", End.Date), "%d-%m-%Y"),
                       Start.Date = base::as.Date(Start.Date, "%d-%m-%Y"))

  # Remove the "delete" column from the data frame
  df2 <- dplyr::select(df2, -delete)

  # Arrange the data frame by "City"
  df2 <- dplyr::arrange(df2, City)

  # Return a list containing the addresses, details, and the path to the saved file
  return(base::list(Adresses = df, Details = df2, Saved = temp))
}
