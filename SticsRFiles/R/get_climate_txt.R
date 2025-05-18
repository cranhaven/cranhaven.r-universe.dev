#' Read STICS input meteorology file
#'
#' @description Read the meteorology input for STICS ("climat.txt")
#'
#' @param workspace Path of the workspace containing the STICS
#' climate file to read
#' @param file_name The meteorology file name (default to \code{climat.txt}).
#' @param preserve Logical, `TRUE`` for keeping the STICS columns related
#' to date calculation (year, month, day, julian),
#' or only the Date column as a `POSIXct` otherwise. Default to `TRUE`.
#'
#' @param dirpath `r lifecycle::badge("deprecated")` `dirpath` is no
#'   longer supported, use `workspace` instead.
#' @param filename `r lifecycle::badge("deprecated")` `filename` is no
#'   longer supported, use `file_name` instead.
#'
#'
#' @note The time-related variables are summarised into one POSIXct column named
#'       `date`.
#'
#' @return A data.frame of the input meteorological variables used as input
#'         for the STICS model.
#'
#'
#' @examples
#' path <- get_examples_path(file_type = "txt")
#' Meteo <- get_climate_txt(path)
#'
#' @export
#'
get_climate_txt <- function(workspace,
                            file_name = "climat.txt",
                            preserve = TRUE,
                            dirpath = lifecycle::deprecated(),
                            filename = lifecycle::deprecated()) {

  # Managing deprecated arguments
  # dirpath
  if (lifecycle::is_present(dirpath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_climate_txt(dirpath)",
      "get_climate_txt(workspace)"
    )
  } else {
    dirpath <- workspace # to remove when we update inside the function
  }
  # filename
  if (lifecycle::is_present(filename)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_climate_txt(filename)",
      "get_climate_txt(file_name)"
    )
  } else {
    filename <- file_name # to remove when we update inside the function
  }



  file_path <- file.path(dirpath, filename)

  # Checking file
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return()
  }

  meteo_data <- data.table::fread(file_path, data.table = FALSE)
  colnames(meteo_data) <- c(
    "station", "year", "month", "day", "julian", "ttmin", "ttmax",
    "ttrg", "ttetp", "ttrr", "ttvent", "ttpm", "ttco2"
  )
  date <- data.frame(Date = as.POSIXct(
    x = paste(meteo_data$year, meteo_data$month, meteo_data$day, sep = "-"),
    format = "%Y-%m-%d", tz = "UTC"
  ))

  # For removing original date components columns
  if (!preserve) {
    col_idx <- grep("year|month|day|julian", colnames(meteo_data))
    if (length(col_idx)) meteo_data <- meteo_data[, -col_idx]
  }

  # Adding date to data.frame
  meteo_data <- cbind(date, meteo_data)

  # Adding file path as attribute
  attr(meteo_data, "file") <- file_path

  return(meteo_data)
}
