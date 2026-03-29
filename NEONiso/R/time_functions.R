#--------------------------------------------------------------
#' Convert a POSIXct object to the format used in NEON HDF5 files
#'
#' Converts a POSIXct object back to the character format used by NEON in their
#' HDF eddy covariance files. Output format, using strptime syntax, is
#' %Y-%m-%dT%H:%M:%OSZ.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param intime POSIXct vector to convert to NEON time format.
#'
#' @return Returns character version of POSIXct object
#'         matching NEON time variable format.
#'
#' @export
#' @examples
#' convert_POSIXct_to_NEONhdf5_time(Sys.time())

convert_POSIXct_to_NEONhdf5_time <- function(intime) {

  intime <- lubridate::ymd_hms(intime)

  outtime <- format(intime, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")

  return(outtime)
}

#' Convert NEON HDF5 file time to POSIXct
#'
#' Converts the date time string in NEON HDF5 files to a POSIXct object
#' for use in R.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param intime Vector of datetimes in NEON data files (as string)
#'           to convert to POSIXct class
#'
#' @return Vector of datetimes from NEON data file now in POSIXct format.
#'
#' @export
#'
#' @examples
#' convert_NEONhdf5_to_POSIXct_time("2019-06-01T12:00:00.000Z")
convert_NEONhdf5_to_POSIXct_time <- function(intime) {

  outtime <- as.POSIXct(intime,
                        format = "%Y-%m-%dT%H:%M:%OSZ",
                        tz = "GMT",
                        origin = "1970-01-01 00:00:00")

  return(outtime)
}
