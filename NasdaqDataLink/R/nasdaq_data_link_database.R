#' Returns the bulk download url
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param database_code Database code on Nasdaq Data Link specified as a string.
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters. Please see \url{https://docs.data.nasdaq.com/docs/parameters-2} for a full list of parameters.
#' @return Returns the download url.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' url = NasdaqDataLink.database.download_url("NSE", download_type="partial")
#' }
#' @export
NasdaqDataLink.database.bulk_download_url <- function(database_code, ...) {
  url <- paste(NasdaqDataLink.base_url(), NasdaqDataLink.database.download_url_path(database_code), sep = "/")

  params <- list()
  if (!is.null(NasdaqDataLink.api_key())) {
    params$api_key <- NasdaqDataLink.api_key()
  }
  if (!is.null(NasdaqDataLink.api_version())) {
    params$api_version <- NasdaqDataLink.api_version()
  }
  params <- c(params, list(...))

  param_names <- names(params)
  if (length(params) > 0) {
    for (i in 1:length(params)) {
      delimiter <- "&"
      if (i == 1) {
        delimiter <- "?"
      }
      query <- paste(param_names[i], params[[i]], sep = "=")
      url <- paste(url, query, sep = delimiter)
    }
  }
  url
}

#' Downloads a zip with all data from a Nasdaq Data Link database
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param database_code Database code on Nasdaq Data Link specified as a string.
#' @param filename Filename (including path) of file to download.
#' @param ... Additional named values that are interpreted as NasdaqDataLink API parameters. Please see \url{https://docs.data.nasdaq.com/docs/parameters-2} for a full list of parameters.
#' @return The filename of the downloaded file.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.database.bulk_download_to_file("NSE", "./NSE.zip")
#' }
#' @export
NasdaqDataLink.database.bulk_download_to_file <- function(database_code, filename, ...) {
  dirname <- dirname(filename)
  nasdaq_data_link.api.download_file(NasdaqDataLink.database.download_url_path(database_code), filename = filename, ...)
}

NasdaqDataLink.database.download_url_path <- function(database_code) {
  paste("databases", database_code, "data", sep = "/")
}
