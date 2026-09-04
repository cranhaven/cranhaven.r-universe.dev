#' get_dataset
#'
#' Download a predefined set of time series.
#' @inheritParams param_defs
#' @param username Your dataservice user name
#' @param file_to_download The name of the file to retrieve.
#' @param target Path to the location, at which to store the file. If NULL, the file will be
#' saved to the working directory.
#' @examples
#' f <- try(download_cached_file("kofdatapkg", "313984fcd9f343d3961891319b0ed321",
#' "empty.txt",file.path(tempdir(),"empty.txt")))
#' @import httr
#' @export
download_cached_file <- function(username, api_key, file_to_download, target=NULL) {
  url_template <- "https://datenservice.kof.ethz.ch/api/v1/user/%s/datasets/%s"

  query <- list(apikey = api_key)

  download_url <- sprintf(url_template, username, file_to_download)

  if(is.null(target)) {
    target <- basename(file_to_download)
  }

  tryCatch(
    validate_response(GET(download_url, write_disk(target, overwrite=TRUE), query = query)),
    error = function(e) {
      # In case of an error, the API response will have been writen into the target file already
      unlink(target)
      stop(e)
  })
}
