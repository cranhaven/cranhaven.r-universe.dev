#' Screen a VMS file and submit for QC checks
#'
#' In this web service the user can upload a file to be screen and
#' validated by the VMS database. The service can be called using post
#' and the file will have to be part of the body of the call. The user
#' needs to be authenticated in order to call this service.
#' This file can be later pushed to the database by the same user.
#'
#' @param file file name of the file containing the data to screen
#' @param verbose return verbose information about the POST request
#'
#' @return text message from the screening process
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' filename <- system.file("test_files/vms_test.csv", package = "icesVMS")
#' screen_vms_file(filename)
#' }
#'
#' @importFrom httr upload_file
#' @importFrom httr content
#' @export
screen_vms_file <- function(file, verbose = FALSE) {
  url <- "https://data.ices.dk/vms/webapi/ScreenVMSFile"

  body <-
    list(
      fileToScreen = upload_file(file)
    )

  resp <- vms_post(url, body, verbose = verbose, use_token = TRUE)

  content(resp)
}
