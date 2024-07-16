#' get list of records for a dataset
#'
#' This API allows the user to screen a file using the API.
#'
#' @param fileToUpload the filename of the file to upload
#' @param dataSetVerID The version of the dataset
#' @param emailAddress alternative email address of the user, if NULL, email address from ICES token
#'   is used
#' @param sendEmail it is set to TRUE by default, this will specify if the user will to receive an
#'   email when the session is finished
#' @param errorLimit does not need to be specified, it is 30000 by default
#' @param verbose get verbose output from the POST request, default FALSE
#'
#' @return The ID of the DATSU session
#'
#' @examples
#'
#' \dontrun{
#' filename <- system.file("test_files/vms_test.csv", package = "icesDatsu")
#' uploadDatsuFileFireAndForget(filename, 145)
#' uploadDatsuFileFireAndForget(filename, 145, sendEmail = FALSE)
#' }
#' @export
#' @importFrom httr upload_file content
#' @importFrom jsonlite toJSON
#' @importFrom icesConnect decode_token
uploadDatsuFileFireAndForget <- function(fileToUpload, dataSetVerID, emailAddress = NULL, sendEmail = TRUE, errorLimit = 30000, verbose = FALSE) {

  if (is.null(emailAddress)) {
    emailAddress <- decode_token()$email
  }

  # form content
  body <-
    list(
      fileToUpload = upload_file(fileToUpload)
    )

  # perform request
  resp <-
    datsu_post(
      datsu_api(
        "UploadDATSUFileFireAndForget",
        EmailAddress = emailAddress,
        DataSetVerID = dataSetVerID,
        SendEmail = sendEmail,
        ErrorLimit = errorLimit
      ),
      body = body, retry = TRUE, verbose = verbose,
      use_token = TRUE
    )

  # get results
  content(resp)
}
