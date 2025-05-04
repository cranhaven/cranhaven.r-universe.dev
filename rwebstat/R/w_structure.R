#' Get the structure of a dataset
#'
#' @param dataset_name Mandatory. String (must be entered between quotes.) The datasets codes can be determined using the w_datasets() function.
#' @param language Optional. String. Defaults to "fr" (French). The only other available option is "en" (English). Determines the language of the metadata. Your Webstat "App" must be subscribed to the API in this language or you'll get a 501 http error.
#' @param option Optional. 'light' or 'full'
#' @param client_ID Optional. String. If you do not specify it when calling the function, it will check if a global variable called "webstat_client_ID" exists and use it. If not, you will be prompted. The easiest way is to save the client ID as a string in a "webstat_client_ID" global variable.
#' @param base_url Optional. String. Defaults to "https://api.webstat.banque-france.fr/webstat-". For internal testing purposes only.
#'
#' @return a list of dataset structure
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#'
#' @examples
#' \dontrun{
#' w_structure("EXR")
#' }
#'
#' @export
w_structure <- function(dataset_name, language = "fr", option = "light", client_ID,
                        base_url = "https://api.webstat.banque-france.fr/webstat-") {
  # check and set client_ID
  client_ID <- check_client_id(client_ID)

  # Get content from request
  w_url <- make_url_structure(dataset_name, language = language, option = option, base_url = base_url)
  req_csv <- get_data(w_url, client_ID)
  cont_csv <- content(req_csv, as = "text", encoding = "UTF-8")
  cont <- fromJSON(cont_csv)
  return(cont)
}
