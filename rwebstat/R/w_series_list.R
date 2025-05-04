#' List the available series from a dataset.
#'
#' @section Identification:
#' You should declare your Webstat client ID in a global "webstat_client_ID" variable. Alternatively, you can enter your client ID as a parameter or enter it when prompted.
#'
#' @param dataset_name Mandatory. String (Must be between quotes.) The datasets codes can be determined with the w_datasets() function.
#' @param language Optional. String. Defaults to "fr" (French). The only other available option is "en" (English). Determines the language of the metadata. Your Webstat "App" must be subscribed to the API in this language (or both) or you'll get a 501 http error.
#' @param base_url Optional. String. Defaults to "https://api.webstat.banque-france.fr/webstat-". For internal testing purposes only.
#' @param client_ID Optional. String. If you do not specify it when calling the function, it will check if a global variable called ".GlobalEnv$webstat_client_ID exists and use it. If not, you will be prompted. The easiest way is to save the client ID as a string in ".GlobalEnv$webstat_client_ID".
#'
#' @examples
#' \dontrun{
#' ## Request the list of all series from the BPM6 dataset
#' w_series_list("BPM6")
#'
#' ## Request the list of all series from the CPP dataset, with English metadata
#' w_series_list("CPP", language = "en")
#'
#' ## Your client ID can be entered as a parameter as follows or saved
#' ## in a global variable named "webstat_client_ID" in order to reuse it.
#' w_series_list("CPP", client_ID = "1234abcd-12ab-12ab-12ab-123456abcdef")
#' }
#'
#' @return A data frame listing all the series from the requested dataset with their codes, titles and dimensions.
#'
#' @import dplyr
#' @import httr
#' @import getPass
#' @import readr
#' @import utils
#'
#' @export
w_series_list <- function(dataset_name, language = "fr", client_ID, base_url = "https://api.webstat.banque-france.fr/webstat-") {
  # check and set client_ID
  client_ID <- check_client_id(client_ID)

  # check language and dataset
  if (language != "en" & language != "fr") {
    stop("language must be either 'fr' or 'en'")
  }
  if (missing(dataset_name)) {
    stop("Dataset is missing")
  }

  # Build API URL "w_url" for the request
  w_url <- make_url_series_list(dataset_name, language, base_url = base_url)

  # Call the API
  req_csv <- get_data(w_url, client_ID)

  # Get content from request
  cont_csv <- content(req_csv, as = "text", encoding = "UTF-8")

  # Read in table format  ------
  sep_lg <- set_sep_lg(language)

  series <- read.csv(text = cont_csv, sep = sep_lg, header = F, stringsAsFactors = F)
  # Clean colnames and remove the last NA-filled column
  series[, 1] <- rid_odd_character(char_vect = series[, 1])
  names(series) <- series[1, ]
  series <- series[-1, -dim(series)[2]]
  return(series)
}
