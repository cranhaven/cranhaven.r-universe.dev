#' List all the available datasets from Webstat (codes and names) in a table. No arguments.
#'
#' @section Identification:
#' You should declare your Webstat client ID in a global "webstat_client_ID" variable. Alternatively, you can enter your client ID as a parameter or enter it when prompted.
#'
#' @param language Optional. String. Defaults to "fr" (French). The only other available option is "en" (English). Determines the language of the metadata. Your Webstat "App" must be subscribed to the API in this language or you'll get a 501 http error.
#' @param base_url Optional. String. Defaults to "https://api.webstat.banque-france.fr/webstat-". For internal testing purposes only.
#' @param client_ID Optional. String. If you do not specify it when calling the function, it will check if a global variable called ".GlobalEnv$webstat_client_ID exists and use it. If not, you will be prompted. The easiest way is to save the client ID as a string in ".GlobalEnv$webstat_client_ID".
#'
#' @examples
#' \dontrun{
#' ## Request the dataset catalogue
#' w_datasets()
#'
#' ## Request the dataset catalogue, in English
#' w_datasets(language = "en")
#'
#' ## Your client ID can be entered as a parameter as follows or saved
#' ## in a global variable named "webstat_client_ID" in order to reuse it.
#' w_datasets(client_ID = "1234abcd-12ab-12ab-12ab-123456abcdef")
#' }
#'
#' @return A data frame containing the dataset codes and datasets names
#'
#' @import dplyr
#' @import httr
#' @import getPass
#' @import readr
#' @import utils
#'
#' @export

w_datasets <- function(language = "fr", base_url = "https://api.webstat.banque-france.fr/webstat-", client_ID) {
  # check and set client_ID
  client_ID <- check_client_id(client_ID)

  # check language
  if (language != "en" & language != "fr") {
    stop("language must be either 'fr' or 'en'")
  }

  # Build API URL "w_url" for the request ------
  w_url <- make_url_dataset_list(language, base_url = base_url)

  # Call the API ------
  req_csv <- get_data(w_url, client_ID)

  # Get content from request
  cont_csv <- content(req_csv, as = "text", encoding = "UTF-8")

  # Read in table format  ------
  sep_lg <- set_sep_lg(language)

  datasets <- read.csv(text = cont_csv, sep = sep_lg, header = F, stringsAsFactors = F, colClasses = c(NA, NA, NA, "NULL")) # colClasses deletes the useless third column filled with "NA"

  # Clean colnames
  datasets <- datasets[-1, ]
  names(datasets) <- c("Name", "Description", "Last_Update")
  datasets$Last_Update <- substr(datasets$Last_Update, 1, nchar(datasets$Last_Update) - 2)

  return(datasets)
}
