#' Search keyword inside dataset catalogue or series catalogue
#'
#' @param dataset_name Optional. String (must be entered between quotes.) The datasets codes can be determined using the w_datasets() function.
#' @param keyword Optional. String or regexp you want to search
#' @param language Optional. String. Defaults to "fr" (French). The only other available option is "en" (English). Determines the language of the metadata. Your Webstat "App" must be subscribed to the API in this language (or both languages) or you'll get a 501 http error.
#' @param client_ID Optional. String. If you do not specify it when calling the function, it will check if a global variable called "webstat_client_ID" exists and use it. If not, you will be prompted. The easiest way is to save the client ID as a string in a "webstat_client_ID" global variable.
#' @param base_url Optional. String. Defaults to "https://api.webstat.banque-france.fr/webstat-". For internal testing purposes only.
#' @param fixed Optional. Boolean. Allow or not regexp expressions
#' @param ignore.case Optional. Boolean. Break case sensitivity
#' @param ... Arguments to be passed to grepl function ("fixed = TRUE" if you don't want to use regexp)
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{
#' # find Danish exchange rate code series (fr & en) :
#' w_search("EXR", keyword = "danoise", fixed = TRUE)
#' w_search("EXR", keyword = "Danish", fixed = TRUE, language = "en")
#'
#' # find datasets with keyword :
#' w_search(keyword = "Emploi", fixed = TRUE)
#' w_search(keyword = "Interest Rates", language = "en", fixed = TRUE)
#'
#' # regexp can also be used - find series starting with "Monetary"
#' w_search(keyword = "^Monetary", language = "en")
#' }
#' @export
w_search <- function(dataset_name,
                     keyword = "",
                     language = "fr",
                     client_ID,
                     base_url = "https://api.webstat.banque-france.fr/webstat-",
                     ignore.case = TRUE,
                     fixed = FALSE,
                     ...) {
  # check and set client_ID
  client_ID <- check_client_id(client_ID)

  # if dataset_name is missing, we look into the dataset list
  if (missing(dataset_name)) {
    df_to_search <- w_datasets(language = language, base_url = base_url, client_ID = client_ID)
  } else {
    df_to_search <- w_series_list(dataset_name, language = language, base_url = base_url, client_ID = client_ID)
  }

  # apply grepl
  result <- apply(df_to_search, FUN = grepl, MARGIN = 2, pattern = keyword, ignore.case = ignore.case, fixed = fixed, ...)
  index <- which(apply(result, FUN = sum, MARGIN = 1) > 0)
  result <- df_to_search[index, ]

  return(result)
}
