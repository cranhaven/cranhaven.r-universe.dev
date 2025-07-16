#' Metadata query function for [lsnstat_macrodata] requests.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#' @importFrom dplyr arrange_at
#' @importFrom dplyr filter
#' @importFrom dplyr matches
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @param dataset dataset requested : list available through [get_lsn_dataset_list] (required)
#' @param param list of dataset 'params' available for a dataset (optional)
#'
#' @examples
#'
#' # GET 'figaro_main_aggregates' (Sectoral production accounts from FIGARO)
#' # table parameters and codes.
#'
#' lsnstat_metadata(dataset = "figaro_main_aggregates")
#'
#' # GET 'figaro_intermediate_inputs' (flattened intermediate flows matrix from FIGARO)
#' # filters for parameter 'use_country'.
#'
#' lsnstat_metadata("figaro_intermediate_inputs",param = "use_country")
#'
#' @return A [data.frame()].
#'
#' @export


lsnstat_metadata = function (dataset,param)
{
  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  entrypoint = "https://api.stats.lasocietenouvelle.org"

  endpoint = paste0(get_endpoint(dataset),"/meta")

  tryCatch({

    raw_data = GET(paste0(entrypoint,endpoint))
    res = fromJSON(rawToChar(raw_data$content))

    # RESPONSE NOT OK
    if (res$header$code!=200) {
      stop(res$header$message)
    }

    # OK
    else {
      formatted_data = res$data
      if(!missing(param)) formatted_data = formatted_data %>% filter(param == !!param)
    }

    # API ERROR
  }, error = function(e) {
    print(e)
    stop(e)
  })

  return(formatted_data)
}

utils::globalVariables("endpoint")




