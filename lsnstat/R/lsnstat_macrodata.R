#' R companion of 'La Societe Nouvelle' stats API services
#'
#' @param dataset dataset requested : list available at <https://docs.lasocietenouvelle.org/series-donnees> (required)
#' @param filter_list filters to apply : R list of dataset 'params' available through function [lsnstat_metadata] or [get_lsn_dataset_list] (optional)
#' @param filters filters to apply : formatted list of dataset 'params' available through function [lsnstat_metadata] or [get_lsn_dataset_list] (optional)
#' @param sort Sort the dataset by one or more 'params' available(s) through function [lsnstat_metadata] or [get_lsn_dataset_list] (optional).
#' @param verbose display or hide supplementary informations.
#'
#' @examples
#'
#' #GET macro footprint : Greenhouse gases emissions intensities
#' # for sector "C19" between 2015 and 2021
#'
#' lsnstat_macrodata(dataset = "macro_fpt",
#'                   filter_list = list(indic = 'GHG',year = 2015:2021,industry = 'C19'))
#'
#' #GET macro footprint trend : Energy uses intensities
#' # for sector "C28" between 2025 and 2030
#'
#' lsnstat_macrodata(dataset = "macro_fpt_trd",
#'                   filters = "indic=NRG&year=2025+2026+2027+2028+2029+2030&industry=C28")
#'
#' # GET sectoral value-added, production and intermediate consumption from FIGARO
#' # for sector 'O84' in 2010 and 2030
#'
#' lsnstat_macrodata("figaro_main_aggregates",list(aggregate = c('X','VA','P2'),
#'                                                 industry = 'O84',
#'                                                 year = c("2010","2030")),
#'                                                 sort = c('country','value'))
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_macrodata = function (dataset,filter_list,filters,sort,verbose = T)
{
  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  if(missing(filter_list) & missing(filters)) filter = ""

  if(missing(filter_list) & !missing(filters)) filter = paste0("?",filters)

  if(!missing(filter_list) & missing(filters)) filter = from_filter_list_to_sql(filter_list)

  ####Stat data -> switch to api.stats
  entrypoint = "https://api.stats.lasocietenouvelle.org"

  endpoint = get_endpoint(dataset)

  url_query = paste0(entrypoint,endpoint,filter)

  tryCatch({

    raw_data = GET(url_query)
    res = fromJSON(rawToChar(raw_data$content))

    # RESPONSE NOT OK
    if (res$header$code!=200) {
      stop(res$header$message)
    }

    # NO DATA FOUND
    else if (length(res$data)==0) {
      stop("No data found")
    }

    # OK
    else {
      formatted_data = res$data %>%
        mutate(across(matches('lastupdate'),as.Date),
               across(matches('lastupload'),as.Date)) %>%
        rename_with(toupper)
    }

    # API ERROR
  }, error = function(e) {
    print(e)
    stop(e)
  })

  if(!missing(sort) && all(toupper(sort) %in% colnames(formatted_data))) formatted_data = formatted_data %>% arrange_at(toupper(sort))

  if(verbose) print('More informations on non-financial data are available at https://docs.lasocietenouvelle.org/empreinte-societale/indicateurs')

  return(formatted_data)

}

utils::globalVariables(c("lastupdate","lastupload"))
