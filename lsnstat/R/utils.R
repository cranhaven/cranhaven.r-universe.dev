#' Get dataset list and related informations
#'#'
#' @examples
#'
#' get_lsn_dataset_list()
#'
#' @export

get_lsn_dataset_list = function()
{

  fromJSON(rawToChar(GET("https://api.stats.lasocietenouvelle.org/datasets/macro/all")$content))$data

}

#' Get table-related endpoint
#'
#' @param dataset dataset requested.
#'
#' @examples
#'
#' lsnstat:::get_endpoint("figaro_main_aggregates")


get_endpoint = function(dataset)
{

  if(missing(dataset)) stop("'dataset' is missing")

  get_lsn_dataset_list() %>%
    filter(dataset == !!dataset) %>%
    select(endpoint) %>%
    unlist()

}

#' Get formatted SQL filter from R named list
#'
#' @param filter_list Filter (named list).
#'
#' @examples
#'
#' fl = list(year = '2018',industry = 'A01',serie_id = 'ghg_obs')
#' lsnstat:::from_filter_list_to_sql(fl)


from_filter_list_to_sql = function(filter_list)
{

  if(missing(filter_list)) stop("'filter_list' is missing")

  filter = "?"

  for(i in 1:length(filter_list))
  {
    filter = paste0(filter,
                          ifelse(i == 1,"","&"),
                          paste0(names(filter_list)[i],"=",
                                 paste0(unique(unlist(filter_list[i])),collapse = "+")))
  }

  return(filter)
}
