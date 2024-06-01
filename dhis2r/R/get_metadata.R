#' #' Get metadata from a DHIS2 instance
#' #'
#' #' @description `Dhis2r$get_metadata()`
#' #' Get metadata about any available resource from a DHIS2 instance e.g "dataElements",
#' #' "organisationUnits", "indicators", "periodTypes"
#' #'
#' #' @return A data frame
#' #'
#' #' @param endpoint a resource, get the available resources using `get_metadata()` without any arguments
#' #' @param fields The specific columns to be return in the dataframe e.g c("name","id")
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # connect to the DHIS2 instance
#' #' dhis2_play_connection <- Dhis2r$new(base_url = "https://play.dhis2.org/", username = "admin",  password = "district",api_version = "2.39.0.1", api_version_position = "before")
#' #'
#' #' # get all the available resources
#' #' resources <- dhis2_play_connection$get_metadata()
#' #'
#' #' # get organisation Units with the default fields i.e c("name","id")
#' #' organisationUnits <- dhis2_play_connection$get_metadata(endpoint = "organisationUnits")
#' #'
#' #' # get a vector of all possible fields of a organisation unit resource
#' #' metadata_fields_organisationUnits <-  dhis2_play_connection$get_metadata_fields(endpoint = "organisationUnits")
#' #'
#' #' # get organisation Units with additional fields i.e c("name","id", "level")
#' #'
#' #' organisationUnits <- dhis2_play_connection$get_metadata(endpoint = "organisationUnits",fields =  c("name","id", "level"))
#' #'
#' #' }
#'
#'
#' Dhis2r$set("public", "get_metadata", overwrite = T,
#'            function(endpoint = NULL, fields = c("name","id")) {
#'
#'              if(is.null(endpoint)){
#'
#'                reponse <- self$request_sent |>
#'                  req_url_path_append("resources") |>
#'                  req_perform()
#'
#'                response_data  <-  reponse |>
#'                  resp_body_json(simplifyVector = TRUE)
#'
#'                tibble::tibble(response_data$resources)
#'
#'              }else{
#'
#'                reponse <- self$request_sent |>
#'                  req_url_path_append(endpoint) |>
#'                  req_url_query(fields = paste0(fields, collapse = ",")) |>
#'                  req_perform()
#'
#'                response_data  <-  reponse |>
#'                  resp_body_json(simplifyVector = TRUE)
#'
#'
#'                tibble::tibble( response_data[[1]])
#'
#'
#'              }
#'
#'
#'
#'            }
#' )
#'
#'
#' #' Get all possible fields for a specific metadata resource from a DHIS2 instance
#' #'
#' #' @return A vector of all possible fields for a specific metadata
#' #'
#' #' @param endpoint a resource, get the available resources using `get_metadata()` without any arguments
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # connect to the dhis2 instance
#' #' dhis2_play_connection <- Dhis2r$new(base_url = "https://play.dhis2.org/", username = "admin",  password = "district",api_version = "2.39.0.1", api_version_position = "before"))
#' #'
#' #' # get all the available resources
#' #' resources <- dhis2_play_connection$get_metadata()
#' #'
#' #' # get organisation Units with the default fields i.e c("name","id")
#' #' organisationUnits <- dhis2_play_connection$get_metadata(endpoint = "organisationUnits")
#' #'
#' #' # get a vector of all possible fields of a organisation unit resource
#' #' metadata_fields_organisationUnits <-  dhis2_play_connection$get_metadata_fields(endpoint = "organisationUnits")
#' #'
#' #' # get organisation Units with additional fields i.e c("name","id", "level")
#' #'
#' #' organisationUnits <- dhis2_play_connection$get_metadata(endpoint = "organisationUnits",fields =  c("name","id", "level"))
#' #'
#' #' }
#'
#' Dhis2r$set("public", "get_metadata_fields", overwrite = T,
#'            function(endpoint = NULL) {
#'
#'                reponse <- self$request_sent |>
#'                  req_url_path_append(endpoint) |>
#'                  req_url_query(fields = ":all") |>
#'                  req_url_query(paging = "true") |>
#'                  req_url_query(pageSize = "1") |>
#'                  req_perform()
#'
#'                response_data  <-  reponse |>
#'                  resp_body_json(simplifyVector = TRUE)
#'
#'
#'                sort(names(response_data[[2]]))
#'              }
#' )
#'
#'
#' #
