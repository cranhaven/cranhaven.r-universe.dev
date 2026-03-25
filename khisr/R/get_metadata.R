#' Get Metadata from a DHIS2 Instance
#'
#' `get_metadata` retrieves metadata for a specified endpoint of a DHIS2 instance.
#'
#' @param endpoint The DHIS2 API endpoint for the metadata of interest
#'   (e.g., `dataElements`, `organisationUnits` endpoints).
#' @param ... One or more [metadata_filter()] parameters in key-value pairs.
#' @param fields The specific columns to be returned in the data frame.
#' @param retry Number of times to retry the API call in case of failure
#'   (defaults to 2).
#' @param verbosity Level of HTTP information to print during the call:
#'   - 0: No output
#'   - 1: Show headers
#'   - 2: Show headers and bodies
#'   - 3: Show headers, bodies, and CURL status message.
#' @param timeout Maximum number of seconds to wait for the DHIS2 API response.
#' @param call The caller environment
#'
#' @return A tibble containing the DHIS2 metadata response.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Get the categories metadata
#' get_metadata('categories')
#'
#' # Get the datasets metadata with fields 'id,name,organisationUnits' and filter
#' # only the datasets with id 'WWh5hbCmvND'
#' get_metadata('dataSets',
#'              fields = 'id,name,organisationUnits[id,name,path]',
#'              id %.eq% 'WWh5hbCmvND')
#'
#' # Get data elements filtered by dataElementGroups id
#' get_metadata('dataElements',
#'              dataElementGroups.id %.eq% 'IXd7DXxZqzL',
#'              fields = ':all')

get_metadata <- function(endpoint,
                         ...,
                         fields = c('id', 'name'),
                         retry = 2,
                         verbosity = 0,
                         timeout = 60,
                         call = caller_env()) {

    check_scalar_character(endpoint, call = call)
    check_string_vector(fields, call = call)

    response <- tryCatch({
        api_get(endpoint = endpoint,
                ...,
                fields = str_c(fields, collapse = ','),
                retry = retry,
                verbosity = verbosity,
                timeout = timeout,
                call = call)
    }, error = function(e) {
        khis_warn(c('x' = 'Error retrieving metadata:', message = e), call = call)
        return(NULL)
    })

    if (is.null(response)) return(NULL)  # Handle potential errors in api_get

    data <- as_tibble(response) %>%
        hoist(endpoint)

    if (nrow(data) == 0) {
        khis_warn(c('!' = 'No data found for the specified endpoint.'), call = call)
        return(NULL)
    }

    data %>%
        unnest_wider(all_of(endpoint))
}
