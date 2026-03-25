#' Retrieves Disaggregated Analytics Data from DHIS2
#'
#' `get_analytics()` retrieves disaggregated data from DHIS2 analytics tables for a
#'   specified period and data element(s), without performing any aggregation.
#'
#' @param ... One or more [analytics_dimension()] parameters in key-value pairs.
#'   These define the analytics query, including data elements, periods, and organization units.
#' @param return_type Optional argument specifying the return format for identifiers.
#'   defaults to `'uid'`. Choose `'name'` for human readable labels.
#' @param retry Number of times to retry the API call in case of failure (defaults to 2).
#' @param verbosity Level of HTTP information to print during the call:
#'   - 0: No output
#'   - 1: Show headers
#'   - 2: Show headers and bodies
#'   - 3: Show headers, bodies, and CURL status message.
#' @param timeout Maximum number of seconds to wait for the API response.

#' @details
#' * Retrieves data directly from DHIS2 analytics tables.
#' * Allows specifying analytics dimensions, return format for identifiers, retry
#'   attempts, and logging verbosity.
#'
#' @return A tibble containing the disaggregated analytics data, or `NULL` if no data is retrieved.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Clinical Breast Examination data elements
#' # XEX93uLsAm2 = CBE Abnormal
#' # cXe64Yk0QMY = CBE Normal
#' element_id <- c('cXe64Yk0QMY', 'XEX93uLsAm2')
#'
#' # Download data from February 2023 to current date
#' data <- get_analytics(dx %.d% element_id, pe %.d% 'LAST_MONTH')
#' data

get_analytics <- function(...,
                          return_type = c('uid', 'name'),
                          retry = 2,
                          verbosity = 0,
                          timeout = 60) {
    x = NULL # due to NSE notes in R CMD check

    return_type <- str_to_upper(arg_match(return_type))

    response <- api_get('analytics',
                    ...,
                    outputIdScheme=return_type,
                    skipData = 'false',
                    skipMeta = 'true',
                    retry = retry,
                    verbosity = verbosity,
                    timeout = timeout)

    if (NROW(response$rows) == 0) {
        return(NULL)
    }

    headers <- map_vec(response$headers, ~ pluck(.x, 'name'))
    value_types <- map_vec(response$headers, ~ pluck(.x, 'valueType'))
    names(value_types) <- headers
    data <- tibble(x = response$rows) %>%
        unnest_wider(x, names_sep = '') %>%
        rename_all(~ headers) %>%
        mutate(across(everything(), ~ if(value_types[[cur_column()]] == 'NUMBER') { as.numeric(.x)  } else { .x }))

    return(data)
}
