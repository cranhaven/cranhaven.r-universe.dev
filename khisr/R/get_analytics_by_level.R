#' Retrieves Analytics Table Data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `get_analytics_by_level()` fetches data from the DHIS2 analytics tables for a
#'   given period and data element(s), without performing any aggregation.
#'
#' @param element_ids Required vector of data element IDs for which to retrieve data.
#' @param start_date Required start date to retrieve data. It is required and in the format `YYYY-MM-dd`.
#' @param end_date Optional ending date for data retrieval (default is the current date).
#' @param level The desired organisation level of data (default: level 1)
#' @param org_ids Optional list of organization units IDs to be filtered.
#' @param ... Other options that can be passed onto DHIS2 API.
#' @param call The caller environment.
#'
#' @details
#' * Retrieves data directly from DHIS2 analytics tables.
#' * Supports optional arguments for providing organization lists, data elements, and categories.
#' * Allows specifying DHIS2 session objects, retry attempts, and logging verbosity.
#'
#' @return A tibble with detailed information, including:
#'
#' * Geographical identifiers (country, subnational, district, facility, depending on level)
#' * Reporting period (month, year, fiscal year)
#' * Data element names
#' * Category options
#' * Reported values
#'
#' @export
#'
#' @seealso
#' * [get_organisations_by_level()] for getting the organisations units
#' * [get_data_elements_with_category_options()] for retrieving the data elements
#'
#' @examplesIf khis_has_cred()
#' # Clinical Breast Examination data elements
#' # XEX93uLsAm2 = CBE Abnormal
#' # cXe64Yk0QMY = CBE Normal
#' element_id = c('cXe64Yk0QMY', 'XEX93uLsAm2')
#'
#' # Download data from February 2023 to current date
#' data <- get_analytics_by_level(element_ids = element_id,
#'                                start_date = '2023-02-01')
#' data

get_analytics_by_level <- function(element_ids,
                                   start_date,
                                   end_date = NULL,
                                   level = 1,
                                   org_ids = NULL,
                                   ...,
                                   call = caller_env()) {

    dx = co = ou = pe = value = period = NULL # due to NSE notes in R CMD check

    check_string_vector(element_ids, call = call)
    check_date(start_date, error_call = call)
    check_date(end_date, can_be_null = TRUE, error_call = call)
    check_integerish(level, call = call)
    org_levels <- check_level_supported(level, ..., call = call)

    if (is.null(end_date)) {
        end_date = today()
    }


    #ou <- 'HfVjCurKxh2' # Kenya
    ou <- NULL
    if (!is.null(org_ids)) {
        check_string_vector(org_ids, call = call)
        ou <- org_ids
    }

    data <- get_analytics(
        dx %.d% element_ids,
        pe %.d% 'all',
        ou %.d% c(str_glue('LEVEL-{level}'), ou),
        co %.d% 'all',
        startDate = start_date,
        endDate = end_date,
        ...,
        call = call
    )

    if (is_empty(data)) {
        return(NULL)
    }

    organisations <- get_organisations_by_level(org_ids = data$ou, level = level, ..., call = call)
    elements <- get_data_elements_with_category_options(element_ids, ..., call = call)

    data %>%
        left_join(organisations, by=c('ou' = 'id')) %>%
        left_join(elements, by=c('dx'='element_id', 'co'='category_id')) %>%
        mutate(
            period = ym(pe),
            month = month(period, label = TRUE, abbr = FALSE),
            year = year(period),
        ) %>%
        select(-ou, -co, -dx, -pe)
}
