#' Retrieves Data Set Reporting Rate Metrics
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `get_data_sets_by_level()` fetches the data set reporting metrics. The metric
#' can be REPORTING_RATE, REPORTING_RATE_ON_TIME, ACTUAL_REPORTS, ACTUAL_REPORTS_ON_TIME, EXPECTED_REPORTS.
#'
#' @param dataset_ids Required vector of data sets IDs for which to retrieve data. Required.
#' @param start_date Optional start date to retrieve data. It is required and in the format `YYYY-MM-dd`.
#' @param end_date Optional ending date for data retrieval (default is the current date).
#' @param level Required desired organisation level of data (default: level 1) .
#' @param org_ids Optional list of organization units IDs to be filtered.
#' @param ... Other options that can be passed onto DHIS2 API.
#' @param call The caller environment.
#'
#' @return A tibble with detailed information, including:
#'
#' * Geographical identifiers (country, subnational, district, facility, depending on level)
#' * Reporting period (month, year, fiscal year)
#' * The reporting metric can be REPORTING_RATE, REPORTING_RATE_ON_TIME, ACTUAL_REPORTS, ACTUAL_REPORTS_ON_TIME, EXPECTED_REPORTS.
#'
#' @export
#'
#' @seealso
#' * [get_organisations_by_level()] for getting the organisations units
#' * [get_data_sets()] for retrieving the data sets
#'
#' @examplesIf khis_has_cred()
#' # The MoH 745 Cancer Screening Program Monthly Summary Form
#' dataset_id = c('WWh5hbCmvND')
#'
#' # Download data from February 2023 to current date
#' data <- get_data_sets_by_level(dataset_ids = dataset_id,
#'                                start_date = '2023-02-01')
#' data

get_data_sets_by_level <- function(dataset_ids,
                                  start_date,
                                  end_date = NULL,
                                  level = 1,
                                  org_ids = NULL,
                                  ...,
                                  call = caller_env()) {

    dx = period = pe = ou = element = value = NULL # due to NSE notes in R CMD check

    check_string_vector(dataset_ids, call = call)
    check_date(start_date, error_call = call)
    check_date(end_date, can_be_null = TRUE, error_call = call)
    check_integerish(level, call = call)
    org_levels <- check_level_supported(level, ..., call = call)

    values <- c('REPORTING_RATE',
                'REPORTING_RATE_ON_TIME',
                'ACTUAL_REPORTS',
                'ACTUAL_REPORTS_ON_TIME',
                'EXPECTED_REPORTS')

    combined <- expand.grid(dataset_ids, values, stringsAsFactors = FALSE)
    dataset_ids_str <- pmap_chr(combined, ~ str_c(..1, ..2, sep = '.'))
    if (is.null(end_date)) {
        end_date = today()
    }
    periods <- format(seq(ymd(start_date), ymd(end_date), by = 'month'), '%Y%m')

    #ou <- 'HfVjCurKxh2' # Kenya
    ou <- NULL
    if (!is.null(org_ids)) {
        check_string_vector(org_ids, call = call)
        ou <- org_ids
    }

    data <- get_analytics(
        dx %.d% dataset_ids_str,
        pe %.d% periods,
        ou %.d% c(str_glue('LEVEL-{level}'), ou),
        ...,
        call = call
    )

    organisations <- get_organisations_by_level(data$ou, level = level, ..., call = call)
    datasets <- get_data_sets(id %.in% dataset_ids, fields = 'id,name~rename(dataset)', ..., call = call)

    data %>%
        separate_wider_delim(dx, ".",  names = c('dx','element')) %>%
        left_join(organisations, by = c('ou'='id'), relationship = 'many-to-many') %>%
        left_join(datasets, by = c('dx'='id'), relationship = 'many-to-many') %>%
        mutate(
            period = ym(pe),
            month = month(period, label = TRUE, abbr = FALSE),
            year = year(period),
        ) %>%
        select(-ou,-dx,-pe) %>%
        pivot_wider(names_from = element, values_from = value, values_fill = 0) %>%
        clean_names()
}
