#' DHIS2 Metadata Helper Functions
#'
#' These functions simplify retrieving data from specific DHIS2 API endpoints
#'   using [get_metadata()].
#'
#' @param ... One or more metadata filters [metadata_filter()] in key-value pairs.
#' @inheritDotParams get_metadata -endpoint
#'
#' @return A tibble containing the DHIS2 metadata response.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Get all organisation units
#' get_organisation_units()
#'
#' # Get all data elements
#' get_data_elements()
#'
#' # Get data elements by element ids
#' get_data_elements(id %.in% c('VR7vdS7P0Gb', 'gQro1y7Rsbq'))
#'
#' # Get datasets by name with the word 'MOH 705'
#' get_data_sets(name %.like% 'MOH 705')
#'
#' @name metadata-helpers

get_categories <- function(...) {
    get_metadata('categories', ...)
}

#' @rdname metadata-helpers
#' @export

get_category_combos <- function(...) {
    get_metadata('categoryCombos', ...)
}

#' @rdname metadata-helpers
#' @export

get_category_option_combos <- function(...) {
    get_metadata('categoryOptionCombos', ...)
}

#' @rdname metadata-helpers
#' @export

get_category_option_group_sets <- function(...) {
    get_metadata('categoryOptionGroupSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_category_option_groups <- function(...) {
    get_metadata('categoryOptionGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_category_options <- function(...) {
    get_metadata('categoryOptions', ...)
}

#' @rdname metadata-helpers
#' @export

get_data_element_group_sets <- function(...) {
    get_metadata('dataElementGroupSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_data_element_groups <- function(...) {
    get_metadata('dataElementGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_data_elements <- function(...) {
    get_metadata('dataElements', ...)
}

#' @rdname metadata-helpers
#' @export

get_data_sets <- function(...) {
    get_metadata('dataSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_user_groups <- function(...) {
    get_metadata('userGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_indicator_group_sets <- function(...) {
    get_metadata('indicatorGroupSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_indicator_groups <- function(...) {
    get_metadata('indicatorGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_indicators <- function(...) {
    get_metadata('indicators', ...)
}

#' @rdname metadata-helpers
#' @export

get_option_group_sets <- function(...) {
    get_metadata('optionGroupSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_option_groups <- function(...) {
    get_metadata('optionGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_option_sets <- function(...) {
    get_metadata('optionSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_options <- function(...) {
    get_metadata('options', ...)
}

#' @rdname metadata-helpers
#' @export

get_organisation_unit_groupsets <- function(...) {
    get_metadata('organisationUnitGroupSets', ...)
}

#' @rdname metadata-helpers
#' @export

get_organisation_unit_groups <- function(...) {
    get_metadata('organisationUnitGroups', ...)
}

#' @rdname metadata-helpers
#' @export

get_organisation_units <- function(...) {
    get_metadata('organisationUnits', ...)
}

#' @rdname metadata-helpers
#' @export

get_organisation_unit_levels <- function(...) {
    get_metadata('organisationUnitLevels', ...)
}

#' @rdname metadata-helpers
#' @export

get_dimensions <- function(...) {
    get_metadata('dimensions', ...)
}

#' @rdname metadata-helpers
#' @export

get_period_types <- function(...) {
    get_metadata('periodTypes', ...)
}

#' @rdname metadata-helpers
#' @export
get_user_profile <- function() {
    api_get('me')
}
