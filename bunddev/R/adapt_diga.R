#' DiGA API adapters
#'
#' Functions to call the DiGA API endpoints.
#'
#' @keywords internal
#' @name adapt_diga
NULL

#' List DiGA device definitions
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to expand list-columns into multiple rows.
#'
#' @return A tibble containing DiGA device definitions from the FHIR API. When
#'   `flatten = FALSE`, nested FHIR resource elements are preserved as list
#'   columns. When `flatten = TRUE`, list columns are expanded based on the
#'   specified `flatten_mode`.
#'
#' @export
#' @examples
#' \dontrun{dig_device_definitions()}

diga_device_definitions <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthApp")
  response <- diga_request("/DeviceDefinition", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

#' List DiGA catalog entries
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns.
#'
#' @return A tibble containing DiGA catalog entry resources from the FHIR API.
#'   Structure depends on `flatten` setting (see [diga_device_definitions()]).
#'
#' @export

diga_catalog_entries <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthAppCatalogEntry")
  response <- diga_request("/CatalogEntry", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

#' List DiGA manufacturers
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns.
#'
#' @return A tibble containing DiGA manufacturer organization resources from
#'   the FHIR API. Structure depends on `flatten` setting.
#'
#' @export

diga_organizations <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthAppManufacturer")
  response <- diga_request("/Organization", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

#' List DiGA prescription units
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns.
#'
#' @return A tibble containing DiGA prescription unit (ChargeItemDefinition)
#'   resources from the FHIR API. Structure depends on `flatten` setting.
#'
#' @export

diga_charge_item_definitions <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthAppPrescriptionUnit")
  response <- diga_request("/ChargeItemDefinition", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

#' List DiGA questionnaires
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns.
#'
#' @return A tibble containing DiGA questionnaire resources from the FHIR API.
#'   Structure depends on `flatten` setting.
#'
#' @export

diga_questionnaires <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthAppQuestionnaire")
  response <- diga_request("/Questionnaire", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

#' List DiGA questionnaire responses
#'
#' @param params Query parameters.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached responses.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns.
#'
#' @return A tibble containing DiGA questionnaire response resources from the
#'   FHIR API. Structure depends on `flatten` setting.
#'
#' @export

diga_questionnaire_responses <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  params <- diga_add_defaults(params, profile = "https://fhir.bfarm.de/StructureDefinition/HealthAppQuestionnaireResponse")
  response <- diga_request("/QuestionnaireResponse", params = params, safe = safe, refresh = refresh)
  diga_tidy_bundle(response, flatten, flatten_mode)
}

# Helper to add default parameters

diga_add_defaults <- function(params, profile) {
  if (is.null(params$`_count`)) params$`_count` <- 1000
  if (is.null(params$`_profile`)) params$`_profile` <- profile
  params
}

# Core request function – delegates to bunddev_call

diga_request <- function(path, params, safe = TRUE, refresh = FALSE, parse = "json") {
  bunddev_call(
    "diga",
    path = path,
    method = "GET",
    params = params,
    parse = parse,
    safe = safe,
    refresh = refresh
  )
}

# Tidy helper – transforms FHIR bundle into tibble

diga_tidy_bundle <- function(response, flatten, flatten_mode) {
  if (is.null(response) || length(response) == 0) return(tibble::tibble())
  entries <- response$entry
  if (is.null(entries) || length(entries) == 0) {
    data <- tibble::tibble(bundle = list(response))
    if (flatten) {
      return(bunddev_flatten_list_cols(data, cols = "bundle", mode = flatten_mode))
    }
    return(data)
  }
  rows <- purrr::map(entries, function(entry) entry$resource %||% list())
  data <- dplyr::bind_rows(rows)
  if (flatten) {
    list_cols <- names(data)[purrr::map_lgl(data, is.list)]
    return(bunddev_flatten_list_cols(data, cols = list_cols, mode = flatten_mode))
  }
  data
}

# Wrapper functions to expose via package namespace

bunddev_diga_device_definitions <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_device_definitions(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_diga_catalog_entries <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_catalog_entries(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_diga_organizations <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_organizations(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_diga_charge_item_definitions <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_charge_item_definitions(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_diga_questionnaires <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_questionnaires(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_diga_questionnaire_responses <- function(params = list(), safe = TRUE, refresh = FALSE, flatten = FALSE, flatten_mode = "json") {
  diga_questionnaire_responses(params = params, safe = safe, refresh = refresh, flatten = flatten, flatten_mode = flatten_mode)
}
