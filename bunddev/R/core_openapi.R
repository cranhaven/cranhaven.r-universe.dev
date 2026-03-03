#' @importFrom stats setNames
NULL

#' List OpenAPI endpoints for an API
#'
#' @param id Registry id.
#'
#' @details
#' Parses the cached OpenAPI spec and returns every available path + method with
#' its operationId and summary, if provided.
#'
#' @seealso
#' [bunddev_parameters()] to inspect parameters and [bunddev_spec()] to access
#' the full spec.
#'
#' @examples
#' \dontrun{
#' bunddev_endpoints("autobahn")
#' }
#'
#' @return A tibble with endpoints.
#' @export
bunddev_endpoints <- function(id) {
  spec <- bunddev_spec(id)
  paths <- spec$paths

  if (is.null(paths) || length(paths) == 0) {
    cli::cli_abort("OpenAPI spec has no paths.")
  }

  endpoints <- purrr::imap_dfr(paths, function(path_item, path) {
    if (!is.list(path_item)) {
      return(tibble::tibble())
    }

    methods <- names(path_item)
    purrr::map_dfr(methods, function(method) {
      operation <- path_item[[method]]
      if (!is.list(operation)) {
        return(tibble::tibble())
      }

      operation_id <- if (is.null(operation$operationId)) NA_character_ else operation$operationId
      summary <- if (is.null(operation$summary)) NA_character_ else operation$summary

      tibble::tibble(
        method = method,
        path = path,
        operation_id = operation_id,
        summary = summary
      )
    })
  })

  if (nrow(endpoints) == 0) {
    cli::cli_abort("No endpoints found in the OpenAPI spec.")
  }

  endpoints
}

#' List OpenAPI parameters for an API
#'
#' @param id Registry id.
#' @param name Optional parameter name to filter.
#' @param path Optional endpoint path to filter.
#' @param method Optional HTTP method to filter.
#'
#' @details
#' Returns one row per parameter defined in the OpenAPI spec. Enumerations are
#' stored in a list-column (`enum`). Use the filter arguments to narrow down
#' results to a specific endpoint or parameter name.
#'
#' @seealso
#' [bunddev_parameters_for()] for adapter-specific parameters and
#' [bunddev_parameter_values()] for enum values.
#'
#' @examples
#' \dontrun{
#' bunddev_parameters("smard", name = "resolution")
#' }
#'
#' @return A tibble with parameter metadata.
#' @export
bunddev_parameters <- function(id, name = NULL, path = NULL, method = NULL) {
  spec <- bunddev_spec(id)
  paths <- spec$paths

  if (is.null(paths) || length(paths) == 0) {
    cli::cli_abort("OpenAPI spec has no paths.")
  }

  params <- purrr::imap_dfr(paths, function(path_item, path) {
    if (!is.list(path_item)) {
      return(tibble::tibble())
    }

    path_params <- path_item$parameters %||% list()
    methods <- setdiff(names(path_item), "parameters")

    purrr::map_dfr(methods, function(method) {
      operation <- path_item[[method]]
      op_params <- operation$parameters %||% list()
      combined <- c(path_params, op_params)

      if (length(combined) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(combined, function(param) {
        resolved <- bunddev_resolve_parameter(param, spec)
        if (is.null(resolved)) {
          return(tibble::tibble())
        }

        schema <- resolved$schema %||% list()
        tibble::tibble(
          method = method,
          path = path,
          name = resolved$name %||% NA_character_,
          location = resolved$`in` %||% NA_character_,
          required = isTRUE(resolved$required),
          description = resolved$description %||% NA_character_,
          schema_type = schema$type %||% NA_character_,
          enum = list(schema$enum %||% character())
        )
      })
    })
  })

  if (nrow(params) == 0) {
    cli::cli_abort("No parameters found in the OpenAPI spec.")
  }

  if (!is.null(name)) {
    params <- dplyr::filter(params, .data$name == name)
  }
  if (!is.null(path)) {
    params <- dplyr::filter(params, .data$path == path)
  }
  if (!is.null(method)) {
    params <- dplyr::filter(params, .data$method == method)
  }

  params
}

bunddev_resolve_parameter <- function(param, spec) {
  if (is.null(param)) {
    return(NULL)
  }
  ref <- param$`$ref`
  if (!is.null(ref)) {
    name <- stringr::str_replace(ref, "^#/components/parameters/", "")
    if (!is.null(spec$components$parameters[[name]])) {
      return(spec$components$parameters[[name]])
    }
    return(NULL)
  }
  param
}

#' Extract parameter enum values
#'
#' @param endpoint Adapter function or its name.
#' @param name Parameter name.
#'
#' @details
#' Returns unique enum values for a parameter defined on the adapter endpoint.
#'
#' @seealso
#' [bunddev_parameters_for()] to inspect all parameters for an adapter.
#'
#' @examples
#' \dontrun{
#' bunddev_parameter_values(smard_timeseries, "resolution")
#' }
#'
#' @return A character vector of enum values.
#' @export
bunddev_parameter_values <- function(endpoint, name) {
  endpoint_name <- bunddev_endpoint_name(endpoint)
  spec <- bunddev_endpoint_spec(endpoint_name)
  params <- bunddev_parameters(
    spec$api,
    name = name,
    path = spec$path,
    method = spec$method
  )
  enums <- purrr::map(params$enum, ~ as.character(.x))
  unique(unlist(enums))
}

#' List OpenAPI parameters for a specific adapter
#'
#' @param endpoint Adapter function or its name.
#'
#' @details
#' Resolves the adapter to an OpenAPI path/method mapping and filters parameters
#' accordingly.
#'
#' @seealso
#' [bunddev_parameters()] for the full API parameter table.
#'
#' @examples
#' \dontrun{
#' bunddev_parameters_for(smard_timeseries)
#' }
#'
#' @return A tibble with parameter metadata.
#' @export
bunddev_parameters_for <- function(endpoint) {
  endpoint_name <- bunddev_endpoint_name(endpoint)
  spec <- bunddev_endpoint_spec(endpoint_name)
  bunddev_parameters(spec$api, path = spec$path, method = spec$method)
}

bunddev_endpoint_name <- function(endpoint) {
  if (is.character(endpoint) && length(endpoint) == 1) {
    return(endpoint)
  }

  name <- deparse(substitute(endpoint))
  if (!identical(name, "endpoint")) {
    return(name)
  }

  if (is.function(endpoint)) {
    name <- deparse(substitute(endpoint, parent.frame()))
    return(name)
  }

  "endpoint"
}

bunddev_endpoint_spec <- function(endpoint) {
  spec <- bunddev_endpoint_map()[[endpoint]]
  if (is.null(spec)) {
    cli::cli_abort("No endpoint mapping found for '{endpoint}'.")
  }
  spec
}

bunddev_endpoint_map <- function() {
  endpoints_path <- system.file("registry", "endpoints.yml", package = "bunddev")
  if (endpoints_path == "") {
    cli::cli_abort("Endpoints file not found.")
  }
  
  endpoints_yaml <- yaml::read_yaml(endpoints_path)
  if (is.null(endpoints_yaml) || !is.list(endpoints_yaml)) {
    cli::cli_abort("Endpoints file is empty or invalid.")
  }
  
  # Convert to named list keyed by endpoint name
  purrr::map(endpoints_yaml, ~ list(
    api = .x$api,
    path = .x$path,
    method = .x$method
  )) |> setNames(purrr::map_chr(endpoints_yaml, "name"))
}
