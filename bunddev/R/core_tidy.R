#' Call an API operation and return tidy data
#'
#' @param api Registry id.
#' @param operation_id OpenAPI operationId.
#' @param params List of parameters.
#' @param ... Additional arguments passed to the tidier.
#'
#' @details
#' This helper calls [bunddev_call()] and then applies the adapter-specific
#' tidier for the API. If no tidier is registered, a tibble containing the raw
#' response is returned.
#'
#' @seealso
#' [bunddev_call()] for raw responses, and [bunddev_parameters()] to discover
#' available parameters.
#'
#' @examples
#' \dontrun{
#' # Tidy Autobahn roadworks
#' bunddev_call_tidy("autobahn", "list-roadworks", params = list(roadId = "A1"))
#' }
#'
#' @return A tibble with tidied results.
#' @export
bunddev_call_tidy <- function(api, operation_id, params = list(), ...) {
  response <- bunddev_call(api, operation_id, params = params, parse = "json", safe = TRUE)
  tidier <- bunddev_tidy_dispatch(api)

  if (is.null(tidier)) {
    return(tibble::tibble(raw = list(response)))
  }

  tidier(response, operation_id = operation_id, ...)
}

bunddev_tidy_dispatch <- function(api) {
  if (api == "bewerberboerse") {
    return(bunddev_tidy_bewerberboerse)
  }
  if (api == "autobahn") {
    return(bunddev_tidy_autobahn)
  }
  if (api == "tagesschau") {
    return(bunddev_tidy_tagesschau)
  }
  if (api == "smard") {
    return(bunddev_tidy_smard)
  }
  if (api == "dwd") {
    return(bunddev_tidy_dwd)
  }
  if (api == "jobsuche") {
    return(bunddev_tidy_jobsuche)
  }
  NULL
}

bunddev_flatten_list_cols <- function(data, cols, mode = "drop") {
  mode <- rlang::arg_match(mode, c("drop", "json", "unnest"))
  cols <- intersect(cols, names(data))

  if (length(cols) == 0) {
    return(data)
  }

  if (mode == "drop") {
    return(dplyr::select(data, -dplyr::any_of(cols)))
  }

  if (mode == "json") {
    for (col in cols) {
      data[[col]] <- purrr::map_chr(
        data[[col]],
        ~ jsonlite::toJSON(.x, auto_unbox = TRUE, null = "null")
      )
    }
    return(data)
  }

  for (col in cols) {
    data <- tidyr::unnest_longer(data, dplyr::all_of(col), values_to = col)
  }
  data
}
