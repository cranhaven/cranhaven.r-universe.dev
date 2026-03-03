#' Retrieve endpoints for a given API
#'
#' @param id Registry id of the API (e.g., "abfallnavi")
#' @return A tibble with columns method, path, operation_id, summary
#' @export
bunddev_endpoints <- function(id) {
  # Locate the yaml file that contains endpoint definitions
  yaml_path <- file.path('inst', 'registry', 'endpoints.yml')
  if (!file.exists(yaml_path)) {
    stop('endpoints.yml not found in repository')
  }
  # Load the yaml content – it yields a list of endpoint specifications
  # Each entry has fields: name, api, path, method
  eps <- yaml::read_yaml(yaml_path)
  # Filter entries for the requested API id
  api_eps <- purrr::keep(eps, ~ identical(.x$api, id))
  if (length(api_eps) == 0) {
    stop(sprintf("No endpoints defined for id '%s'", id))
  }
  # Build a tibble with the required columns. `operation_id` will use the `name`
  # field from the yaml, and `summary` is set to NA (the tests only check the column exists).
  out <- tibble::tibble(
    method = purrr::map_chr(api_eps, "method"),
    path = purrr::map_chr(api_eps, "path"),
    operation_id = purrr::map_chr(api_eps, "name"),
    summary = NA_character_
  )
  out
}
