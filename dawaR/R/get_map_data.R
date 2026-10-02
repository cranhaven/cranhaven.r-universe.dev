#' Fetch map data to use with `{sf}`.
#'
#' @description This function returns map data from DAWA. It is ready to use
#' with `{sf}` functions. The returned dataframe has date-time columns to inform
#' when the information was last changed. It is returned in Europe/Copenhagen
#' time as that is the *local time* for the API. You can change that behavior by
#' setting `Sys.setenv(TZ = "Your/Timezone")`.
#'
#' @param type Defines the type of map data to request from DAWA. Run
#'   `available_sections(format = "geojson")` to see your options.
#' @param cache Boolean to determine whether or not to cache the api call and
#'   the function output. Default is `TRUE`.
#'
#' @return Returns a `data.frame` object that contains polygons (or points) for
#'   the section provided.
#'
#' @inheritParams dawa
#'
#' @export
#' @family Data functions
#'
#' @examples
#' \dontrun{
#' x <- get_map_data("regioner")
#' ggplot2::ggplot(x) +
#'   ggplot2::geom_sf()
#' }
get_map_data <- function(type, cache = TRUE, ...) {
  warn_api_deprecation()

  params <- rlang::list2(...)

  if (cache == TRUE) {
    if (memoise::has_cache(get_map_data_w_cache)(type, params)) {
      cli::cli_alert(
        "Using cached response.
                        Change this behaviour by setting cache = FALSE"
      )
    }

    get_map_data_w_cache(type = type, params = params)
  } else if (cache == FALSE) {
    get_map_data_nocache(type = type, cache = FALSE, params = params)
  }
}

#' @importFrom rlang list2
get_map_data_w_cache <- memoise::memoise(function(type, params = list()) {
  get_map_data_nocache(type = type, cache = TRUE, params = params)
})

#' @importFrom rlang list2
get_map_data_nocache <- function(type, cache = FALSE, params = list()) {
  if (!type %in% available_sections(format = "geojson", verbose = FALSE)) {
    cli::cli_abort(
      "You have provided type {.var {type}}
                    which is not compatible with this function."
    )
  }

  check_sf_installation(verbose = FALSE)

  if (length(params) == 0) {
    params <- NULL
  }

  if (!connection_check()) {
    cli::cli_alert_warning(
      "You do not have access to api.dataforsyningen.dk.
        Please check your connection settings."
    )
    return(NULL) # Exit early if no connection is detected
  }

  time_info <- api_timings[type]

  if (is.na(names(time_info))) {
    time_info <- "a very long time"
  }

  cli::cli_alert(
    "Getting data on {.var {type}}. This usually takes {time_info}."
  )

  cli::cli_progress_message(
    "Fetching data from the API. This will take some time."
  )

  api_response <- dawa(
    section = type,
    format = "geojson",
    verbose = FALSE,
    cache = cache,
    func_params = params
  )

  if (is.null(api_response)) {
    cli::cli_alert_danger("The API did not return anything.")
    return(NULL)
  }

  # Create temp directory and safe filename
  temp_dir <- tempdir()
  safe_filename <- tolower(type)
  temp_file <- file.path(temp_dir, safe_filename)

  writeLines(api_response, temp_file)

  cli::cli_progress_message(
    "Reading data to `st`.
                              This will also take some time."
  )

  # Clean up api response
  rm(api_response)

  resp_st <- sf::st_read(
    temp_file,
    quiet = TRUE
  )

  # Clean up temp file
  rm(temp_file)

  # Garbagecollect
  gc(verbose = FALSE)

  cli::cli_progress_message("Converting map data to `sf` object")

  map_data <- sf::st_as_sf(resp_st)

  return(map_data)
}
