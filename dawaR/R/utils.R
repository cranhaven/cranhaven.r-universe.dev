# nolint start
.dawaR_env <- new.env(parent = emptyenv())

.dawaR_env$deprecation_warning_shown <- FALSE

warn_api_deprecation <- function() {
  if (isTRUE(.dawaR_env$deprecation_warning_shown)) {
    return(invisible(NULL))
  }

  warning(
    paste(
      "DAWA is deprecated and is scheduled to shut down on October 1, 2026.",
      "Functions that depend on the API will stop working after that date.",
      "See https://dataforsyningen.dk/data/4924 for details."
    ),
    call. = FALSE,
    immediate. = TRUE
  )

  .dawaR_env$deprecation_warning_shown <- TRUE
  invisible(NULL)
}

.onAttach <- function(...) {
  packageStartupMessage(
    "## {dawaR} provides data from the Danish Agency of Climate Data"
  )
  packageStartupMessage("## DAWA is scheduled to shut down on October 1, 2026.")
  packageStartupMessage("## Terms and conditions apply.")
  packageStartupMessage(
    "## Read more at: https://dawadocs.dataforsyningen.dk/dok/om#vilkaar"
  )
}

get_status_message <- function() {
  body <- httr2::request(
    "https://raw.githubusercontent.com/aleksanderbl29/dawaR/refs/heads/main/status-message"
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  gsub("\\n", "\n", x = body)
}
# nolint end

available_coordinates <- function() {
  x <- c(
    "WGS84",
    "4326",
    "ETRS89",
    "UTM zone 32 Nord",
    "UTM32",
    "utm32",
    "25832"
  )

  return(x)
}

check_coordinate_type <- function(type_to_check) {
  test <- type_to_check %in% available_coordinates()
  return(test)
}

coordinate_type <- function(type) {
  if (check_coordinate_type(type)) {
    if (type %in% c("WGS84", "4326")) {
      type_to_return <- "4326"
    } else if (
      type %in%
        c(
          "ETRS89",
          "UTM zone 32 Nord",
          "UTM32",
          "utm32",
          "25832"
        )
    ) {
      type_to_return <- "25832"
    }
  } else {
    cli::cli_abort(
      c(
        "Coordinate type {.var {type}} is not one of the available types",
        "Use one of {.var WGS84} or {.var UTM32}"
      )
    )
  }
  return(type_to_return)
}

api_timings <- list(
  # this is from /benchmarking/timing.qmd and the accompanying
  # /benchmarking/map_timings.rda
  afstemningsomraader = "18.77s",
  kommuner = "13.13s",
  landsdele = "10.13s",
  menighedsraadsafstemningsomraader = "25.9s",
  opstillingskredse = "14.14s",
  politikredse = "14.14s",
  postnumre = "9.34s",
  regioner = "10.24s",
  retskredse = "11.44s",
  sogne = "26.71s",
  steder = "2.9m",
  storkredse = "11.16s",
  vlaglandsdele = "10.47s"
)
