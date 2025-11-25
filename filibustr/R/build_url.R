build_url <- function(data_source, chamber = "all", congress = NULL, sheet_type = NULL) {
  chamber_code <- match_chamber(chamber)

  congress_code <- match_congress(congress, call = rlang::caller_env())

  url <- switch(
    tolower(data_source),
    voteview = build_voteview_url(sheet_type = sheet_type,
                                  chamber_code = chamber_code,
                                  congress_code = congress_code),
    hvw = build_hvw_url(chamber_code = chamber_code),
    lhy = build_hvw_url(chamber_code = chamber_code),
    les = build_les_url(chamber_code = chamber_code),
    "source not implemented"
  )

  if (url == "source not implemented") {
    cli::cli_abort(c(
      "Invalid data source name: {.arg {data_source}}",
      "i" = paste("Expected data sources (case-insensitive):",
                  "{.arg Voteview},", "{.arg HVW},", "{.arg LHY},", "{.arg LES}")
    ))
  }

  url
}

build_voteview_url <- function(sheet_type, chamber_code = "HS", congress_code = "all") {
  source <- paste0("https://voteview.com/static/data/out/", sheet_type)

  paste0(source, "/", chamber_code, congress_code, "_", sheet_type, ".csv")
}

build_hvw_url <- function(chamber_code) {
  # no "all" option for HVW
  if (!(chamber_code %in% c("H", "S"))) {
    cli::cli_abort(c(
      paste("Invalid {.arg chamber} argument ({.arg {chamber_code}})",
            "provided for {.code get_hvw_data()}."),
      "i" = "{.arg chamber} must be either House or Senate, not both."
    ),
    call = rlang::caller_env(2))
  }

  source <- "https://dataverse.harvard.edu/api/access/datafile"
  file <- if (chamber_code == "H") "6299608" else "6299605"

  paste0(source, "/", file)
}

build_les_url <- function(chamber_code) {
  # no "all" option for LES
  if (!(chamber_code %in% c("H", "S"))) {
    cli::cli_abort(c(
      "Invalid {.arg chamber} argument ({.arg {chamber_code}}) provided for {.code get_les()}.",
      "i" = "{.arg chamber} must be either House or Senate, not both."
    ),
    call = rlang::caller_env(2))
  }

  source <- "https://thelawmakers.org/wp-content/uploads/2025/03/CEL"
  chamber_name <- if (chamber_code == "H") "House" else "Senate"

  paste0(source, chamber_name, "93to118Reduced.dta")
}

match_chamber <- function(chamber) {
  chamber_code <- dplyr::case_match(tolower(chamber),
                                    c("all", "congress", "hs") ~ "HS",
                                    c("house", "h", "hr") ~ "H",
                                    c("senate", "s", "sen") ~ "S",
                                    .default = "HS_default")

  # Warn for invalid chamber argument
  if (chamber_code == "HS_default") {
    cli::cli_warn(paste("Invalid {.arg chamber} argument ({.val {chamber}}) provided.",
                        "Using {.arg chamber = {.val all}}."),
                  call = rlang::caller_env())
    chamber_code <- "HS"
  }

  chamber_code
}

#' Get Voteview string for a specified Congress
#'
#' Get a Congress number as a three-digit string.
#' This is the format of Congress numbers in Voteview data file names.
#'
#' If no Congress number is given, this will return `"all"`.
#' Any argument that is not a valid Congress number (i.e., the integers 1 to
#' `r current_congress()`) is an error.
#'
#' @param congress A Congress number.
#'
#' Valid Congress numbers are integers between 1 and `r current_congress()`
#' (the current Congress).
#'
#'
#' @returns A three-character string.
#'
#' Either three digits between `"001"` and ``r paste0('"', current_congress(), '"')``,
#' or `"all"` if no Congress is specified.
#'
#' @examples
#' match_congress(118)
#' match_congress(1)
#'
#' match_congress(NULL)
#' match_congress(300)
#' match_congress("not a valid number")
#'
#' @noRd
match_congress <- function(congress = NULL, call = rlang::caller_env()) {
  if (length(congress) > 1) {
    return(sapply(congress,
                  function(.x) match_congress(congress = .x, call = call)))
  }

  # default: all
  if (is.null(congress)) {
    return("all")
  }

  # error for invalid `congress`
  if (!is.numeric(congress) ||
      !all(congress %in% 1:current_congress())) {
    cli::cli_abort(c(
      "Invalid {.arg congress} argument ({.val {congress}}) provided.",
      "i" = "{.arg congress} must be a whole number between {.val {1}} and {.val {current_congress()}}."
    ),
    call = call)
  }

  # valid Congress numbers: pad with zeros to 3 characters
  stringr::str_pad(string = as.integer(congress),
                   width = 3, side = "left", pad = 0)
}
