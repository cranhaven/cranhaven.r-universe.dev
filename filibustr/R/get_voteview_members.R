#' Get data on members of Congress from Voteview
#'
#' `get_voteview_members()` returns a tibble of data on members of Congress,
#' sourced from [Voteview](https://voteview.com/data). Members in the data include
#' Senators, Representatives, and Presidents. Each row is one member in one
#' Congress (i.e., each member is listed once for every two years in office).
#'
#' @param chamber (Optional) Which chamber to get data for. Options are:
#'  * `"all"`, `"congress"`, `"hs"`: Both House and Senate data (the default).
#'  * `"house"`, `"h"`, `"hr"`: House data only.
#'  * `"senate"`, `"s"`, `"sen"`: Senate data only.
#'
#'  These options are case-insensitive. If you explicitly pass a different value,
#'  it will default to "all" with a warning.
#'
#'  Note that presidents are included in all datasets. Therefore, reading *both* `"house"`
#'  and `"senate"` data will duplicate data on the presidents. The recommended way to get
#'  all data is to use the default argument, `"all"`.
#'
#' @param congress (Optional) A whole number (to get data for a single Congress), or
#'  a numeric vector (to get data for a set of congresses).
#'
#'  If not provided, will retrieve data for all Congresses by default.
#'  If specified, Congress numbers cannot be greater than the [current_congress()]
#'  (i.e., you cannot try to get future data).
#'
#' @param local_path (Optional) A file path for reading from a local file. If no
#'  `local_path` is specified, will read data from the Voteview website.
#'
#' @returns A tibble.
#'
#' The tibble includes data on the member's office, party, and ideology.
#' See [Voteview](https://voteview.com/data) for descriptions of specific columns.
#'
#' @section Parallel downloads with \CRANpkg{mirai}:
#' If you have installed the packages \CRANpkg{mirai} and \CRANpkg{carrier}, then the Voteview
#' functions can download Voteview data from multiple Congresses in parallel.
#'
#' To download Voteview data in parallel, use [mirai::daemons()] to create
#' parallel processes. If you are downloading Voteview data for many
#' Congresses, this can provide a significant speed-up.
#'
#' See `vignette("parallel-downloads")` for full usage details.
#'
#' @details
#' See the
#' [Voteview](https://voteview.com/data) website for more information on their data.
#'
#' Please cite this dataset as:
#'
#' Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin,
#' and Luke Sonnet (`r format(Sys.Date(), "%Y")`).
#' *Voteview: Congressional Roll-Call Votes Database*. <https://voteview.com/>
#'
#' @export
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' get_voteview_members()
#'
#' # Get data for only one chamber
#' # NOTE: the President is included in all data
#' get_voteview_members(chamber = "house")
#' get_voteview_members(chamber = "senate")
#'
#' @examplesIf !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # Get data for a specific Congress
#' get_voteview_members(congress = 100)
#' get_voteview_members(congress = current_congress())
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # Get data for a set of Congresses
#' get_voteview_members(congress = 1:10)
#'
get_voteview_members <- function(chamber = "all", congress = NULL, local_path = NULL) {
  # join multiple congresses (for online downloads)
  if (length(congress) > 1 && is.numeric(congress) && is.null(local_path)) {
    return(
      multi_congress_read(fun = get_voteview_members,
                          chamber = chamber,
                          congress = congress)
    )
  }

  if (is.null(local_path)) {
    # online reading
    url <- build_url(data_source = "voteview", chamber = chamber, congress = congress,
                     sheet_type = "members")
    online_file <- get_online_data(url = url, source_name = "Voteview")
    df <- readr::read_csv(online_file, col_types = "ifiinfiiiccnnnnnniilnn")
  } else {
    # local reading
    df <- read_local_file(path = local_path, col_types = "ifiinfiiiccnnnnnniilnn")
  }

  if (isTRUE(tools::file_ext(local_path) == "dta")) {
    # fixes for coming from .dta files
    df <- df |>
      # no need to specify levels if data is already coming from saved DTA file
      dplyr::mutate(dplyr::across(.cols = c("chamber", "state_abbrev"),
                                  .fns = haven::as_factor),
                    dplyr::across(.cols = "conditional",
                                  .fns = as.logical),
                    dplyr::across(.cols = "bioguide_id",
                                  .fns = ~ dplyr::na_if(.x, "")))
  } else {
    # fix order of state abbreviations
    df <- df |>
      dplyr::mutate(dplyr::across(.cols = "state_abbrev",
                                  .fns = ~ factor(.x, levels = c(datasets::state.abb, "USA"))))
  }

  if (!is.null(local_path)) {
    df <- df |>
      filter_congress(congress = congress) |>
      filter_chamber(chamber = chamber)
  }

  df <- df |>
    dplyr::mutate(dplyr::across(.cols = c("district_code", "born", "died"),
                                .fns = as.integer))

  df
}
