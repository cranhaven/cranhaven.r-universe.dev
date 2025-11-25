#' Get data on congressional roll call votes from Voteview
#'
#' `get_voteview_rollcall_votes()` returns a tibble with information on recorded
#' (roll call) votes in the House and Senate.
#'
#' @inherit get_voteview_members
#'
#' @param chamber Which chamber to get data for. Options are:
#'  * `"all"`, `"congress"`, `"hs"`: Both House and Senate data (the default).
#'  * `"house"`, `"h"`, `"hr"`: House data only.
#'  * `"senate"`, `"s"`, `"sen"`: Senate data only.
#'
#'  These options are case-insensitive. If you explicitly pass a different value,
#'  it will default to "all" with a warning.
#'
#' @returns A tibble.
#' @export
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' get_voteview_rollcall_votes()
#'
#' # Get data for only one chamber
#' # NOTE: the President is included in all data
#' get_voteview_rollcall_votes(chamber = "house")
#' get_voteview_rollcall_votes(chamber = "senate")
#'
#' @examplesIf !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # Get data for a specific Congress
#' get_voteview_rollcall_votes(congress = 100)
#' get_voteview_rollcall_votes(congress = current_congress())
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # Get data for a set of Congresses
#' get_voteview_rollcall_votes(congress = 1:10)
#'
get_voteview_rollcall_votes <- function(chamber = "all", congress = NULL, local_path = NULL) {
  # join multiple congresses (for online downloads)
  if (length(congress) > 1 && is.numeric(congress) && is.null(local_path)) {
    return(
      multi_congress_read(fun = get_voteview_rollcall_votes,
                          chamber = chamber,
                          congress = congress)
    )
  }

  if (is.null(local_path)) {
    # online reading
    url <- build_url(data_source = "voteview", chamber = chamber, congress = congress,
                     sheet_type = "rollcalls")
    online_file <- get_online_data(url = url, source_name = "Voteview")
    df <- readr::read_csv(online_file, col_types = "ifiDddiidddddccccc")
  } else {
    # local reading
    df <- read_local_file(path = local_path, col_types = "ifiDddiidddddccccc")
  }

  # fixes for coming from .dta files
  if (isTRUE(tools::file_ext(local_path) == "dta")) {
    df <- df |>
      dplyr::mutate(dplyr::across(.cols = "chamber",
                                  .fns = haven::as_factor),
                    dplyr::across(.cols = c("congress", "rollnumber",
                                            "yea_count", "nay_count"),
                                  .fns = as.integer),
                    # convert empty strings to NAs
                    dplyr::across(.cols = dplyr::where(is.character),
                                  .fns = \(str) dplyr::na_if(str, "")))
  }

  # filter local files
  if (!is.null(local_path)) {
    df <- df |>
      filter_congress(congress = congress) |>
      filter_chamber(chamber = chamber)
  }

  df |>
    dplyr::mutate(dplyr::across(.cols = c("session", "clerk_rollnumber"),
                                .fns = as.integer))
}
