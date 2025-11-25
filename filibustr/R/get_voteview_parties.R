#' Get data on congressional parties from Voteview
#'
#' @description
#' `get_voteview_parties()` returns a tibble with information on the parties (aka caucuses)
#' in each Congress. The party information includes a party's ICPSR code, number of
#' members, and DW-NOMINATE scores.
#'
#' The parties of the President, Senate, and House are listed in the data.
#' Each row is one party in one chamber for each Congress (i.e., each party is listed
#' once for every two years).
#'
#' @inherit get_voteview_members
#'
#' @returns A tibble.
#' @export
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' get_voteview_parties()
#'
#' # get parties for only one chamber
#' # NOTE: the President is included in all data
#' get_voteview_parties(chamber = "house")
#' get_voteview_parties(chamber = "senate")
#'
#' @examplesIf !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # get parties for a specific Congress
#' get_voteview_parties(congress = 100)
#' get_voteview_parties(congress = current_congress())
#'
#' @examplesIf interactive() && !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # get parties for a set of Congresses
#' get_voteview_parties(congress = 1:10)
#'
get_voteview_parties <- function(chamber = "all", congress = NULL, local_path = NULL) {
  # join multiple congresses (for online downloads)
  if (length(congress) > 1 && is.numeric(congress) && is.null(local_path)) {
    return(
      multi_congress_read(fun = get_voteview_parties,
                          chamber = chamber,
                          congress = congress)
    )
  }

  if (is.null(local_path)) {
    # online reading
    url <- build_url(data_source = "voteview", chamber = chamber, congress = congress,
                     sheet_type = "parties")
    online_file <- get_online_data(url = url, source_name = "Voteview")
    df <- readr::read_csv(online_file, col_types = "ififidddd")
  } else {
    # local reading
    df <- read_local_file(path = local_path, col_types = "ififidddd")
  }

  # local file fixes
  if (isTRUE(tools::file_ext(local_path) == "dta")) {
    # fixes for coming from .dta files
    df <- df |>
      # no need to specify levels if data is already coming from saved DTA file
      dplyr::mutate(dplyr::across(.cols = c("chamber", "party_name"),
                                  .fns = haven::as_factor),
                    dplyr::across(.cols = c("congress", "party_code", "n_members"),
                                  .fns = as.integer))
  }

  # filter local files
  if (!is.null(local_path)) {
    df <- df |>
      filter_congress(congress = congress) |>
      filter_chamber(chamber = chamber)
  }

  df
}
