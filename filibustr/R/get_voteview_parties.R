#' Get ICPSR party codes from Voteview
#'
#' `get_voteview_parties()` returns a tibble associating ICPSR party codes with more
#' information on the party.
#' The parties of the President, Senate, and House are listed in the data.
#' Each row is one party in one chamber for each Congress (i.e., each party is listed
#' once for every two years).
#'
#' @inheritParams get_voteview_members
#' @inherit get_voteview_members details
#'
#' @returns A [tibble()].
#' @export
#'
#' @examplesIf interactive()
#' get_voteview_parties()
#'
#' # Force to get data from Voteview website
#' get_voteview_parties(local = FALSE)
#'
#' # get parties for only one chamber
#' # NOTE: the President is included in all data
#' get_voteview_parties(chamber = "house")
#' get_voteview_parties(chamber = "senate")
#'
#' @examples
#' # get parties for a specific Congress
#' get_voteview_parties(congress = 100)
#' get_voteview_parties(congress = current_congress())
#'
#' @examplesIf interactive()
#' # get parties for a set of Congresses
#' get_voteview_parties(congress = 1:10)
#'
get_voteview_parties <- function(chamber = "all", congress = NULL, local = TRUE, local_dir = ".") {
  # join multiple congresses
  if (length(congress) > 1 & is.numeric(congress)) {
    list_of_dfs <- lapply(congress, function(.cong) get_voteview_parties(local = local,
                                                                         local_dir = local_dir,
                                                                         chamber = chamber,
                                                                         congress = .cong))
    return(dplyr::bind_rows(list_of_dfs))
  }

  full_path <- build_file_path(data_source = "voteview", chamber = chamber, congress = congress,
                               sheet_type = "parties", local = local, local_dir = local_dir)

  # request data from online
  if (R.utils::isUrl(full_path)) {
    full_path <- get_online_data(url = full_path, source_name = "Voteview")
  }

  readr::read_csv(full_path, col_types = "ififidddd")
}
