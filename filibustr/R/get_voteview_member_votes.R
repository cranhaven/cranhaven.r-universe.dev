#' Get data on the votes of individual members of Congress
#'
#' `get_voteview_member_votes()` returns a tibble that lists how each member
#' of Congress voted in recorded (roll call) votes in the House and Senate.
#' Members are identified by their ICPSR ID number, which you can use to
#' join with additional member data from [get_voteview_members()].
#'
#' @inheritParams get_voteview_rollcall_votes
#' @inherit get_voteview_members details
#'
#' @returns A [tibble()].
#' @export
#'
#' @examplesIf interactive()
#' get_voteview_member_votes()
#'
#' # Force to get data from Voteview website
#' get_voteview_member_votes(local = FALSE)
#'
#' # Get data for only one chamber
#' get_voteview_member_votes(chamber = "house")
#' get_voteview_member_votes(chamber = "senate")
#'
#' # Get data for a specific Congress
#' get_voteview_member_votes(congress = 110)
#' get_voteview_member_votes(congress = current_congress())
#'
#' @examples
#' # Get data for a set of Congresses
#' get_voteview_member_votes(congress = 1:3)
#'
get_voteview_member_votes <- function(chamber = "all", congress = NULL,
                                      local = TRUE, local_dir = ".") {
  # join multiple congresses
  if (length(congress) > 1 & is.numeric(congress)) {
    list_of_dfs <- lapply(congress,
                          function(.cong) get_voteview_member_votes(local = local,
                                                                    local_dir = local_dir,
                                                                    chamber = chamber,
                                                                    congress = .cong))
    return(dplyr::bind_rows(list_of_dfs))
  }

  full_path <- build_file_path(data_source = "voteview", chamber = chamber, congress = congress,
                               sheet_type = "votes", local = local, local_dir = local_dir)

  # TODO: error handling with `get_online_data()`

  readr::read_csv(full_path, col_types = "ifiddd", na = c("", "N/A")) |>
    dplyr::mutate(dplyr::across(.cols = c("icpsr", "cast_code"),
                                .fns = as.integer))
}
