#' Key to Voteview cast codes in individual member votes
#'
#' `get_voteview_cast_codes()` returns a tibble with definitions
#' of the 10 cast codes used in Voteview's member votes data
#' (i.e., the `cast_code` column in the data frames from
#' [get_voteview_member_votes()]).
#'
#' For more information on these cast codes, visit Voteview's
#' [article](https://voteview.com/articles/data_help_votes) on the
#' member votes data.
#'
#' @seealso [get_voteview_member_votes()], which uses these cast codes.
#'
#' @returns A tibble.
#' @export
#'
#' @examples
#' get_voteview_cast_codes()
get_voteview_cast_codes <- function() {
  dplyr::tibble(cast_code = 0:9,
         vote_cast = factor(
           c("Not a Member",
             "Yea", "Paired Yea", "Announced Yea",
             "Announced Nay", "Paired Nay", "Nay",
             "Present", "Present", "Not Voting")
         ))
}
