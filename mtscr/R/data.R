#' Creativity assessment through semantic distance dataset
#'
#' A dataset from Forthmann, Karwowski & Beaty (2023) paper.
#' It contains a set of responses in Alternative Uses Task for different items with their
#' semantic distance assessment.
#'
#' @return a [tibble][tibble::tibble-package]
#' @format ## `mtscr_creativity`
#' A `tibble` with 4585 rows and 10 columns:
#' \describe{
#'   \item{id}{patricipant's unique identification number}
#'   \item{response}{response in AUT}
#'   \item{item}{item for which alternative uses were searched for}
#'   \item{SemDis_MEAN}{mean semantic distance}
#' }
#'
#' @source <https://osf.io/7rgsp/>
#' @references \doi{10.1037/aca0000571}
"mtscr_creativity"

#' @rdname mtscr_self_rank
#' @title Self-chosen best answers
#' @description
#' An example dataset with best answers self-chosen by the participant. Use with `self_ranking`
#' argument in [mtscr_model][mtscr::mtscr_model].
#'
#' @format ## `mtscr_self_rank`
#' A tibble with 3225 rows and 4 columns:
#' \describe{
#'   \item{subject}{patricipant's unique identification number}
#'   \item{task}{divergent thinking task number}
#'   \item{avr}{average judges' raiting}
#'   \item{top_two}{indicator of self-chosen two best answer; 1 if chosen, 0 if not}
#' }
#'
#' @source <https://osf.io/7rgsp/>
#' @references \doi{10.1037/aca0000571}
"mtscr_self_rank"
