#' @keywords internal
"_PACKAGE"

#' couplr: Optimal Pairing and Matching via Linear Assignment
#'
#' Solves optimal pairing and matching problems using linear assignment algorithms.
#' Designed for matching plots, sites, samples, or any pairwise optimization problem.
#' Provides modern, tidy implementations of 'Hungarian', 'Jonker-Volgenant', 'Auction',
#' and other LAP solvers.
#'
#' @section Main functions:
#' \itemize{
#'   \item{\code{\link{lap_solve}}}: Solve single assignment problems
#'   \item{\code{\link{lap_solve_batch}}}: Solve multiple problems efficiently
#'   \item{\code{\link{lap_solve_kbest}}}: Find k-best optimal solutions
#' }
#'
#' @name couplr-package
#' @aliases couplr
#' @useDynLib couplr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr group_by group_vars group_keys group_split bind_rows bind_cols summarise first n arrange
#' @importFrom purrr map_dfr
#' @importFrom stats dist median quantile sd
#' @importFrom utils type.convert
NULL

#' Re-export of dplyr::group_by
#'
#' @name group_by
#' @rdname group_by
#' @return See \code{\link[dplyr]{group_by}}.
#' @keywords internal
#' @export
#' @importFrom dplyr group_by
NULL

