#' Sample integer solution from linear programming solution with sample sizes correct in expectation
#'
#' The linear programming solution of \code{\link{balance_LP}()} that is used
#' within \code{\link{optimize_controls}()} sometimes selects fractional control units.
#' Here, we select any unit the linear programming solution chose with coefficient 1.
#' Then, we select sample each unit with a fractional solution with
#' probability equal to the linear programming solution. The total sample
#' size is then correct in expectation. Used within \code{\link{optimize_controls}()}
#' if \code{correct_sizes = FALSE}.
#'
#' @inheritParams optimize_controls
#' @inheritParams balance_LP
#' @param o linear programming results, as found in the `o` element of the
#' returned list from \code{\link{balance_LP}()}.
#'
#' @return Dataframe with two columns: \code{pr}, which contains
#' the coefficient determined for that unit from the linear programming
#' solution, and \code{select}, a boolean vector stating whether that
#' unit was selected for inclusion by randomized rounding.
#'
#' @keywords internal
#' @importFrom stats rbinom rmultinom

randomized_rounding_expectation <- function(o, N, n_comp) {
  pr <- o$solution[1:(n_comp * N)]
  pr <- round(pr, 5)
  select <- rep(FALSE, length(pr))
  for (i in 1:N) {
    pr_i <- c()
    for (comp in 1:n_comp) {
      pr_i <- c(pr_i, pr[N * (comp - 1) + i])
    }
    draw <- rmultinom(1, 1, c(pr_i, max(round(1 - sum(pr_i), 5), 0)))
    comp_i <- which(draw == 1)
    if (comp_i <= n_comp) {
      select[N * (comp_i - 1) + i] <- TRUE
    }
  }

  units <- data.frame(pr, select)

  return(units = units)
}
