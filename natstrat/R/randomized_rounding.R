#' Sample integer solution from linear programming solution with correct sample sizes
#'
#' The linear programming solution of \code{\link{balance_LP}()} that is used
#' within \code{\link{optimize_controls}()} sometimes selects fractional units.
#' Here, we select any unit the linear programming solution chose with coefficient 1.
#' Then, we select the remaining required number of units from those that have
#' fractional solutions by sampling with probabilities equal to the linear
#' programming solution and fixed sample size. Used within \code{\link{optimize_controls}()}
#' if \code{correct_sizes = TRUE}.
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

randomized_rounding <- function(o, N, st, st_vals, S, z) {

  pr <- o$solution[1:N]
  pr <- round(pr, 5)
  select <- rep(FALSE, N)
  select[pr == 1] <- TRUE
  ind <- 1:N
  for (group in levels(z)) {
    for (j in 1:S) {
      w <- (pr < 1) & (pr > 0) & (st == st_vals[j]) & (z == group)
      needed <- sum(pr[pr != 1 & (st == st_vals[j]) & (z == group)])  # Sample how many? Total weight of non-1 entries
      needed <- round(needed, 3)
      if (needed > 0) {
        if (length(ind[w]) == 1) {
          sa <- ind[w]
        } else if (needed == 1) {
          sa <- sample(ind[w], needed, replace = FALSE, prob = pr[w])
        } else {
          if (sum(w) < 100) {
            sampled <- pps::sampford(pr[w], needed)
          } else {
            sampled <- sampling::UPmidzuno(pr[w])
            sampled <- sampled == 1
          }
          sa <- (ind[w])[sampled]
        }
        select[sa] <- TRUE
      }
    }
  }
  units <- data.frame(pr, select)

  return(units)
}
