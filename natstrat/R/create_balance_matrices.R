#' Create matrix of balance constraints for linear program
#'
#' This creates the matrix of constraints that seek covariate balance for use in \code{\link{balance_LP}()}
#' which creates the linear program used by \code{\link{optimize_controls}()} to choose which controls
#' to select.
#'
#' @inheritParams optimize_controls
#' @inheritParams balance_LP
#' @param return one of "all", "A", or "X", denoting whether all matrices should be
#' returned, or just A or just the X matrix blocks.
#'
#' @return A list containing up to three elements:
#' \describe{
#' \item{\code{A}}{The matrix of covariate values and +/- 1s that are used as coefficients
#' for the unit indicators and the epsilons in order to set the epsilons equal to
#' the covariate imbalances.}
#' \item{\code{x_blk}}{The covariate values used as coefficients for the unit
#' indicators in the objective function.}
#' }
#'
#' @keywords internal
#' @import slam

# Set up balance constraints
create_balance_matrices <- function(X, z, N, nvars, kc2, q_s, return = "all") {
  n_comp <- length(q_s)
  X[is.na(X)] <- 0
    zero_blk <- simple_triplet_zero_matrix(nrow = nvars, ncol = n_comp * N)
    zero_eps_blk <- simple_triplet_zero_matrix(nrow = nvars, ncol = 2 * n_comp * nvars * kc2)
  A <- NULL
  full_x_blk <- NULL
  pairs <- combn(levels(z), 2)

  for (comp in 1:n_comp) {
    for (pair_num in 1:kc2) {
      group1 <- pairs[1, pair_num]
      group2 <- pairs[2, pair_num]
      Q1 <- sum(q_s[[comp]][levels(z) == group1, ])
      Q2 <- sum(q_s[[comp]][levels(z) == group2, ])
      x_blk <- zero_blk
      eps_blk <- zero_eps_blk
      if (Q1 > 0 & Q2 > 0) {
        x_blk[1:nrow(x_blk), (N * (comp - 1) + which(z == group1))] <- t(X[z == group1, 1:ncol(X)] / Q1)
        x_blk[1:nrow(x_blk), (N * (comp - 1) + which(z == group2))] <- -t(X[z == group2, 1:ncol(X)] / Q2)
      }
      eps_blk[, ((2 * comp - 2) * nvars * kc2 + (pair_num - 1) * nvars + 1):((2 * comp - 2) * nvars * kc2 + pair_num * nvars)] <-
        simple_triplet_diag_matrix(rep(1, nvars))
      eps_blk[, ((2 * comp - 1) * nvars * kc2 + (pair_num - 1) * nvars + 1):((2 * comp - 1) * nvars * kc2 + pair_num * nvars)] <-
        simple_triplet_diag_matrix(rep(-1, nvars))
      new_A <- cbind(x_blk, eps_blk)
      A <- rbind(A, new_A)
      full_x_blk <- rbind(full_x_blk, x_blk)
    }
  }

  if (return == "A") {
    return(list(A = A))
  }
  if (return == "X") {
    return(list(x_blk = full_x_blk))
  } else {
    return(list(A = A, x_blk = full_x_blk))
  }
}
