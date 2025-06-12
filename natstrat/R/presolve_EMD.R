#' Solve the earthmover's distance problem
#'
#' Determine how many controls should be chosen from each stratum to minimize
#' the distance between the strata of the chosen controls and those that were desired.
#' Used within \code{\link{generate_qs}()}.
#'
#' @param S the total number of strata.
#' @param desired_qs a named vector containing the number of controls desired in
#' each stratum with names matching the strata names.
#' @param max_s a vector containing the maximum number of controls that should
#' be selected in each stratum. The order of the strata should match that of \code{desired_qs}.
#' @param strata_dist_flat a flattened distance matrix between the strata.
#'
#' @return A named vector with names identical to those of \code{desired_qs} and
#' elements containing the number of controls to select from the given stratum.
#'
#' @keywords internal

presolve_EMD <- function(S, desired_qs, max_s, strata_dist_flat) {
  # Set up and solve the linear program
  model <- list()
  model$obj <- strata_dist_flat

  # Constrain the flow out of strata to be less than or equal to the max desired/available
  flow_out <- t(sapply(1:S, function(a) {
    b <- rep(0, S^2)
    b[((a-1) * S + 1):(S * a)] <- 1
    return(b)
  }))
  model$A <- flow_out
  model$sense <- c(rep("<=", S))
  model$rhs <- max_s

  # Now constrain the flow into strata to be the desired amount
  flow_in <- t(sapply(1:S, function(a) {
    b <- rep(0, S^2)
    b[seq(a, S^2, by = S)] <- 1
    return(b)
  }))
  model$A <- rbind(model$A, flow_in)
  model$sense <- c(model$sense, rep("==", S))
  model$rhs <- c(model$rhs, desired_qs)

  o <- Rglpk::Rglpk_solve_LP(model$obj, model$A, model$sense, model$rhs)

  warnings()
  if (o$status != 0) {
    stop("No solution found for the linear program for earth-movers distance.",
         call. = FALSE)
  }

  q_s <- sapply(1:S, function(x) {sum(o$solution[((x - 1) * S + 1):(x * S)])})
  names(q_s) <- names(desired_qs)
  return(q_s)
}
