#' Linear program that selects which controls to use in order to optimize balance
#'
#' This linear program is used by \code{\link{optimize_controls}()} to choose which controls
#' to select.
#'
#' @inheritParams optimize_controls
#' @param q_s a named vector or matrix indicating how many control units are to be selected from each stratum.
#'   If there is one control group and all treated units are desired, this can be a vector; otherwise,
#'   this should have one row per treatment group, where the order of the rows matches the order of
#'   the levels of \code{z}, including the treated level.
#' @param st_vals the unique stratum levels contained in \code{st}.
#' @param S the number of unique stratum levels contained in \code{st}.
#' @param N the total number of available controls in the data.
#'
#' @return A list containing two elements:
#' \describe{
#' \item{\code{lpdetails}}{The output of either \code{gurobi()} or \code{\link[Rglpk]{Rglpk_solve_LP}()},
#' except that if \code{gurobi()} is used, the elements \code{objval} and \code{x}
#' are renamed \code{optimum} and \code{solution}
#' to be consistent with the output of \code{\link[Rglpk]{Rglpk_solve_LP}()}.}
#' \item{\code{o}}{The original output of either \code{gurobi()} or \code{\link[Rglpk]{Rglpk_solve_LP}()}.}
#' }
#'
#' @keywords internal
#' @import ramify
#' @import slam

balance_LP <- function(z, X, importances, st, st_vals, S, q_s, N,
                       solver, integer, time_limit, threads = 1,
                       weight_comp = 1) {

  if (solver == "gurobi" && !requireNamespace("gurobi", quietly = TRUE)) {
    stop("Package \'gurobi\' needed if \"solver\" parameter set to \"gurobi\". Please
         install it or switch the \"solver\" parameter to \"Rglpk\".",
         call. = FALSE)
  }
  groups <- levels(z)
  k <- length(groups)
  kc2 <- choose(k, 2)
  n_comp <- length(q_s)

  # Set up and solve the linear program
  model <- list()
  params <- list(TimeLimit = time_limit, OutputFlag = 0, Threads = threads)

  nvars <- dim(X)[2]  # number of variables
  model$obj <- rep(0, n_comp * N)
  for (comp in 1:n_comp) {
    model$obj <- c(model$obj, rep(rep(importances * weight_comp[comp], 2), kc2))
  }
  model$A <- create_balance_matrices(X = X, z = z, N = N, nvars = nvars,
                          kc2 = kc2, q_s = q_s, return = "A")$A

    # Now, append stratum size constraints for each comparison
  st_mats <- simple_triplet_zero_matrix(nrow = k * S, ncol = N)
  for (group_num in 1:k) {
    group <- groups[group_num]
    st_mats[((group_num - 1) * S + 1):(group_num * S), which(z == group)] <- 1 * outer(st_vals, st[z == group], "==")
  }
  for (comp in 1:n_comp) {
    model$A <- rbind(model$A,
                     cbind(simple_triplet_zero_matrix(nrow = k * S, ncol = (N * (comp - 1))),
                           st_mats, simple_triplet_zero_matrix(nrow = k * S, ncol = N * (n_comp - comp) + 2 * n_comp * kc2 * nvars)))
  }

  # Now, if multiple comparisons, add constraint that all a's for a unit add to <= 1
  # (so that one unit is not chosen for multiple comparisons)
  if (n_comp > 1) {
    mat <- do.call(cbind, replicate(n_comp, simple_triplet_diag_matrix(rep(1, N)), simplify=FALSE))
    model$A <- rbind(model$A, cbind(mat, simple_triplet_zero_matrix(nrow = N, ncol = 2 * n_comp * kc2 * nvars)))
  }

  # Constraints for eps are equalities, number of controls per strata are equalities
  # Constraints for units only counting in one comparison are <=
  model$sense <- c(rep("==", n_comp * kc2 * nvars), rep("==", n_comp * k * S))
  if (n_comp > 1) {
    model$sense <- c(model$sense, rep("<=", N))
  }

  # right hand side of constraints
  model$rhs <- rep(0, n_comp * kc2 * nvars)
  for (comp in 1:n_comp) {
    model$rhs <- c(model$rhs, ramify::flatten(q_s[[comp]]))
  }
  if (n_comp > 1) {
    model$rhs <- c(model$rhs, rep(1, N))
  }

  ndecv <- as.integer(n_comp * N + (2 * n_comp * kc2 * nvars))  # number of decision variables
  model$ub <- c(rep(1, n_comp * N), rep(Inf, 2 * n_comp * kc2 * nvars))
  model$lb <- rep(0, ndecv)
  bounds <- list(lower = list(ind = 1:ndecv, val = model$lb),
                 upper = list(ind = 1:ndecv, val = model$ub))
  if (integer) {
    model$vtype <- c(rep("B", n_comp * N), rep("C", 2 * n_comp * kc2 * nvars))
  } else {
    model$vtype <- rep("C", ndecv)
  }

  if (solver == "Rglpk") {
    if (params$TimeLimit < Inf) {
      params$TimeLimit <- params$TimeLimit * 1000
    } else {
      params$TimeLimit <- 0
    }
    o <- Rglpk::Rglpk_solve_LP(obj = model$obj, mat = model$A, dir = model$sense,
                               rhs = model$rhs, bounds = bounds,
                               types = model$vtype, control = list(
                                 canonicalize_status = FALSE, tm_limit = params$TimeLimit))
    if (o$status != 5) {
      warning("No optimal solution found for the linear program.")
      return(NULL)
    }
    lpdetails <- o
  }
  if (solver == "gurobi") {
    # Note that for gurobi, all inequalities are interpreted to be "or equal to"
    model$sense <- c(rep("=", n_comp * kc2 * nvars), rep("=", n_comp * k * S),
                     rep("<", N))
    o <- gurobi::gurobi(model, params)
    if (o$status != "OPTIMAL") {
      warning("No solution found for the linear program.")
      return(NULL)
    }
    lpdetails <- o
    names(o)[c(6,7)] <- c("optimum", "solution")
  }

  return(list(lpdetails = lpdetails, o = o))
}
