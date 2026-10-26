# Shared fixtures for the certification harness.
#
# The rest of the suite hardcodes its method vectors inline; c("jv","hungarian",
# "auction") appears verbatim in six files, and the widest vector anywhere names
# 17 of the 20 solvers. A registry means adding a solver adds a row here and
# every certification test picks it up.

# Tolerances used across the certification tests, so a change is one edit.
CERT_TOL      <- 1e-9
CERT_COST_TOL <- 1e-8

# Every method `assignment()` can dispatch to, with the inputs it accepts.
# `accepts` decides whether a generated problem is valid for that solver, so a
# solver with a restricted domain is skipped rather than silently expected to
# handle something it does not claim to.
cert_solver_registry <- function() {
  list(
    list(method = "jv",             accepts = function(n, m, kind) TRUE),
    list(method = "hungarian",      accepts = function(n, m, kind) TRUE),
    list(method = "munkres",        accepts = function(n, m, kind) TRUE),
    list(method = "auction",        accepts = function(n, m, kind) TRUE),
    list(method = "auction_gs",     accepts = function(n, m, kind) TRUE),
    list(method = "auction_scaled", accepts = function(n, m, kind) TRUE),
    list(method = "sap",            accepts = function(n, m, kind) TRUE),
    list(method = "csflow",         accepts = function(n, m, kind) TRUE),
    list(method = "lapmod",         accepts = function(n, m, kind) TRUE),
    list(method = "csa",            accepts = function(n, m, kind) TRUE),
    list(method = "cycle_cancel",   accepts = function(n, m, kind) TRUE),
    list(method = "gabow_tarjan",   accepts = function(n, m, kind) TRUE),
    list(method = "ramshaw_tarjan", accepts = function(n, m, kind) TRUE),
    list(method = "push_relabel",   accepts = function(n, m, kind) TRUE),
    list(method = "sap_dense",          accepts = function(n, m, kind) TRUE),
    list(method = "network_simplex", accepts = function(n, m, kind) TRUE),
    list(method = "ssap_bucket",    accepts = function(n, m, kind) kind == "integer"),
    list(method = "hk01",           accepts = function(n, m, kind) kind == "binary"),
    list(method = "bruteforce",     accepts = function(n, m, kind) n <= 8 && m <= 8)
  )
}

cert_methods <- function() {
  vapply(cert_solver_registry(), function(s) s$method, character(1))
}

cert_methods_for <- function(n, m, kind) {
  reg <- cert_solver_registry()
  keep <- vapply(reg, function(s) isTRUE(s$accepts(n, m, kind)), logical(1))
  vapply(reg[keep], function(s) s$method, character(1))
}

# One generator for the whole harness. `kind` names the cost structure, which is
# what decides solver applicability; `sparsity` is the fraction of forbidden
# entries. Every row keeps at least one admissible column, so a generated
# problem is always feasible unless `allow_infeasible` asks otherwise.
cert_problem <- function(n, m, kind = "continuous", sparsity = 0,
                         allow_infeasible = FALSE) {
  stopifnot(n <= m || allow_infeasible)
  cost <- switch(
    kind,
    continuous = matrix(stats::runif(n * m), n, m),
    integer    = matrix(sample.int(50L, n * m, replace = TRUE), n, m),
    binary     = matrix(sample(0:1, n * m, replace = TRUE), n, m),
    constant   = matrix(3, n, m),
    negative   = matrix(stats::runif(n * m, -5, 5), n, m),
    stop("unknown cost kind: ", kind)
  )
  storage.mode(cost) <- "double"

  if (sparsity > 0) {
    n_forbid <- floor(sparsity * n * m)
    if (n_forbid > 0) {
      cost[sample.int(n * m, n_forbid)] <- NA_real_
    }
    if (!allow_infeasible) {
      # Restore one admissible entry per row and per column, so infeasibility
      # is something a test asks for rather than something sparsity produces by
      # accident.
      for (i in seq_len(n)) {
        if (all(is.na(cost[i, ]))) cost[i, sample.int(m, 1L)] <- stats::runif(1)
      }
    }
  }
  cost
}

# Solver and shape combinations that are known to return a suboptimal answer,
# each with the issue tracking it. The harness both excludes these from the
# "everything certifies" sweep and asserts separately that they still fail, so
# an entry has to be deleted when the bug is fixed instead of quietly masking it.
cert_known_suboptimal <- function() {
  list()
}

cert_shape <- function(n, m) if (m > n) "wide" else "square"

cert_is_known_suboptimal <- function(method, n, m) {
  shape <- cert_shape(n, m)
  any(vapply(cert_known_suboptimal(),
             function(k) identical(k$method, method) && identical(k$shape, shape),
             logical(1)))
}

# Solve `cost` with `method`, returning NULL when the solver declines the
# problem, so a harness can distinguish "refused" from "answered wrongly".
cert_try_solve <- function(cost, method, ...) {
  tryCatch(assignment(cost, method = method, ...), error = function(e) NULL)
}
