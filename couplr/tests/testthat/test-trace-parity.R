# ==============================================================================
# Parametric parity tests for every registered animation trace
# ==============================================================================
# For each method returned by animated_methods(), check that:
#
#   - trace runs end-to-end on a battery of small cost matrices
#   - every frame's matching is a valid partial state
#   - no frame uses forbidden (NA/Inf) edges
#   - meta$total_cost matches the C++ production oracle within tolerance
#
# This file grows automatically as new trace_*.R files register more methods:
# nothing here mentions method names. If you add a new trace, it gets tested.
# ==============================================================================

# A small battery of cost matrices covering the cases each trace must handle.
# Kept small so the per-method test budget stays reasonable when many methods
# are registered.
.parity_test_matrices <- function() {
  set.seed(2026)
  list(
    list(name = "3x3 small integer",
         cost = matrix(sample.int(20, 9), nrow = 3)),
    list(name = "4x4 integer with ties",
         cost = matrix(sample.int(5, 16, replace = TRUE), nrow = 4)),
    list(name = "6x6 random integer",
         cost = matrix(sample.int(100, 36), nrow = 6)),
    list(name = "5x5 with forbidden edges",
         cost = local({
           m <- matrix(sample.int(50, 25), nrow = 5)
           m[1, 2] <- NA_real_
           m[3, 4] <- Inf
           m[5, 1] <- NA_real_
           m
         })),
    list(name = "4x4 doubles",
         cost = matrix(runif(16) * 10, nrow = 4))
  )
}

# Methods that don't accept arbitrary cost matrices. hk01 in particular only
# makes sense on 0/1 matrices and on the standard battery would be a no-op.
.parity_method_matrices <- function(method) {
  base <- .parity_test_matrices()
  if (identical(method, "hk01")) {
    # hk01 needs a perfect matching to exist in the *preferred*-cost subgraph
    # (0-cost edges for min, 1-cost edges for max). Use balanced fixtures so
    # both directions have enough edges of each class. The 4x4 all-equal case
    # exercises the all-equal palette branch.
    set.seed(2027)
    return(list(
      list(name = "4x4 all equal",
           cost = matrix(0, 4, 4)),
      list(name = "4x4 binary balanced",
           cost = matrix(c(0,1,0,1, 1,0,1,0, 0,1,0,1, 1,0,1,0), 4, 4)),
      list(name = "5x5 binary mixed",
           cost = matrix(c(0,1,1,0,1, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 1,0,1,1,0), 5, 5))
    ))
  }
  if (identical(method, "bruteforce")) {
    return(base[1:3])
  }
  if (identical(method, "ssap_bucket")) {
    # ssap_bucket is exact only for costs of bounded decimal precision (it
    # scales to integers); full-precision reals are refused by contract.
    # Round the doubles case to two decimals so it still
    # exercises fractional scaling within the supported domain.
    for (i in seq_along(base)) {
      if (identical(base[[i]]$name, "4x4 doubles")) {
        base[[i]]$cost <- round(base[[i]]$cost, 2)
      }
    }
    return(base)
  }
  base
}

# Known production-C++ bugs surfaced by the parity test. Each entry is a
# (method, case_name, maximize) tuple where the C++ oracle disagrees with the
# trace because the C++ is buggy, not because the trace is wrong. The trace
# itself produces the true optimum and a valid matching at every frame; the
# parity validator just can't agree with a sub-optimal oracle.
#
# When a C++ bug is fixed, remove the entry here so the test starts enforcing
# parity again on that case.
.parity_known_oracle_bugs <- list()

.is_known_bug <- function(method, case_name, maximize) {
  for (b in .parity_known_oracle_bugs) {
    if (identical(b$method, method) &&
        identical(b$case_name, case_name) &&
        identical(b$maximize, maximize)) {
      return(b$note)
    }
  }
  NULL
}

test_that("every registered trace passes full per-frame parity checks", {
  skip_on_cran()
  methods <- animated_methods()
  expect_gt(length(methods), 0)

  for (method in methods) {
    mats <- .parity_method_matrices(method)
    for (case in mats) {
      label <- sprintf("method = %s, case = %s", method, case$name)

      trace_fn <- get_trace_fn(method)

      skip_min <- .is_known_bug(method, case$name, FALSE)
      if (is.null(skip_min)) {
        ok_min <- tryCatch({
          trace <- trace_fn(case$cost, maximize = FALSE)
          validate_trace_parity(trace, case$cost, maximize = FALSE, method = method)
          TRUE
        }, error = function(e) {
          fail(sprintf("[min] %s: %s", label, conditionMessage(e)))
          FALSE
        })
        if (!ok_min) next
      }

      mat_max <- case$cost
      if (!any(is.finite(mat_max))) next

      skip_max <- .is_known_bug(method, case$name, TRUE)
      if (is.null(skip_max)) {
        tryCatch({
          trace <- trace_fn(mat_max, maximize = TRUE)
          validate_trace_parity(trace, mat_max, maximize = TRUE, method = method)
        }, error = function(e) {
          fail(sprintf("[max] %s: %s", label, conditionMessage(e)))
        })
      }
    }
  }
  succeed()
})
