# tests/testthat/test-auction-hang.R
testthat::test_that("Auction finishes quickly (<1s) and without errors", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("clue")

  # --------------------------------------------------------------------
  # Portable timeout helper
  # --------------------------------------------------------------------
  run_with_timeout <- function(expr, timeout = 1) {
    if ("with_timeout" %in% getNamespaceExports("testthat")) {
      return(testthat::with_timeout(timeout, force(expr)))
    }
    if (requireNamespace("R.utils", quietly = TRUE)) {
      return(R.utils::withTimeout(force(expr), timeout = timeout, onTimeout = "error"))
    }
    on.exit(try(setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE), silent = TRUE), add = TRUE)
    setTimeLimit(elapsed = timeout, transient = TRUE)
    force(expr)
  }

  # --------------------------------------------------------------------
  # Synthetic problem (dense, near-flat)
  # --------------------------------------------------------------------
  set.seed(1)
  n <- 165L
  C <- matrix(runif(n * n, 0, 2e-3), n, n)
  band <- seq_len(floor(0.2 * n))
  C[band, ] <- 0.116 + runif(length(band) * n, 0, 2e-3)
  C <- C - min(C)
  C <- C / max(1e-12, max(C))

  # Sanity check: Hungarian result
  res_h <- lap_solve(C, method = "hungarian")
  testthat::expect_s3_class(res_h, "lap_solve_result")
  testthat::expect_equal(nrow(res_h), n)
  testthat::expect_equal(length(unique(res_h$target)), n)

  # --------------------------------------------------------------------
  # Guarded run (upper bound)
  # --------------------------------------------------------------------
  call_auction <- function() lap_solve(C, method = "auction")
  testthat::expect_error(
    run_with_timeout(call_auction(), timeout = 1),
    NA
  )

  # --------------------------------------------------------------------
  # Explicit timing + print + strict check
  # --------------------------------------------------------------------
  timing <- system.time(result <- lap_solve(C, method = "auction"))
  elapsed <- unname(timing["elapsed"])
  message(sprintf("Auction finished in %.3f seconds", elapsed))

  if (!(elapsed < 1.0)) {
    testthat::fail(sprintf("Auction took %.3f s, expected < 1.0 s", elapsed))
  }
})
