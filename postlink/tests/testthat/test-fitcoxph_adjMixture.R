local_edition(3)

# -------------------------------------------------------------------------
# Helper: Mock Constructor for adjMixture objects
# -------------------------------------------------------------------------
mock_adjMixture <- function(data, m.formula = ~1, m.rate = NULL, safe.matches = NULL) {
 e <- new.env(parent = emptyenv())
 e$data <- data
 structure(
  list(
   data_ref = e,
   m.formula = m.formula,
   m.rate = m.rate,
   safe.matches = safe.matches
  ),
  class = c("adjMixture", "adjustment")
 )
}

# -------------------------------------------------------------------------
# Helper: Mock Engine (coxphMixture)
# -------------------------------------------------------------------------
# We mock the engine as it is not the focus of these test and
# inspect what data actually gets passed to it.
mock_coxph_args <- new.env()

my_mock_coxphMixture <- function(x, y, cens, z, m.rate, safe.matches, control, ...) {
 # Capture arguments for inspection
 mock_coxph_args$x <- x
 mock_coxph_args$y <- y
 mock_coxph_args$cens <- cens
 mock_coxph_args$z <- z
 mock_coxph_args$m.rate <- m.rate
 mock_coxph_args$safe.matches <- safe.matches

 # Return dummy object structure
 structure(
  list(
   coefficients = rep(1, ncol(as.matrix(x))),
   linear.predictors = rep(0, nrow(as.matrix(x))),
   match.prob = rep(0.5, nrow(as.matrix(x)))
  ),
  class = "coxphMixture"
 )
}

# -------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------

test_that("Basic Dispatch: Passes correct data and transforms Surv object", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 # Setup Data
 df <- data.frame(
  id = 1:5,
  time = c(10, 20, 30, 40, 50),
  status = c(1, 0, 1, 0, 1), # 1=Event, 0=Censored
  x1 = c(1, 2, 3, 4, 5),
  z1 = c(0, 1, 0, 1, 0)
 )
 rownames(df) <- paste0("row", 1:5)

 # Create Inputs
 X <- model.matrix(~ x1, data = df)
 Y <- Surv(df$time, df$status)
 adj <- mock_adjMixture(df, m.formula = ~ z1, m.rate = 0.05)

 # Run Function
 fit <- fitcoxph.adjMixture(x = X, y = Y, adjustment = adj, control = list())

 # Assertions
 expect_true(inherits(fit, "coxphMixture"))

 # Check Argument Passing
 expect_equal(mock_coxph_args$x, X)
 expect_equal(mock_coxph_args$m.rate, 0.05)

 # Check Surv Translation (Engine expects: cens 1=censored, 0=event)
 # Input Status: 1, 0, 1, 0, 1
 # Expected Cens: 0, 1, 0, 1, 0
 expect_equal(mock_coxph_args$y, df$time)
 expect_equal(mock_coxph_args$cens, 1 - df$status)

 # Check Z matrix construction
 expect_equal(as.vector(mock_coxph_args$z[, "z1"]), df$z1)
})

test_that("Row Alignment: Correctly subsets adjustment data (Stage 1)", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 # Scenario: plcoxph() removed rows 2 and 4 (e.g., due to missingness in X)
 df <- data.frame(time = 1:5, status = 1, x = 1:5, z = 1:5)
 rownames(df) <- c("A", "B", "C", "D", "E")

 # Simulate plcoxph inputs (subsetted)
 keep_rows <- c("A", "C", "E")
 X <- model.matrix(~ x, data = df)[keep_rows, , drop = FALSE]
 Y <- Surv(df$time, df$status)[c(1, 3, 5), ] # Surv subsetting matches rows
 rownames(Y) <- keep_rows # Surv objects preserve rownames usually

 adj <- mock_adjMixture(df, m.formula = ~ z)

 fit <- fitcoxph.adjMixture(X, Y, adj, list())

 # Engine should receive Z only for rows A, C, E
 expected_z <- df$z[c(1, 3, 5)]
 received_z <- mock_coxph_args$z[, "z"]

 expect_equal(as.vector(received_z), expected_z)
 expect_equal(nrow(mock_coxph_args$x), 3)
})

test_that("Row Alignment: Handles implicit row ordering (No Row Names)", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 # Setup Data
 df <- data.frame(time = 1:5, status = 1, z = 1:5)

 # Force inputs to have NO row names
 X <- matrix(1:5, ncol = 1)
 Y <- Surv(df$time, df$status)

 # Ensure we are actually testing the "No Row Name" path
 expect_null(rownames(X))

 adj <- mock_adjMixture(df, m.formula = ~ z)

 # Should work silently
 expect_silent(fitcoxph.adjMixture(X, Y, adj, list()))

 # Should fail if dimensions mismatch (can't align implicitly)
 X_short <- matrix(1:3, ncol = 1)
 Y_short <- Y[1:3, ]

 expect_error(
  fitcoxph.adjMixture(X_short, Y_short, adj, list()),
  "Row mismatch"
 )
})

test_that("Missingness in Z: Drops rows and warns (Stage 2 Secondary Intersection)", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 # Setup Data: Row C has NA in Z
 df <- data.frame(
  time = 1:5,
  status = 1,
  x = 1:5,
  z = c(1, 2, NA, 4, 5)
 )
 rownames(df) <- LETTERS[1:5]

 X <- model.matrix(~ x, data = df)
 Y <- Surv(df$time, df$status)
 adj <- mock_adjMixture(df, m.formula = ~ z)

 # Execute: We expect a warning about the dropped observation
 expect_warning(
  fit <- fitcoxph.adjMixture(X, Y, adj, list()),
  "Dropped 1 observation.*missing values.*mismatch covariates"
 )

 # Verify Drop
 # Original: 5 rows. Dropped C. Expected: 4 rows.
 expect_equal(nrow(mock_coxph_args$x), 4)
 expect_equal(length(mock_coxph_args$y), 4)
 expect_equal(length(mock_coxph_args$cens), 4)
 expect_equal(nrow(mock_coxph_args$z), 4)

 # Verify specific rows kept (A, B, D, E)
 # X matrix should have rownames A, B, D, E
 expect_equal(rownames(mock_coxph_args$x), c("A", "B", "D", "E"))

 # Verify Post-Processing Output restoration
 # The output fitted object should have names corresponding to the kept rows
 expect_equal(names(fit$match.prob), c("A", "B", "D", "E"))
})

test_that("Safe Matches: Aligns correctly with subsetting and drops", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 # Scenario:
 # 1. Start with 5 rows.
 # 2. plcoxph removes Row 2 (manual subset).
 # 3. Z has NA in Row 3 -> fitcoxph removes Row 3.
 # 4. Check if safe.matches aligns to the remaining rows (1, 4, 5).

 df <- data.frame(
  time = 1:5,
  status = 1,
  z = c(1, 1, NA, 1, 1) # Row 3 bad
 )
 rownames(df) <- 1:5
 safe_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE) # T, F, T, F, T

 # plcoxph input (already subsetted to 1, 3, 4, 5)
 keep_idx <- c(1, 3, 4, 5)
 X <- matrix(rnorm(4), ncol=1)
 rownames(X) <- rownames(df)[keep_idx]
 Y <- Surv(df$time[keep_idx], df$status[keep_idx])

 adj <- mock_adjMixture(df, m.formula = ~ z, safe.matches = safe_vec)

 expect_warning(
  fit <- fitcoxph.adjMixture(X, Y, adj, list()),
  "Dropped 1 observation"
 )

 # Expected Remaining: Rows 1, 4, 5.
 # Original Safe Vector: T, F, T, F, T
 # Row 1 (T) -> Keep
 # Row 2 (F) -> Gone before function called
 # Row 3 (T) -> Dropped by function (NA Z)
 # Row 4 (F) -> Keep
 # Row 5 (T) -> Keep

 # Expected Safe passed to engine: T, F, T
 expect_equal(mock_coxph_args$safe.matches, c(TRUE, FALSE, TRUE))
})

test_that("Input Validation: Switchboard Guardrails", {
 local_mocked_bindings(coxphMixture = my_mock_coxphMixture)

 df <- data.frame(time = 1:5, status = 1, z = 1:5)
 X <- matrix(1:5, ncol=1); rownames(X) <- 1:5
 Y <- Surv(df$time, df$status)

 # 1. Invalid m.rate
 adj_bad_rate <- mock_adjMixture(df, m.formula = ~ z, m.rate = 1.5)
 expect_error(
  fitcoxph.adjMixture(X, Y, adj_bad_rate, list()),
  "m.rate must be strictly between 0 and 1"
 )

 # 2. Invalid Response (Not a Surv object)
 adj_ok <- mock_adjMixture(df, m.formula = ~ z)
 Y_bad <- 1:5
 expect_error(
  fitcoxph.adjMixture(X, Y_bad, adj_ok, list()),
  "Response y must be a Surv object"
 )

 # 3. Missing Data in Adjustment Object
 adj_empty <- mock_adjMixture(NULL)
 adj_empty$data_ref$data <- NULL
 expect_error(
  fitcoxph.adjMixture(X, Y, adj_empty, list()),
  "does not contain linked data"
 )
})
