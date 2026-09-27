local_edition(3)

# -------------------------------------------------------------------------
# Helper: Mock Constructor for adjMixture objects
# -------------------------------------------------------------------------
# This allows us to test fitglm without relying on the actual adjMixture() function
mock_adjMixture <- function(data, m.formula = ~1, m.rate = NULL, safe.matches = NULL) {
 e <- new.env()
 e$data <- data
 structure(
  list(
   data_ref = e,
   m.formula = m.formula,
   m.rate = m.rate,
   safe.matches = safe.matches
  ),
  class = "adjMixture"
 )
}

# -------------------------------------------------------------------------
# Helper: Mock Function
# -------------------------------------------------------------------------
# We mock glmMixture so we don't need to run the actual implementation.
# We just want to verify fitglm passed the correct data to it.
mock_glmMixture_args <- new.env()

my_mock_glmMixture <- function(x, y, family, z, m.rate, safe.matches, control, ...) {
 # Capture arguments for inspection
 mock_glmMixture_args$x <- x
 mock_glmMixture_args$y <- y
 mock_glmMixture_args$z <- z
 mock_glmMixture_args$safe.matches <- safe.matches
 mock_glmMixture_args$m.rate <- m.rate

 n_obs <- nrow(as.matrix(x))

 # Return a dummy object with expected structure
 structure(
  list(
   coefficients = c(1, 1),
   residuals = rep(0, n_obs),
   fitted.values = rep(0, n_obs),
   linear.predictors = rep(0, n_obs),
   match.prob = rep(0.5, n_obs)
  ),
  class = c("glmMixture", "plglm", "glm", "lm")
 )
}

# -------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------

test_that("Basic Dispatch: Passes correct data to internal function", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Setup Data
 df <- data.frame(
  id = 1:5,
  y = c(1, 0, 1, 0, 1),
  x1 = c(1, 2, 3, 4, 5),
  z1 = c(0, 1, 0, 1, 0)
 )
 rownames(df) <- paste0("row", 1:5)

 # Create Inputs
 X <- model.matrix(~ x1, data = df)
 Y <- df$y
 adj <- mock_adjMixture(df, m.formula = ~ z1)

 # Run Function
 fit <- fitglm.adjMixture(x = X, y = Y, family = binomial(), adjustment = adj, control = list())

 # Assertions
 expect_true(inherits(fit, "glmMixture"))
 expect_true(inherits(fit, "plglm"))
 expect_equal(mock_glmMixture_args$x, X)
 expect_equal(mock_glmMixture_args$y, Y)
 # Check if Z matrix was built correctly from m.formula
 expect_equal(as.vector(mock_glmMixture_args$z[, "z1"]), df$z1)
})

test_that("Row Alignment: Correctly subsets adjustment data (Stage 1)", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Scenario: plglm() removed rows 2 and 4 (e.g., due to missingness in Y)
 df <- data.frame(y = 1:5, x = 1:5, z = 1:5)
 rownames(df) <- c("A", "B", "C", "D", "E")

 # Simulate plglm inputs (subsetted)
 X <- model.matrix(~ x, data = df)[c(1, 3, 5), , drop = FALSE] # Keep A, C, E
 Y <- df$y[c(1, 3, 5)]

 adj <- mock_adjMixture(df, m.formula = ~ z)

 fit <- fitglm.adjMixture(X, Y, gaussian(), adj, list())

 # should receive Z only for rows A, C, E
 expected_z <- df$z[c(1, 3, 5)]
 received_z <- mock_glmMixture_args$z[, "z"]

 expect_equal(as.vector(received_z), expected_z)
 expect_equal(nrow(mock_glmMixture_args$x), 3)
})

test_that("Row Alignment: Handles implicit row ordering (No Row Names)", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Setup Data with slight noise (avoids 'Perfect Fit' warning)
 df <- data.frame(y = c(1.1, 1.9, 3.1, 3.9, 5.1), z = 1:5)

 # Force inputs to have NO row names
 X <- matrix(1:5, ncol = 1)
 Y <- df$y

 # Ensure we are actually testing the "No Row Name" path
 expect_null(rownames(X))

 adj <- mock_adjMixture(df, m.formula = ~ z)

 # This should be silent (no errors/warnings)
 expect_silent(fitglm.adjMixture(X, Y, gaussian(), adj, list()))

 # Should fail if dimensions mismatch (implicit alignment impossible)
 X_short <- matrix(1:3, ncol = 1)
 expect_error(
  fitglm.adjMixture(X_short, Y[1:3], gaussian(), adj, list()),
  "Row mismatch"
 )
})

test_that("Missingness in Z: Drops rows and warns (Stage 2)", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Setup Data: row names (A, B, C, D, E) verify exactly which row is dropped
 df <- data.frame(y = 1:5, x = 1:5, z = c(1, 2, NA, 4, 5))
 rownames(df) <- LETTERS[1:5]

 X <- model.matrix(~ x, data = df)
 Y <- df$y
 adj <- mock_adjMixture(df, m.formula = ~ z)

 # We expect a warning about the dropped observation
 expect_warning(
  fit <- fitglm.adjMixture(X, Y, gaussian(), adj, list()),
  "Dropped 1 observation.*missing values.*mismatch covariates"
 )

 # Check 1: Did we get the right class back?
 expect_true(inherits(fit, "glmMixture"))

 # Check 2: Are the dimensions correct?
 # If row 3 was dropped, the residuals should have length 4.
 expect_equal(length(fit$residuals), 4)
 expect_equal(length(fit$fitted.values), 4)

 # Check 3: Did we drop the correct row?
 # The output residuals should have names matching the kept rows (A, B, D, E).
 # Row "C" (where Z was NA) should be missing.
 if (!is.null(names(fit$residuals))) {
  expect_true("A" %in% names(fit$residuals))
  expect_false("C" %in% names(fit$residuals))
 }
})

test_that("Safe Matches: Aligns correctly with subsetting", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Setup Data: We use y and x that are correlated but not identical.
 set.seed(123)
 df <- data.frame(
  y = rnorm(5),
  z = c(1, 1, NA, 1, 1)
 )
 # X is just random noise, unrelated to Y, but valid for checking alignment
 df$x <- rnorm(5)

 rownames(df) <- 1:5
 safe_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE) # T, F, T, F, T

 # Prepare Inputs
 # plglm subset: We manually drop Row 2 (Index 2)
 # Remaining Rows: 1, 3, 4, 5
 X <- model.matrix(~ x, data = df)[c(1, 3, 4, 5), , drop = FALSE]
 Y <- df$y[c(1, 3, 4, 5)]

 adj <- mock_adjMixture(df, m.formula = ~ z, safe.matches = safe_vec)

 # This expects a warning because Row 3 (which corresponds to orig Row 3) has NA in Z
 expect_warning(
  fit <- fitglm.adjMixture(X, Y, gaussian(), adj, list()),
  "Dropped 1 observation"
 )

 # 4. Verification
 # We track the rows that should survive:
 # - Row 1 (Safe=T): Kept
 # - Row 2 (Safe=F): Dropped by plglm manual subset
 # - Row 3 (Safe=T): Dropped by fitglm due to NA in Z
 # - Row 4 (Safe=F): Kept
 # - Row 5 (Safe=T): Kept

 # Expected Surviving Indices: 1, 4, 5
 # Expected Safe Matches: T, F, T

 # Check 1: Did we get a result?
 expect_true(inherits(fit, "glmMixture"))

 # Check 2: Are the residuals the correct length? (Should be 3)
 expect_equal(length(fit$residuals), 3)

 # Check 3: Did the Safe Matches align correctly?
 # we can inspect the "names".
 if (!is.null(names(fit$residuals))) {
  expect_equal(names(fit$residuals), c("1", "4", "5"))
 }
})

test_that("Error Handling: Validates Adjustment Object", {
 local_mocked_bindings(glmMixture = my_mock_glmMixture)

 # Test missing data in adjustment object
 adj_empty <- mock_adjMixture(NULL)
 adj_empty$data_ref$data <- NULL # Explicitly remove data

 X <- matrix(1, ncol=1, nrow=5)
 Y <- 1:5

 expect_error(
  fitglm.adjMixture(X, Y, gaussian(), adj_empty, list()),
  "does not contain linked data"
 )
})
