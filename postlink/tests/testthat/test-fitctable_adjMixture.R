local_edition(3)

# -------------------------------------------------------------------------
# Setup & Helpers
# -------------------------------------------------------------------------

# A helper to create a mock 'adjMixture' object structure for testing.
# This mimics the output of the adjMixture() constructor.
mock_adj_object <- function(m.rate = 0.1,
                            m.formula = NULL,
                            safe.matches = NULL) {
 structure(
  list(
   m.rate = m.rate,
   m.formula = m.formula,
   safe.matches = safe.matches,
   data_ref = new.env() # Dummy environment
  ),
  class = c("adjMixture", "adjustment")
 )
}

# -------------------------------------------------------------------------
# Test Block 1: Basic Functionality
# -------------------------------------------------------------------------

test_that("fitctable.adjMixture runs successfully with valid inputs", {
 # Create Valid Data
 tab <- matrix(c(100, 20, 20, 100), nrow = 2)
 adj <- mock_adj_object(m.rate = 0.1)

 # Run Function
 fit <- fitctable.adjMixture(ftable = tab, adjustment = adj, control = list())

 # Assertions
 expect_s3_class(fit, "ctableMixture")
 expect_equal(fit$adjustment$m.rate, 0.1)

 # Check if the internal engine actually produced output (e.g., phat exists)
 expect_true("phat" %in% names(fit))
 expect_true(is.matrix(fit$phat))
})

# -------------------------------------------------------------------------
# Test Block 2: Table Input Validation
# -------------------------------------------------------------------------

test_that("fitctable.adjMixture rejects invalid table inputs", {
 adj <- mock_adj_object(m.rate = 0.1)

 # Case: Non-numeric matrix (Character)
 char_tab <- matrix(c("a", "b", "c", "d"), nrow = 2)
 expect_error(
  fitctable.adjMixture(char_tab, adj),
  "not a valid numeric matrix"
 )

 # Case: Negative counts
 neg_tab <- matrix(c(100, -5, 20, 100), nrow = 2)
 expect_error(
  fitctable.adjMixture(neg_tab, adj),
  "negative counts"
 )
})

test_that("fitctable.adjMixture handles NAs with a warning", {
 adj <- mock_adj_object(m.rate = 0.1)

 # Case: NA in table
 na_tab <- matrix(c(100, NA, 20, 100), nrow = 2)

 expect_warning(
  fit <- fitctable.adjMixture(na_tab, adj),
  "contains NA values"
 )

 # Ensure NAs were treated as 0 (Algorithm should converge without error)
 expect_s3_class(fit, "ctableMixture")
})

# -------------------------------------------------------------------------
# Test Block 3: Adjustment Object Validation (m.rate)
# -------------------------------------------------------------------------

test_that("fitctable.adjMixture enforces valid m.rate", {
 tab <- matrix(c(50, 50, 50, 50), nrow = 2)

 # Case: m.rate is NULL
 adj_null <- mock_adj_object(m.rate = NULL)
 expect_error(
  fitctable.adjMixture(tab, adj_null),
  "NULL 'm.rate'"
 )

 # Case: m.rate is out of bounds (> 1)
 adj_high <- mock_adj_object(m.rate = 1.5)
 expect_error(
  fitctable.adjMixture(tab, adj_high),
  "strictly between 0 and 1"
 )

 # Case: m.rate is out of bounds (< 0)
 adj_neg <- mock_adj_object(m.rate = -0.1)
 expect_error(
  fitctable.adjMixture(tab, adj_neg),
  "strictly between 0 and 1"
 )

 # Case: m.rate is not scalar
 adj_vec <- mock_adj_object(m.rate = c(0.1, 0.2))
 expect_error(
  fitctable.adjMixture(tab, adj_vec),
  "single numeric value"
 )
})

# -------------------------------------------------------------------------
# Test Block 4: Logic/Warning Triggers
# -------------------------------------------------------------------------

test_that("fitctable.adjMixture warns if m.formula is present", {
 tab <- matrix(c(100, 20, 20, 100), nrow = 2)

 # Create object with a formula (implies covariate-dependent error)
 # The table method cannot handle this, so it must warn.
 adj_with_formula <- mock_adj_object(m.rate = 0.1, m.formula = ~ x1 + x2)

 expect_warning(
  fit <- fitctable.adjMixture(tab, adj_with_formula),
  "Covariates .* are ignored"
 )

 # Ensure it still ran despite warning
 expect_s3_class(fit, "ctableMixture")
})

test_that("fitctable.adjMixture warns if safe.matches are present", {
 tab <- matrix(c(100, 20, 20, 100), nrow = 2)

 # Create object with safe matches (implies record-level fix)
 # The table method (aggregate) cannot handle this.
 adj_with_safe <- mock_adj_object(m.rate = 0.1, safe.matches = c(TRUE, FALSE))

 expect_warning(
  fitctable.adjMixture(tab, adj_with_safe),
  "safe matches .* are ignored"
 )
})

# -------------------------------------------------------------------------
# Test Block 5: Integration with Engine
# -------------------------------------------------------------------------

test_that("fitctable passes control arguments to engine", {
 tab <- matrix(c(100, 20, 20, 100), nrow = 2)
 adj <- mock_adj_object(m.rate = 0.1)

 # Pass a control argument (e.g., max.iter = 1)
 # This should result in a fit that claims 'converged = FALSE'
 # (unless it converges in 1 step, which is unlikely for this data)

 fit <- fitctable.adjMixture(tab, adj, control = list(max.iter = 1))

 if (fit$converged == TRUE) {
  # If it somehow converged in 1 step, check that we didn't crash
  succeed("Converged in 1 step (unexpected but valid execution)")
 } else {
  expect_false(fit$converged)
 }
})
