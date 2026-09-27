local_edition(3)

# -------------------------------------------------------------------------
# Setup: Mock Infrastructure
# -------------------------------------------------------------------------

# Define a dummy class
dummy_class_tab <- "adjTestTable"

# Define a dummy internal engine
# fitctable receives the computed table, not the raw data frame.
fitctable.adjTestTable <- function(ftable, adjustment, control, ...) {
 list(
  status = "dispatched",
  table_dims = dim(ftable),
  table_sum = sum(ftable),
  table_class = class(ftable),
  adjustment_class = class(adjustment),
  control = control,
  captured_args = list(...)
 )
}

# Register the S3 method
if (exists("registerS3method")) {
 registerS3method("fitctable", dummy_class_tab, fitctable.adjTestTable)
}

# -------------------------------------------------------------------------
# Test Suite: Input Validation
# -------------------------------------------------------------------------

test_that("plctable validates inputs", {
 df <- data.frame(row = c("A", "B"), col = c("X", "Y"))

 # Missing adjustment
 expect_error(
  plctable(row ~ col, data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Invalid adjustment
 expect_error(
  plctable(row ~ col, adjustment = "invalid string", data = df),
  "'adjustment' must be a valid adjustment object"
 )
})

# -------------------------------------------------------------------------
# Test Suite: Table Construction & Data Priority
# -------------------------------------------------------------------------

test_that("plctable correctly constructs table from adjustment data", {
 # Setup: Adjustment object with embedded data
 # Create a 2x2 scenario: A-X, A-Y, B-X, B-Y
 df_internal <- data.frame(
  r = c("A", "A", "B", "B"),
  c = c("X", "Y", "X", "Y")
 )
 adj <- list(data = df_internal); class(adj) <- dummy_class_tab

 # Run
 fit <- plctable(~ r + c, adjustment = adj)

 # Verify dispatch
 expect_equal(fit$status, "dispatched")
 # Expect 2x2 table with sum = 4
 expect_equal(fit$table_dims, c(2, 2))
 expect_equal(fit$table_sum, 4)
})

test_that("plctable handles LHS/RHS formula syntax for xtabs", {
 # xtabs supports `~ x + y` (creates table of x vs y)
 # and `count ~ x + y` (weighted table).
 # The plctable doc says: "left and right hand sides specifying column and row"

 df <- data.frame(
  row_var = c("R1", "R2", "R1"),
  col_var = c("C1", "C2", "C1"),
  weights = c(10, 20, 30)
 )
 adj <- list(data = df); class(adj) <- dummy_class_tab

 # Standard cross-classification (~ row + col)
 fit1 <- plctable(~ row_var + col_var, adjustment = adj)
 expect_equal(fit1$table_sum, 3) # 3 rows, count is 1 each

 # Weighted classification (weights ~ row + col)
 fit2 <- plctable(weights ~ row_var + col_var, adjustment = adj)
 expect_equal(fit2$table_sum, 60) # 10 + 20 + 30
})

# -------------------------------------------------------------------------
# Test Suite: Argument Passthrough (Subset, Exclude, Control)
# -------------------------------------------------------------------------

test_that("plctable handles subset, na.action, and exclude", {
 df <- data.frame(
  r = c("A", "A", "B", NA),
  c = c("X", "X", "Y", "Y"),
  grp = c(1, 2, 1, 1)
 )
 adj <- list(data = df); class(adj) <- dummy_class_tab

 # Test Subset (grp == 1) -> Rows 1, 3, 4
 # Note: Row 4 has NA in 'r'.
 # Default xtabs behavior drops NAs unless exclude=NULL or na.action=na.pass.

 # We test explicit subsetting logic
 fit_sub <- plctable(~ r + c, adjustment = adj, subset = (grp == 1))
 # Rows: (A, X), (B, Y), (NA, Y). xtabs drops NA by default.
 # So we expect sum = 2 (Rows 1 and 3)
 expect_equal(fit_sub$table_sum, 2)

 # Test Exclude
 # Exclude "A" -> Only "B" remains
 fit_ex <- plctable(~ r + c, adjustment = adj, exclude = c("A", NA))
 # Should have 1 entry (B, Y). (NA is dropped by default, A is excluded)
 expect_equal(fit_ex$table_sum, 1)

 # Test Control
 ctrl <- list(tol = 1e-8)
 fit_c <- plctable(~ r + c, adjustment = adj, control = ctrl)
 expect_equal(fit_c$control$tol, 1e-8)
})

# -------------------------------------------------------------------------
# Test Suite: Return Object Structure
# -------------------------------------------------------------------------

test_that("plctable attaches correct class and formula", {
 df <- data.frame(r = 1:2, c = 1:2)
 adj <- list(data = df); class(adj) <- dummy_class_tab

 form <- ~ r + c
 fit <- plctable(form, adjustment = adj)

 expect_true(inherits(fit, "plctable"))
 expect_true(!is.null(fit$call))
 expect_equal(fit$formula, form)
})
