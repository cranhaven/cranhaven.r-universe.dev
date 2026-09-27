local_edition(3)

# -------------------------------------------------------------------------
# Setup: Mock Infrastructure for S3 Dispatch
# -------------------------------------------------------------------------

# Define a dummy class for the adjustment object
dummy_class_cox <- "adjTestCox"

# Define a dummy internal engine
# This mocks the behavior of fitcoxph methods (e.g., fitcoxph.adjMixture).
# It captures inputs to verify plcoxph passed them correctly.
fitcoxph.adjTestCox <- function(x, y, adjustment, control, ...) {
 list(
  status = "dispatched",
  dim_x = dim(x),
  class_y = class(y),
  len_y = nrow(y), # Surv objects are matrices
  adjustment_class = class(adjustment),
  control = control,
  captured_args = list(...)
 )
}

# Register the S3 method manually for the test environment
# This ensures plcoxph finds our mock function without needing a full package namespace.
if (exists("registerS3method")) {
 registerS3method("fitcoxph", dummy_class_cox, fitcoxph.adjTestCox)
}

# -------------------------------------------------------------------------
# Test Suite: Input Validation
# -------------------------------------------------------------------------

test_that("plcoxph throws errors for invalid inputs", {
 # Setup dummy data
 df <- data.frame(time = 1:5, status = c(1,0,1,0,1), x = 1:5)

 # Missing adjustment
 expect_error(
  plcoxph(Surv(time, status) ~ x, data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Invalid adjustment type
 expect_error(
  plcoxph(Surv(time, status) ~ x, adjustment = "invalid_string", data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Response is not a Surv object
 adj <- list(data = df); class(adj) <- dummy_class_cox
 expect_error(
  plcoxph(time ~ x, adjustment = adj),
  "Response must be a 'Surv' object"
 )
})

# -------------------------------------------------------------------------
# Test Suite: Data Retrieval & Precedence
# -------------------------------------------------------------------------

test_that("plcoxph prioritizes data in adjustment object (Reference Semantics)", {
 # Setup: Adjustment object with specific data (n=3)
 df_internal <- data.frame(time = c(10, 20, 30), status = c(1, 1, 1), x = c(1, 2, 3))
 data_env <- new.env()
 data_env$data <- df_internal

 adj_obj <- list(data_ref = data_env)
 class(adj_obj) <- c(dummy_class_cox, "adjustment")

 # Setup: External data argument (n=5) - Should be IGNORED
 df_external <- data.frame(time = 1:5, status = 1, x = 1:5)

 # Run
 fit <- plcoxph(Surv(time, status) ~ x, adjustment = adj_obj, data = df_external)

 # Assert dispatch occurred on the internal data (n=3)
 expect_equal(fit$status, "dispatched")
 expect_equal(fit$len_y, 3)
})

# -------------------------------------------------------------------------
# Test Suite: Model Frame & Matrix Construction
# -------------------------------------------------------------------------

test_that("plcoxph constructs correct Cox design matrices (No Intercept)", {
 df <- data.frame(time = 1:4, status = 1, f = factor(c("A", "B", "A", "B")))
 adj <- list(data = df); class(adj) <- dummy_class_cox

 fit <- plcoxph(Surv(time, status) ~ f, adjustment = adj)

 # Cox models should drop the intercept from the design matrix
 # For a factor with 2 levels, model.matrix creates Intercept + fB.
 # fitcoxph should receive only fB (1 column).
 expect_equal(fit$dim_x, c(4, 1))
})

test_that("plcoxph handles subsetting and NAs", {
 df <- data.frame(
  time = c(1, 2, 3, 4, 5),
  status = c(1, 1, 1, 1, 1),
  x = c(1, 2, NA, 4, 5),
  grp = c("A", "A", "B", "B", "B")
 )
 adj <- list(data = df); class(adj) <- dummy_class_cox

 # Test Subset (grp == "A") -> 2 rows
 fit_sub <- plcoxph(Surv(time, status) ~ x, adjustment = adj, subset = (grp == "A"))
 expect_equal(fit_sub$len_y, 2)

 # Test NA handling (default na.omit should drop row 3) -> 4 rows
 fit_na <- plcoxph(Surv(time, status) ~ x, adjustment = adj)
 expect_equal(fit_na$len_y, 4)
})

# -------------------------------------------------------------------------
# Test Suite: Return Object Structure
# -------------------------------------------------------------------------

test_that("plcoxph returns expected class structure and components", {
 df <- data.frame(time = 1:10, status = 1, x = rnorm(10))
 adj <- list(data = df); class(adj) <- dummy_class_cox

 # Request x, y, and model
 fit <- plcoxph(Surv(time, status) ~ x, adjustment = adj, x = TRUE, y = TRUE, model = TRUE)

 # Check Classes
 expect_true(inherits(fit, "plcoxph"))
 expect_s3_class(fit, "plcoxph")
 expect_s3_class(fit, "plmodel")

 # Check Components
 expect_true(!is.null(fit$call))
 expect_true(!is.null(fit$x))
 expect_true(is.matrix(fit$x))
 expect_true(inherits(fit$y, "Surv"))
 expect_s3_class(fit$model, "data.frame")

 # Verify control passing
 ctrl <- list(iter.max = 99)
 fit_c <- plcoxph(Surv(time, status) ~ x, adjustment = adj, control = ctrl)
 expect_equal(fit_c$control$iter.max, 99)
})
