local_edition(3)

# -------------------------------------------------------------------------
# Setup: Mock Infrastructure for S3 Dispatch
# -------------------------------------------------------------------------

# Define a dummy class for the adjustment object
dummy_class_surv <- "adjTestSurv"

# Define a dummy internal engine
# This mocks the behavior of fitsurvreg methods.
# It captures inputs to verify plsurvreg passed them correctly.
fitsurvreg.adjTestSurv <- function(x, y, dist, adjustment, control, ...) {
 list(
  status = "dispatched",
  dim_x = dim(x),
  class_y = class(y),
  len_y = nrow(y), # Surv objects are matrices
  dist = dist,
  adjustment_class = class(adjustment),
  control = control,
  captured_args = list(...)
 )
}

# Register the S3 method manually for the test environment
# This ensures plsurvreg finds our mock function without needing a full package namespace.
if (exists("registerS3method")) {
 registerS3method("fitsurvreg", dummy_class_surv, fitsurvreg.adjTestSurv)
}

# -------------------------------------------------------------------------
# Test Suite: Input Validation
# -------------------------------------------------------------------------

test_that("plsurvreg throws errors for invalid inputs", {
 # Setup dummy data
 df <- data.frame(time = 1:5, status = c(1,0,1,0,1), x = 1:5)

 # Missing adjustment
 expect_error(
  plsurvreg(Surv(time, status) ~ x, data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Invalid adjustment type (should pass something that is not a list)
 expect_error(
  plsurvreg(Surv(time, status) ~ x, adjustment = "invalid_string", data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Response is not a Surv object
 adj <- list(data = df); class(adj) <- dummy_class_surv
 expect_error(
  plsurvreg(time ~ x, adjustment = adj),
  "Response must be a 'Surv' object"
 )
})

# -------------------------------------------------------------------------
# Test Suite: Data Retrieval & Precedence
# -------------------------------------------------------------------------

test_that("plsurvreg prioritizes data in adjustment object (Reference Semantics)", {
 # Setup: Adjustment object with specific data (n=3)
 df_internal <- data.frame(time = c(10, 20, 30), status = c(1, 1, 1), x = c(1, 2, 3))
 data_env <- new.env()
 data_env$data <- df_internal

 adj_obj <- list(data_ref = data_env)
 class(adj_obj) <- c(dummy_class_surv, "adjustment")

 # Setup: External data argument (n=5) - Should be ignored
 df_external <- data.frame(time = 1:5, status = 1, x = 1:5)

 # Run
 fit <- plsurvreg(Surv(time, status) ~ x, adjustment = adj_obj, data = df_external)

 # Assert dispatch occurred on the internal data (n=3)
 expect_equal(fit$status, "dispatched")
 expect_equal(fit$len_y, 3)
})

test_that("plsurvreg retrieves data from 'data' list component (Manual/List Style)", {
 # Setup: Simple list with data component
 df <- data.frame(time = c(1, 2, 3, 4), status = c(1, 0, 1, 0), x = c(1, 2, 3, 4))

 adj_list <- list(data = df, param = 0.5)
 class(adj_list) <- dummy_class_surv

 # Run
 fit <- plsurvreg(Surv(time, status) ~ x, adjustment = adj_list)

 # Assertions
 expect_equal(fit$status, "dispatched")
 expect_equal(fit$len_y, 4)
})

# -------------------------------------------------------------------------
# Test Suite: Argument Handling (Dist, Subset, NA, Control)
# -------------------------------------------------------------------------

test_that("plsurvreg passes 'dist' argument correctly", {
 df <- data.frame(time = 1:5, status = 1, x = 1:5)
 adj <- list(data = df); class(adj) <- dummy_class_surv

 # Default (weibull)
 fit_def <- plsurvreg(Surv(time, status) ~ x, adjustment = adj)
 expect_equal(fit_def$dist, "weibull")

 # Custom (e.g., "lognormal")
 fit_cust <- plsurvreg(Surv(time, status) ~ x, adjustment = adj, dist = "lognormal")
 expect_equal(fit_cust$dist, "lognormal")
})

test_that("plsurvreg handles subsetting and NAs", {
 df <- data.frame(
  time = c(1, 2, 3, 4, 5),
  status = c(1, 1, 1, 1, 1),
  x = c(1, 2, NA, 4, 5),
  grp = c("A", "A", "B", "B", "B")
 )
 adj <- list(data = df); class(adj) <- dummy_class_surv

 # Test Subset (grp == "A") -> 2 rows
 fit_sub <- plsurvreg(Surv(time, status) ~ x, adjustment = adj, subset = (grp == "A"))
 expect_equal(fit_sub$len_y, 2)

 # Test NA handling (default na.omit should drop row 3) -> 4 rows
 fit_na <- plsurvreg(Surv(time, status) ~ x, adjustment = adj)
 expect_equal(fit_na$len_y, 4)
})

test_that("plsurvreg passes control list correctly", {
 df <- data.frame(time = 1:5, status = 1, x = 1:5)
 adj <- list(data = df); class(adj) <- dummy_class_surv

 ctrl <- list(maxiter = 100, tol = 1e-4)
 fit <- plsurvreg(Surv(time, status) ~ x, adjustment = adj, control = ctrl)

 expect_equal(fit$control, ctrl)
})

# -------------------------------------------------------------------------
# Test Suite: Return Object Structure
# -------------------------------------------------------------------------

test_that("plsurvreg returns expected components", {
 df <- data.frame(time = 1:10, status = 1, x = rnorm(10))
 adj <- list(data = df); class(adj) <- dummy_class_surv

 # Get x, y, and model
 fit <- plsurvreg(Surv(time, status) ~ x, adjustment = adj, x = TRUE, y = TRUE, model = TRUE)

 # Check Components
 expect_true(!is.null(fit$call))
 expect_true(!is.null(fit$x))
 expect_true(is.matrix(fit$x))
 expect_true(inherits(fit$y, "Surv"))
 expect_s3_class(fit$model, "data.frame")

 # Verify call object
 expect_true(inherits(fit$call, "call"))
 expect_equal(as.character(fit$call[[1]]), "plsurvreg")
})
