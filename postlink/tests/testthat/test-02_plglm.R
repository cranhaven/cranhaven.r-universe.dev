local_edition(3)

# -------------------------------------------------------------------------
# Setup: Mock Infrastructure for Testing
# -------------------------------------------------------------------------

# Define a dummy class to trigger S3 dispatch
# This simulates an adjustment object class like "adjELE" or "adjMixture"
dummy_class <- "adjTest"

# Define a dummy internal engine
# This mocks the behavior of fitglm.adjELE or fitglm.adjMixture.
# It simply captures the inputs it received so we can verify plglm passed them correctly.
fitglm.adjTest <- function(x, y, family, adjustment, control, ...) {
 list(
  status = "dispatched",
  dim_x = dim(x),
  len_y = length(y),
  family_name = family$family,
  adjustment_class = class(adjustment),
  control = control,
  captured_args = list(...)
 )
}

# Register the S3 method manually for the test environment
# This ensures that when plglm calls fitglm(), R finds our mock function.
# Note: In a real package load, this happens automatically via namespace.
if (exists("registerS3method")) {
 registerS3method("fitglm", dummy_class, fitglm.adjTest)
}

# -------------------------------------------------------------------------
# Test Suite: Input Validation
# -------------------------------------------------------------------------

test_that("plglm throws errors for invalid inputs", {
 # Setup dummy data
 df <- data.frame(y = c(1, 0, 1), x = c(1, 2, 3))

 # Missing adjustment
 expect_error(
  plglm(y ~ x, data = df),
  "'adjustment' must be a valid adjustment object"
 )

 # Invalid adjustment type (scalar, not list/object)
 expect_error(
  plglm(y ~ x, adjustment = "invalid_string"),
  "'adjustment' must be a valid adjustment object"
 )

 # Invalid family
 adj <- list(data = df)
 class(adj) <- dummy_class
 expect_error(
  plglm(y ~ x, family = "not_a_real_family", adjustment = adj),
  "'family' not recognized"
 )
})

# -------------------------------------------------------------------------
# Test Suite: Data Retrieval Mechanisms
# -------------------------------------------------------------------------

test_that("plglm retrieves data from 'data_ref' environment (Constructor Style)", {
 # Setup: Create environment structure mimicking adjELE/adjMixture
 df <- data.frame(outcome = c(1, 0, 1), predictor = c(10, 20, 30))
 data_env <- new.env()
 data_env$data <- df

 adj_obj <- list(data_ref = data_env)
 class(adj_obj) <- c(dummy_class, "adjustment")

 # Run plglm
 fit <- plglm(outcome ~ predictor, family = binomial, adjustment = adj_obj)

 # Assertions
 expect_equal(fit$status, "dispatched")
 expect_equal(fit$dim_x, c(3, 2)) # Intercept + predictor
 expect_equal(fit$len_y, 3)
})

test_that("plglm retrieves data from 'data' list component (Manual/List Style)", {
 # Setup: Simple list with data component
 df <- data.frame(outcome = c(1, 1, 0, 0), predictor = c(1, 2, 3, 4))

 adj_list <- list(data = df, param = 0.5)
 class(adj_list) <- dummy_class

 # Run plglm
 fit <- plglm(outcome ~ predictor, family = gaussian, adjustment = adj_list)

 # Assertions
 expect_equal(fit$status, "dispatched")
 expect_equal(fit$len_y, 4)
 expect_equal(fit$family_name, "gaussian")
})

# -------------------------------------------------------------------------
# Test Suite: Argument Handling (Subset, NA, Control)
# -------------------------------------------------------------------------

test_that("plglm handles subsetting and NAs correctly", {
 # Setup: Data with NA and distinct groups
 df <- data.frame(
  y = c(1, 0, 1, NA, 0),
  x = c(1, 2, 3, 4, 5),
  grp = c("A", "A", "B", "B", "B")
 )
 adj <- list(data = df)
 class(adj) <- dummy_class

 # Test Subsetting
 fit_sub <- plglm(y ~ x, adjustment = adj, subset = (grp == "A"))
 expect_equal(fit_sub$len_y, 2) # Only 2 rows in group A

 # Test NA Action (na.omit is default usually, but we check if it works)
 fit_na <- plglm(y ~ x, adjustment = adj, na.action = na.omit)
 expect_equal(fit_na$len_y, 4) # 5 rows - 1 NA = 4

 # Test Control
 ctrl <- list(tol = 1e-6, maxiter = 50)
 fit_ctrl <- plglm(y ~ x, adjustment = adj, control = ctrl)
 expect_equal(fit_ctrl$control, ctrl)
})

# -------------------------------------------------------------------------
# Test Suite: Return Objects (x, y, model)
# -------------------------------------------------------------------------

test_that("plglm returns model components when requested", {
 df <- data.frame(y = c(1, 0), x = c(1, 2))
 adj <- list(data = df)
 class(adj) <- dummy_class

 # Case 1: Default (model=TRUE, x=FALSE, y=FALSE)
 fit1 <- plglm(y ~ x, adjustment = adj)
 expect_true(!is.null(fit1$model))
 expect_null(fit1$x)
 expect_null(fit1$y)
 expect_s3_class(fit1$model, "data.frame")

 # Case 2: Request everything (model=TRUE, x=TRUE, y=TRUE)
 fit2 <- plglm(y ~ x, adjustment = adj, x = TRUE, y = TRUE)
 expect_true(!is.null(fit2$x))
 expect_true(!is.null(fit2$y))
 expect_true(is.matrix(fit2$x))
 expect_true(is.numeric(fit2$y)) # or factor depending on input

 # Case 3: Verify Call is attached
 expect_true(!is.null(fit2$call))
 expect_true(inherits(fit2$call, "call"))
})

# -------------------------------------------------------------------------
# Test Suite: List Support (No Class Inheritance)
# -------------------------------------------------------------------------

test_that("plglm accepts a raw list if class is manually set for dispatch", {
 # This tests the relaxation of "inherits(adjustment, 'adjustment')"
 # The user provides a list but must set the class for the S3 generic to work.

 df <- data.frame(y = 1:5, x = 1:5)

 # Raw list, but we give it the dummy class so dispatch finds fitglm.adjTest
 adj_raw <- list(data = df, some_param = 100)
 class(adj_raw) <- dummy_class

 fit <- plglm(y ~ x, adjustment = adj_raw)

 expect_equal(fit$status, "dispatched")
 expect_equal(fit$len_y, 5)
})
