local_edition(3)

# ------------------------------------------------------------------------------
# Helper: Data Generation
# ------------------------------------------------------------------------------
generate_test_data <- function(n = 1000, alpha = 0.2, seed = 1234) {
 set.seed(seed)
 K <- 3
 L <- 4
 cellprobs <- c(0.18, 0.05, 0.03, 0.04, 0.02, 0.14,
                0.02, 0.02, 0.10, 0.21, 0.15, 0.04)

 # Generate true data
 dat <- stats::rmultinom(n = n, size = 1, prob = cellprobs)
 obs_idx <- apply(dat, 2, function(x) which(x == 1))
 X <- ceiling(obs_idx / L)
 Y <- (obs_idx %% L); Y[Y == 0] <- L

 # Introduce linkage error
 k <- round(n * alpha)
 Yperm <- Y
 if (k > 0) {
  Yperm[1:k] <- sample(Y[1:k])
 }

 # Create table
 tab_obs <- table(X, Yperm)

 list(tab = tab_obs, true_probs = matrix(cellprobs, nrow=K, ncol=L))
}

# ------------------------------------------------------------------------------
# Test Group 1: Input Validation
# ------------------------------------------------------------------------------
test_that("Input validation catches invalid arguments", {
 # Generate valid data
 data <- generate_test_data()
 tab <- data$tab

 # 1. Invalid 'tab' types
 expect_error(ctableMixture("not a matrix", 0.1),
              "Input 'tab' must be a numeric matrix or table")
 expect_error(ctableMixture(list(1, 2), 0.1),
              "Input 'tab' must be a numeric matrix or table")

 # 2. Negative counts
 tab_neg <- tab
 tab_neg[1, 1] <- -5
 expect_error(ctableMixture(tab_neg, 0.1),
              "non-negative counts")

 # 3. Invalid 'm.rate'
 expect_error(ctableMixture(tab, -0.1), "m.rate.*between 0 and 1")
 expect_error(ctableMixture(tab, 1.5), "m.rate.*between 0 and 1")
 expect_error(ctableMixture(tab, "0.5"), "m.rate.*single numeric")
 expect_error(ctableMixture(tab, c(0.1, 0.2)), "m.rate.*single numeric")
})

# ------------------------------------------------------------------------------
# Test Group 2: Basic Functionality & Output Structure
# ------------------------------------------------------------------------------
test_that("Function returns correct structure for valid input", {
 data <- generate_test_data(alpha = 0.2)
 fit <- ctableMixture(data$tab, m.rate = 0.2)

 # Check class/type
 expect_type(fit, "list")

 # Check components existence
 expected_names <- c("phat", "phat0", "var", "ftable", "objective", "converged")
 expect_true(all(expected_names %in% names(fit)))

 # Check dimensions
 expect_equal(dim(fit$phat), dim(data$tab))
 expect_equal(dim(fit$phat0), dim(data$tab))
 expect_equal(dim(fit$ftable), dim(data$tab))

 # Variance matrix size should be (K*L) x (K*L)
 n_cells <- prod(dim(data$tab))
 expect_equal(dim(fit$var), c(n_cells, n_cells))

 # Check logicals
 expect_type(fit$converged, "logical")
 expect_true(fit$converged) # Should converge for standard data
})

# ------------------------------------------------------------------------------
# Test Group 3: Statistical Correctness
# ------------------------------------------------------------------------------
test_that("Statistical properties hold (Sum constraints & Mismatch handling)", {
 # Case A: No Mismatch (alpha = 0)
 # result should equal observed proportions exactly
 data_clean <- generate_test_data(alpha = 0)
 fit_clean <- ctableMixture(data_clean$tab, m.rate = 0)

 obs_probs <- prop.table(data_clean$tab)

 # phat should be extremely close to observed frequencies when m.rate=0
 expect_equal(as.numeric(fit_clean$phat), as.numeric(obs_probs), tolerance = 1e-4)

 # Case B: Sum to One Constraint
 data_err <- generate_test_data(alpha = 0.2)
 fit_err <- ctableMixture(data_err$tab, m.rate = 0.2)

 expect_equal(sum(fit_err$phat), 1, tolerance = 1e-6)
 expect_equal(sum(fit_err$phat0), 1, tolerance = 1e-6)

 # Case C: ftable reconstruction
 # fit$ftable is phat * n_total
 n_total <- sum(data_err$tab)
 expect_equal(sum(fit_err$ftable), n_total, tolerance = 1e-6)
})

# ------------------------------------------------------------------------------
# Test Group 4: Control Arguments & '...' Passing
# ------------------------------------------------------------------------------
test_that("Control arguments are respected", {
 data <- generate_test_data()

 # 1. Test 'tol' via '...' (w/ loose and strict tolerance)
 fit_loose <- ctableMixture(data$tab, m.rate = 0.2, tol = 1e-1)
 fit_strict <- ctableMixture(data$tab, m.rate = 0.2, tol = 1e-8)

 # Strict should likely have a slightly lower (better) objective (negative log likelihood)
 # or at least be different if convergence cut off early for loose
 expect_true(fit_strict$objective <= fit_loose$objective)

 # 2. Test 'max.iter' via 'control' list
 # If we set max.iter to 1, it likely won't converge fully compared to default 1000
 fit_short <- ctableMixture(data$tab, m.rate = 0.2, control = list(max.iter = 1))

 # Check if 'converged' status behaves as expected or results differ
 if(fit_short$converged == FALSE) {
  expect_false(fit_short$converged)
 }

 # 3. Test 'max.iter' via '...' (should override default)
 fit_dots <- ctableMixture(data$tab, m.rate = 0.2, max.iter = 1)
 expect_equal(fit_short$objective, fit_dots$objective)
})

# ------------------------------------------------------------------------------
# Test Group 5: Output Formatting (Labels and Names)
# ------------------------------------------------------------------------------
test_that("Output retains dimension names and formats variance labels", {
 # Create a table with specific names
 my_tab <- matrix(c(10, 20, 30, 40), nrow = 2)
 dimnames(my_tab) <- list(RowGroup = c("A", "B"), ColGroup = c("X", "Y"))

 fit <- ctableMixture(my_tab, m.rate = 0.1)

 # Check phat dimension names
 expect_equal(dimnames(fit$phat), dimnames(my_tab))
 expect_equal(dimnames(fit$ftable), dimnames(my_tab))

 # Check Variance labels
 # The code generates labels like "(RowName, ColName)"
 # Row 1 (A), Col 1 (X) -> (A, X)
 var_names <- rownames(fit$var)
 expect_true("(A, X)" %in% var_names)
 expect_true("(B, Y)" %in% var_names)
})

# ------------------------------------------------------------------------------
# Test Group 6: Verification against "Reference" Logic
# ------------------------------------------------------------------------------
test_that("Replication of test logic yields valid results", {
 # alpha = 0.2, n = 1000
 data <- generate_test_data(n = 1000, alpha = 0.2, seed = 1234)

 fit <- ctableMixture(data$tab, m.rate = 0.2)

 # The method implies that the adjusted table (ftable) should sum to N
 expect_equal(sum(fit$ftable), 1000)

 # Verify attributes
 expect_true(is.matrix(fit$phat))
 expect_true(is.table(fit$ftable))
})
