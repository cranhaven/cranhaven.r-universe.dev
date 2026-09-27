local_edition(3)

# Setup: Create Dummy Data for Testing
setup_data <- function() {
 n <- 100
 data.frame(
  id = 1:n,
  commf = runif(n),
  comml = runif(n),
  hndlnk = sample(c(TRUE, FALSE), n, replace = TRUE),
  bad_col = sample(c("A", "B"), n, replace = TRUE)
 )
}

test_that("adjMixture: Basic valid construction works", {
 df <- setup_data()

 adj <- adjMixture(linked.data = df,
                   m.formula = ~ commf + comml,
                   m.rate = 0.05,
                   safe.matches = df$hndlnk)

 # Check Class
 expect_s3_class(adj, "adjMixture")
 expect_s3_class(adj, "adjustment")

 # Check Structure
 expect_true(is.environment(adj$data_ref))
 expect_equal(adj$data_ref$data, df)
 expect_equal(adj$m.formula, ~ commf + comml)
 expect_equal(-log((1 - adj$m.rate) / adj$m.rate), -log((1 - 0.05) / 0.05))
 expect_equal(adj$safe.matches, df$hndlnk)
})

test_that("adjMixture: Reference semantics (lightweight container) are preserved", {
 df <- setup_data()
 adj <- adjMixture(linked.data = df, m.formula = ~1)

 # Ensure data is stored in an environment, not directly in the list
 expect_type(adj$data_ref, "environment")
 expect_true(exists("data", envir = adj$data_ref))
 expect_equal(adj$data_ref$data, df)
})

test_that("adjMixture: Non-Standard Evaluation (NSE) for safe.matches works", {
 df <- setup_data()

 # Case 1: safe.matches is a bare column name in linked.data
 adj_nse <- adjMixture(linked.data = df,
                       m.formula = ~ commf,
                       safe.matches = hndlnk)

 expect_equal(adj_nse$safe.matches, df$hndlnk)

 # Case 2: safe.matches is an expression evaluating to a column - not supported
 # usually expect the symbol itself or an external vector

 # Case 3: Error when bare name does not exist
 expect_error(
  adjMixture(linked.data = df, m.formula = ~ commf, safe.matches = non_existent_col),
  "Could not find object 'non_existent_col'"
 )
})

test_that("adjMixture: Standard Evaluation for safe.matches works", {
 df <- setup_data()
 external_vec <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE)

 # Case 1: External vector
 adj_ext <- adjMixture(linked.data = df,
                       m.formula = ~ commf,
                       safe.matches = external_vec)
 expect_equal(adj_ext$safe.matches, external_vec)

 # Case 2: Explicit df$col syntax
 adj_dollar <- adjMixture(linked.data = df,
                          m.formula = ~ commf,
                          safe.matches = df$hndlnk)
 expect_equal(adj_dollar$safe.matches, df$hndlnk)
})

test_that("adjMixture: Formula validation enforces rules", {
 df <- setup_data()

 # Not a formula
 expect_error(adjMixture(df, m.formula = "not a formula"), "must be a formula object")

 # Not one-sided
 expect_error(adjMixture(df, m.formula = y ~ x), "must be a one-sided formula")

 # Uses dot (.)
 expect_error(adjMixture(df, m.formula = ~ .), "Usage of '.' in 'm.formula' is not supported")
})

test_that("adjMixture: m.rate validation enforces bounds", {
 df <- setup_data()

 expect_error(adjMixture(df, m.rate = 0), "strictly between 0 and 1")
 expect_error(adjMixture(df, m.rate = 1), "strictly between 0 and 1")
 expect_error(adjMixture(df, m.rate = 1.5), "strictly between 0 and 1")
 expect_error(adjMixture(df, m.rate = -0.1), "strictly between 0 and 1")
 expect_error(adjMixture(df, m.rate = "0.5"), "must be a single numeric value")
})

test_that("adjMixture: safe.matches validation enforces types", {
 df <- setup_data()

 # Non-logical
 expect_error(adjMixture(df, safe.matches = df$id), "must be a logical vector")

 # Contains NAs
 bad_vec <- df$hndlnk
 bad_vec[1] <- NA
 expect_error(adjMixture(df, safe.matches = bad_vec), "cannot contain NA values")

 # Length mismatch
 short_vec <- c(TRUE, FALSE)
 expect_error(adjMixture(df, safe.matches = short_vec), "Length of 'safe.matches' does not match")
})

test_that("adjMixture: Coherence checks between data and formula", {
 df <- setup_data()

 # Variable in formula missing from data
 expect_error(adjMixture(df, m.formula = ~ commf + missing_var),
              "variables in 'm.formula' are missing from 'linked.data'")
})

test_that("adjMixture: linked.data type coercion", {
 df <- setup_data()

 # Pass as list (should coerce to df)
 as_list <- as.list(df)
 adj_list <- adjMixture(linked.data = as_list)
 expect_s3_class(adj_list$data_ref$data, "data.frame")

 # Pass invalid type
 expect_error(adjMixture(linked.data = "character_string"),
              "must be a data.frame, list, or environment")
})

test_that("adjMixture: Environment resolution (linked.data = NULL)", {
 # Setup data in current environment
 n <- 50
 env_df <- data.frame(x = runif(n), y = runif(n))
 x <- env_df$x # Variables must exist in environment for model.frame
 y <- env_df$y

 # Should find x and y in this environment
 adj_env <- adjMixture(linked.data = NULL, m.formula = ~ x + y)

 expect_equal(nrow(adj_env$data_ref$data), n)
 expect_true(all(c("x", "y") %in% names(adj_env$data_ref$data)))

 # Check safe.matches length against environment resolution
 wrong_len_vec <- rep(TRUE, n + 1)
 expect_error(adjMixture(linked.data = NULL, m.formula = ~ x, safe.matches = wrong_len_vec),
              "Length of 'safe.matches' does not match")
})
