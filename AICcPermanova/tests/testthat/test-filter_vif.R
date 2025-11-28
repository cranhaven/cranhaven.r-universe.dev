library(vegan)
# Test that the function runs without errors
test_that("filter_vif runs without errors", {
  skip_on_cran()
  # Generate some test data
  data(dune)
  data(dune.env)
  AllModels <- make_models(vars = c("A1", "Moisture", "Manure"), ncores = 1)

  # Call the function
  res <- filter_vif(all_forms = AllModels,
                    env_data = dune.env,
                    ncores = 1,
                    filter = TRUE,
                    verbose = FALSE)

  # Check that the function returned a data.frame
  testthat::expect_true(is.data.frame(res))
})

# Test that the function filters out models with high collinearity
test_that("filter_vif filters out models with high collinearity", {
  # Generate some test data with high collinearity
  skip_on_cran()
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)
  x3 <- x1 + rnorm(n, sd = 0.1)
  y <- x1 + x2 + x3 + rnorm(n)
  env_data <- data.frame(x1, x2, x3, y)
  AllModels <- make_models(vars = c("x1", "x2", "x3"), ncores = 1)

  # Call the function with filter = TRUE
  res <- filter_vif(all_forms = AllModels,
                    env_data = env_data,
                    ncores = 2,
                    filter = TRUE,
                    verbose = FALSE)

  # Check that the filtered data.frame has fewer rows than the original
  testthat::expect_true(nrow(res) < nrow(AllModels))
})

# Test that the function flags models with high collinearity when filter = FALSE
test_that("filter_vif flags models with high collinearity when filter = FALSE", {
  skip_on_cran()
  # Generate some test data with high collinearity
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)
  x3 <- x1 + rnorm(n, sd = 0.1)
  y <- x1 + x2 + x3 + rnorm(n)
  env_data <- data.frame(x1, x2, x3, y)
  AllModels <- make_models(vars = c("x1", "x2", "x3"), ncores = 1)

  # Call the function with filter = FALSE
  res <- filter_vif(all_forms = AllModels,
                    env_data = env_data,
                    ncores = 2,
                    filter = FALSE,
                    verbose = FALSE)

  # Check that the flagged data.frame has a column called "collinearity"
  expect_true("collinearity" %in% colnames(res))
})

# Test that the function handles missing values correctly
test_that("filter_vif handles missing values correctly", {
  skip_on_cran()
  # Generate some test data with missing values
  data(dune)
  data(dune.env)
  env_data <- dune.env
  env_data[1, 1] <- NA
  AllModels <- make_models(vars = c("A1", "Moisture", "Manure"), ncores = 1)

  # Call the function


  # Test that the function correctly handles missing values
  testthat::expect_message(  res <- filter_vif(all_forms = AllModels,
                                               env_data = env_data,
                                               ncores = 2,
                                               filter = FALSE))
})
