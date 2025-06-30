set.seed(123)

df <- data.frame(
  decimalLongitude = runif(50, -5, 5),
  decimalLatitude = runif(50, -5, 5),
  pa = sample(0:1, 50, replace = TRUE),
  timestamp = sample(2000:2004, 50, replace = TRUE)
)
residual_df <- df
residual_df$residual <- rnorm(nrow(df))

r1 <- terra::rast(nrows=10, ncols=10, xmin=-5, xmax=5, ymin=-5, ymax=5)
values(r1) <- runif(ncell(r1), 0, 1)
r2 <- terra::rast(nrows=10, ncols=10, xmin=-5, xmax=5, ymin=-5, ymax=5)
values(r2) <- runif(ncell(r2), 0, 1)
pred_stack <- c(r1, r2)
terra::crs(pred_stack) <- "epsg:4326"
names(pred_stack) <- c("var1", "var2")

test_that("spatial_blocks with residuals_autocorrelation works", {
  result <- generate_cv_folds(
    data = df,
    method = "spatial_blocks",
    block_method = "residuals_autocorrelation",
    model_residuals = residual_df,
    k = 2
  )

  expect_type(result$folds, "integer")
  expect_length(result$folds, nrow(df))
  expect_true(result$block_size > 0)
  expect_equal(result$block_method, "residuals_autocorrelation")
})

test_that("spatial_blocks with predictors_autocorrelation works", {
  result <- generate_cv_folds(
    data = df,
    method = "spatial_blocks",
    block_method = "predictors_autocorrelation",
    predictor_raster = pred_stack,
    k = 2
  )

  expect_type(result$folds, "integer")
  expect_length(result$folds, nrow(df))
  expect_true(result$block_size > 0)
  expect_equal(result$block_method, "predictors_autocorrelation")
})

test_that("k-fold generates valid folds", {
  set.seed(123)
  df <- data.frame(pa = sample(0:1, 20, replace = TRUE),
                   decimalLongitude = runif(20, -5, 5),
                   decimalLatitude = runif(20, -5, 5))

  res <- generate_cv_folds(df, method = "k-fold", k = 4)

  expect_type(res, "list")
  expect_equal(length(res$folds), nrow(df))
  expect_equal(length(unique(res$folds)), 4)
  expect_true(all(res$folds %in% 1:4))
})

test_that("manual spatial_blocks returns valid folds", {
  df <- data.frame(pa = sample(0:1, 30, replace = TRUE),
                   decimalLongitude = runif(30, -5, 5),
                   decimalLatitude = runif(30, -5, 5))

  res <- generate_cv_folds(df, method = "spatial_blocks",
                           block_method = "manual",
                           block_size = 100000,  # 100 km
                           k = 3)

  expect_type(res, "list")
  expect_equal(length(res$folds), nrow(df))
  expect_true(all(res$folds %in% 1:3))
})

test_that("temporal_blocks respects timestamps", {
  df <- data.frame(pa = sample(0:1, 15, replace = TRUE),
                   decimalLongitude = runif(15, -2, 2),
                   decimalLatitude = runif(15, -2, 2),
                   timestamp = rep(2000:2004, each = 3))

  res <- generate_cv_folds(df, method = "temporal_blocks", k = 3)

  expect_type(res, "list")
  expect_equal(length(res$folds), nrow(df))
  expect_equal(length(unique(res$folds)), 3)
})

test_that("temporal_blocks fails with too few timestamps", {
  df <- data.frame(pa = sample(0:1, 10, replace = TRUE),
                   decimalLongitude = runif(10),
                   decimalLatitude = runif(10),
                   timestamp = rep(1, 10))

  expect_error(
    generate_cv_folds(df, method = "temporal_blocks", k = 3),
    "Not enough unique timestamps"
  )
})

test_that("cross_validate_model runs", {
  set.seed(123)
  n <- 30
  data <- data.frame(
    pa = rep(c(0, 1), each = n/2),
    var1 = rnorm(n),
    var2 = runif(n),
    decimalLongitude = runif(n, -10, 10),
    decimalLatitude = runif(n, -10, 10),
    timestamp = sample(1:3, n, replace = TRUE)
  )
  folds <- sample(1:3, n, replace = TRUE)

  # Run CV
  result <- cross_validate_model(data = data, folds = folds, predictor_cols = c("var1", "var2"), seed = 101)

  expect_type(result, "list")
  expect_named(result, c("metrics", "predictions"))
  expect_s3_class(result$metrics, "data.frame")
  expect_true(all(c("TP", "FP", "FN", "TN", "Fscore", "TSS", "status") %in% colnames(result$metrics)))
  expect_equal(nrow(result$metrics), length(unique(folds)))
  expect_s3_class(result$predictions, "data.frame")
  expect_true(all(c("pa", "predicted", "probability", "fold") %in% colnames(result$predictions)))
  expect_equal(nrow(result$predictions), sum(result$metrics$status == "valid", na.rm = TRUE) * n / 3, tolerance = 5)
  expect_true(all(result$predictions$probability >= 0 & result$predictions$probability <= 1))
})

test_that("cross_validate_model skips folds with only presences or absences", {
  data <- data.frame(
    pa = c(rep(1, 10), rep(0, 5)),
    var1 = rnorm(15),
    var2 = runif(15),
    decimalLongitude = runif(15, -5, 5),
    decimalLatitude = runif(15, -5, 5),
    timestamp = sample(1:2, 15, replace = TRUE)
  )
  folds <- c(rep(1, 5), rep(2, 5), rep(3, 5))  # Folds with all 1s or 0s

  result <- suppressWarnings(cross_validate_model(data = data, folds = folds, predictor_cols = c("var1", "var2")))

  expect_true(any(result$metrics$status == "skipped"))
})

test_that("cross_validate_model is reproducible with seed", {
  data <- data.frame(
    pa = rep(c(0, 1), each = 10),
    var1 = rnorm(20),
    var2 = runif(20),
    decimalLongitude = runif(20, -5, 5),
    decimalLatitude = runif(20, -5, 5),
    timestamp = sample(1:2, 20, replace = TRUE)
  )
  folds <- sample(1:2, 20, replace = TRUE)

  r1 <- cross_validate_model(data, folds, c("var1", "var2"), seed = 42)
  r2 <- cross_validate_model(data, folds, c("var1", "var2"), seed = 42)

  expect_equal(r1$metrics, r2$metrics)
  expect_equal(r1$predictions, r2$predictions)
})

