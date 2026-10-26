# ==============================================================================
# Coverage tests for matching_utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# validate_matching_inputs
# ------------------------------------------------------------------------------

test_that("validate_matching_inputs errors on non-data frame left", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(list(x = 1:3), data.frame(x = 1:3)),
    "left must be a data frame"
  )
})

test_that("validate_matching_inputs errors on non-data frame right", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(data.frame(x = 1:3), matrix(1:3)),
    "right must be a data frame"
  )
})

test_that("validate_matching_inputs errors on empty left", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(data.frame(x = numeric(0)), data.frame(x = 1:3)),
    "left must have at least one row"
  )
})

test_that("validate_matching_inputs errors on empty right", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(data.frame(x = 1:3), data.frame(x = numeric(0))),
    "right must have at least one row"
  )
})

test_that("validate_matching_inputs errors on missing left vars", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3),
      data.frame(x = 1:3, y = 1:3),
      vars = c("x", "y")
    ),
    "left is missing required variables"
  )
})

test_that("validate_matching_inputs errors on missing right vars", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3, y = 1:3),
      data.frame(x = 1:3),
      vars = c("x", "y")
    ),
    "right is missing required variables"
  )
})

test_that("validate_matching_inputs errors on non-numeric left var", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = letters[1:3]),
      data.frame(x = 1:3),
      vars = "x"
    ),
    "Variable 'x' in left must be numeric"
  )
})

test_that("validate_matching_inputs errors on non-numeric right var", {
  skip_on_cran()
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3),
      data.frame(x = letters[1:3]),
      vars = "x"
    ),
    "Variable 'x' in right must be numeric"
  )
})

test_that("validate_matching_inputs passes for valid inputs", {
  skip_on_cran()
  expect_true(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3, y = 4:6),
      data.frame(x = 7:9, y = 10:12),
      vars = c("x", "y")
    )
  )
})

test_that("validate_matching_inputs passes without vars", {
  skip_on_cran()
  expect_true(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3),
      data.frame(x = 7:9)
    )
  )
})

# ------------------------------------------------------------------------------
# extract_ids
# ------------------------------------------------------------------------------

test_that("extract_ids uses id column when present", {
  skip_on_cran()
  df <- data.frame(id = c("a", "b", "c"), x = 1:3)
  ids <- couplr:::extract_ids(df, prefix = "test")
  expect_equal(ids, c("a", "b", "c"))
})

test_that("extract_ids uses row names when meaningful", {
  skip_on_cran()
  df <- data.frame(x = 1:3)
  rownames(df) <- c("row_a", "row_b", "row_c")
  ids <- couplr:::extract_ids(df, prefix = "test")
  expect_equal(ids, c("row_a", "row_b", "row_c"))
})

test_that("extract_ids creates sequential IDs when needed", {
  skip_on_cran()
  df <- data.frame(x = 1:3)
  ids <- couplr:::extract_ids(df, prefix = "unit")
  expect_equal(ids, c("unit_1", "unit_2", "unit_3"))
})

# ------------------------------------------------------------------------------
# extract_matching_vars
# ------------------------------------------------------------------------------

test_that("extract_matching_vars returns matrix", {
  skip_on_cran()
  df <- data.frame(x = 1:3, y = 4:6)
  mat <- couplr:::extract_matching_vars(df, c("x", "y"))
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(3, 2))
})

test_that("extract_matching_vars errors on NA", {
  skip_on_cran()
  df <- data.frame(x = c(1, NA, 3))
  expect_error(
    couplr:::extract_matching_vars(df, "x"),
    "Missing values"
  )
})

test_that("extract_matching_vars errors on NaN", {
  skip_on_cran()
  # NaN triggers "Missing values" first because is.na(NaN) is TRUE
  df <- data.frame(x = c(1, NaN, 3))
  expect_error(
    couplr:::extract_matching_vars(df, "x"),
    "Missing values"
  )
})

test_that("extract_matching_vars errors on Inf", {
  skip_on_cran()
  df <- data.frame(x = c(1, Inf, 3))
  expect_error(
    couplr:::extract_matching_vars(df, "x"),
    "Infinite values"
  )
})

# ------------------------------------------------------------------------------
# get_block_id_column
# ------------------------------------------------------------------------------

test_that("get_block_id_column finds block_id", {
  skip_on_cran()
  df <- data.frame(block_id = 1:3, x = 4:6)
  expect_equal(couplr:::get_block_id_column(df), "block_id")
})

test_that("get_block_id_column finds blockid", {
  skip_on_cran()
  df <- data.frame(blockid = 1:3, x = 4:6)
  expect_equal(couplr:::get_block_id_column(df), "blockid")
})

test_that("get_block_id_column finds block", {
  skip_on_cran()
  df <- data.frame(block = 1:3, x = 4:6)
  expect_equal(couplr:::get_block_id_column(df), "block")
})

test_that("get_block_id_column finds stratum", {
  skip_on_cran()
  df <- data.frame(stratum = 1:3, x = 4:6)
  expect_equal(couplr:::get_block_id_column(df), "stratum")
})

test_that("get_block_id_column finds stratum_id", {
  skip_on_cran()
  df <- data.frame(stratum_id = 1:3, x = 4:6)
  expect_equal(couplr:::get_block_id_column(df), "stratum_id")
})

test_that("get_block_id_column returns NULL when no block column", {
  skip_on_cran()
  df <- data.frame(x = 1:3, y = 4:6)
  expect_null(couplr:::get_block_id_column(df))
})

# ------------------------------------------------------------------------------
# has_blocks
# ------------------------------------------------------------------------------

test_that("has_blocks returns TRUE when block column exists", {
  skip_on_cran()
  df <- data.frame(block_id = 1:3, x = 4:6)
  expect_true(couplr:::has_blocks(df))
})

test_that("has_blocks returns FALSE when no block column", {
  skip_on_cran()
  df <- data.frame(x = 1:3, y = 4:6)
  expect_false(couplr:::has_blocks(df))
})

# ------------------------------------------------------------------------------
# validate_weights
# ------------------------------------------------------------------------------

test_that("validate_weights returns ones for NULL", {
  skip_on_cran()
  result <- couplr:::validate_weights(NULL, c("x", "y", "z"))
  expect_equal(result, c(1, 1, 1))
})

test_that("validate_weights accepts numeric vector", {
  skip_on_cran()
  result <- couplr:::validate_weights(c(2, 3), c("x", "y"))
  expect_equal(result, c(2, 3))
})

test_that("validate_weights errors on wrong length", {
  skip_on_cran()
  expect_error(
    couplr:::validate_weights(c(1, 2, 3), c("x", "y")),
    "weights must have length"
  )
})

test_that("validate_weights errors on negative weights", {
  skip_on_cran()
  expect_error(
    couplr:::validate_weights(c(1, -1), c("x", "y")),
    "weights must be non-negative"
  )
})

test_that("validate_weights accepts named weights", {
  skip_on_cran()
  result <- couplr:::validate_weights(c(x = 2, y = 3), c("x", "y"))
  expect_equal(as.numeric(result), c(2, 3))
})

test_that("validate_weights errors on unknown variable in list weights", {
  skip_on_cran()
  # Named list weights validate unknown variables
  expect_error(
    couplr:::validate_weights(list(x = 1, z = 2), c("x", "y")),
    "unknown variable"
  )
})

test_that("validate_weights accepts list weights", {
  skip_on_cran()
  result <- couplr:::validate_weights(list(x = 2, y = 3), c("x", "y"))
  expect_equal(as.numeric(result), c(2, 3))
})

test_that("validate_weights errors on invalid type", {
  skip_on_cran()
  expect_error(
    couplr:::validate_weights("invalid", c("x", "y")),
    "must be a numeric vector or named list"
  )
})

# ------------------------------------------------------------------------------
# validate_calipers
# ------------------------------------------------------------------------------

test_that("validate_calipers returns NULL for NULL", {
  skip_on_cran()
  expect_null(couplr:::validate_calipers(NULL, c("x", "y")))
})

test_that("validate_calipers accepts named vector", {
  skip_on_cran()
  result <- couplr:::validate_calipers(c(x = 0.1), c("x", "y"))
  expect_equal(result, c(x = 0.1))
})

test_that("validate_calipers accepts list", {
  skip_on_cran()
  result <- couplr:::validate_calipers(list(x = 0.1), c("x", "y"))
  expect_equal(result, list(x = 0.1))
})

test_that("validate_calipers errors on non-list non-numeric", {
  skip_on_cran()
  expect_error(
    couplr:::validate_calipers("invalid", c("x", "y")),
    "must be a named numeric vector or list"
  )
})

test_that("validate_calipers errors on unnamed", {
  skip_on_cran()
  expect_error(
    couplr:::validate_calipers(c(0.1, 0.2), c("x", "y")),
    "must be named"
  )
})

test_that("validate_calipers errors on unknown variable", {
  skip_on_cran()
  expect_error(
    couplr:::validate_calipers(c(z = 0.1), c("x", "y")),
    "unknown variables"
  )
})

test_that("validate_calipers errors on non-positive values", {
  skip_on_cran()
  expect_error(
    couplr:::validate_calipers(c(x = 0), c("x", "y")),
    "must be positive"
  )
})

test_that("validate_calipers errors on negative values", {
  skip_on_cran()
  expect_error(
    couplr:::validate_calipers(c(x = -0.1), c("x", "y")),
    "must be positive"
  )
})
