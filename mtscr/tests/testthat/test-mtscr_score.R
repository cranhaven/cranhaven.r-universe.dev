data("mtscr_creativity", package = "mtscr")

# Test that `format` argument works as expected
# full
test_that("format argument works as expected for full", {
  # call function with format = "full"
  res_full <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3, format = "full")

  # check that res_full has the expected number of rows and columns
  expect_equal(ncol(res_full), ncol(mtscr_creativity) + 3)

  # check that res_full has the expected column names
  expect_named(res_full, c(names(mtscr_creativity), paste0(".creativity_score_top", 1:3)), ignore.order = TRUE)
})

# minimal
test_that("format argument works as expected for minimal", {
  # call function with format = "minimal"
  res_minimal <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3, format = "minimal")

  # check that res_wide has the expected number of rows and columns
  expect_equal(ncol(res_minimal), 4)

  # check that res_wide has the expected column names
  expect_named(
    res_minimal, c("id", paste0(".creativity_score_top", 1:3)),
    ignore.order = TRUE
  )
})

# Test that .creativity_score columns are numeric
test_that(".creativity_score columns are numeric", {
  # call function with top = 1
  res_top1 <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1)

  # check that .creativity_score_top1 column is numeric
  expect_type(res_top1$.creativity_score_top1, "double")

  # check that variance isn't 0
  expect_false(var(res_top1$.creativity_score_top1) == 0)

  # call function with top = 1:3
  res_top1to3 <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3)

  # check that .creativity_score_topX columns are numeric
  expect_type(res_top1to3$.creativity_score_top1, "double")
  expect_type(res_top1to3$.creativity_score_top2, "double")
  expect_type(res_top1to3$.creativity_score_top3, "double")

  # check that variance isn't 0
  expect_false(var(res_top1to3$.creativity_score_top1) == 0)
  expect_false(var(res_top1to3$.creativity_score_top2) == 0)
  expect_false(var(res_top1to3$.creativity_score_top3) == 0)
})

df <- data.frame(
  id = rep(1:2, each = 9),
  item = rep(letters[1:3], 2, each = 3),
  score = runif(18, 0, 1)
)

# Test that invalid format errors are thrown
test_that("invalid format error is thrown", {
  # call function with format = "invalid"
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, format = "invalid"))

  # call function with format = c("minimal", "full")
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, format = c("minimal", "invalid")))
})

# Test that top argument must be integer
test_that("top argument must be an integer", {
  # call function with top = "yes."
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = "yes."), regexp = "must be an integer")

  # call function with top = 1.5
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = 1.5), regexp = "must be an integer")

  # call function with top = -2
  expect_error(mtscr_score(mtscr_creativity, id, item, SemDis_MEAN, top = -2), regexp = "positive integer")
})

# Test that df must be a data frame
test_that("df must be a data frame", {
  # call function with a vector
  expect_error(mtscr_score(1:10, id, item, score), regexp = "must be a data frame.")
})

# Test that all columns exist in the data
# id_column
test_that("id_column must exist in the data", {
  # create a test data frame without the id column
  df_no_id <- df[, c("item", "score")]

  # call function with test data frame and no id column
  expect_error(mtscr_score(df_no_id, id, item, score), regexp = "does not exist.")
})

# item_column
test_that("item_column must exist in the data", {
  # create a test data frame without the item column
  df_no_item <- df[, c("id", "score")]

  # call function with test data frame and no item column
  expect_error(mtscr_score(df_no_item, id, item, score), regexp = "does not exist.")
})

# score_column
test_that("score_column must exist in the data", {
  # create a test data frame without the score column
  df_no_score <- df[, c("id", "item")]

  # call function with test data frame and no score column
  expect_error(mtscr_score(df_no_score, id, item, score), regexp = "does not exist.")
})

# Test that score_column must be numeric
test_that("score_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df_string_scores <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_score(df_string_scores, id, item, value), regexp = "must be numeric.")
})

# Test that works the same with quoted and unquoted column names
test_that("works the same with quoted and unquoted column names", {
  # call function with quoted column names
  res_quoted <- mtscr_score(mtscr_creativity, "id", "item", "SemDis_MEAN")

  # call function with unquoted column names
  res_unquoted <- mtscr_score(mtscr_creativity, id, item, SemDis_MEAN)

  # check that the two results are the same
  expect_equal(res_quoted, res_unquoted, tolerance = 0.0001)
})

# Test that works if item column is omitted
test_that("works if item column is omitted", {
  # create filtered dataframe
  df_filtered <- dplyr::filter(df, item == "a")

  # call function with filtered dataframe with item column
  result_item <- mtscr_score(df_filtered, id, item, score)

  # call function with filtered dataframe without item column
  result_no_item <- mtscr_score(df_filtered, id, score_column = score)

  # check that results are the same
  expect_equal(result_item, result_no_item)
})
