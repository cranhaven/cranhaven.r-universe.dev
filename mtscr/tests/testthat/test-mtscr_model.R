data("mtscr_creativity", package = "mtscr")
model <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1)

# Test the function with the test data
test_that("mtscr_model() works as expected", {
  # Test that the function returns a glmmTMB object
  expect_s3_class(model, "glmmTMB")

  # Test that the function returns the expected number of fixed and random effects

  expect_equal(length(glmmTMB::fixef(model)), 3)
  expect_equal(length(glmmTMB::ranef(model)), 2)
})

# Test that `prepared` argument works as expected
test_that("prepared argument works as expected", {
  # call function with prepared = TRUE
  res_prepared <- mtscr_creativity |>
    mtscr_prepare(id, item, SemDis_MEAN) |>
    mtscr_model(id, item, SemDis_MEAN, prepared = TRUE)

  # call function with prepared = FALSE
  res_unprepared <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, prepared = FALSE)

  # check that res_prepared and res_unprepared are the same
  expect_equal(glmmTMB::fixef(res_prepared), glmmTMB::fixef(res_unprepared), tolerance = 0.001)
  expect_equal(glmmTMB::ranef(res_prepared), glmmTMB::ranef(res_unprepared), tolerance = 0.001)
})

# Test that `top` argument works as expected
test_that("model_type argument works as expected", {
  # call function with top = 1
  res_all_max <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1)

  # call function with top = 2
  res_all_top2 <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 2)

  # call function with top = 1:2
  res_both <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:2)

  # check that res_all_max is a glmmTMB object
  expect_s3_class(res_all_max, "glmmTMB")

  # check that res_all_top2 is a glmmTMB object
  expect_s3_class(res_all_top2, "glmmTMB")

  # check that res_both is a list
  expect_true(is.list(res_both))

  # check that res_both has two elements
  expect_equal(length(res_both), 2)

  # check that res_both has the expected names
  expect_equal(names(res_both), paste0("top", 1:2))
})

# Test that `top` argument throws an error when invalid values are provided
test_that("top argument throws an error when invalid values are provided", {
  # call function with top = "invalid"
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = "invalid"))

  # call function with top = c(1, "invalid")
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = c(1, "invalid")))
})

# Test that top argument must be integer
test_that("top argument must be an integer", {
  # call function with top = "yes."
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = "yes."), regexp = "must be an integer")

  # call function with top = 1.5
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1.5), regexp = "must be an integer")

  # call function with top = -2
  expect_error(mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = -2), regexp = "positive integer")
})

df <- data.frame(
  id = rep(1:2, each = 9),
  item = rep(letters[1:3], 2, each = 3),
  score = runif(18, 0, 1)
)

# Test that df must be a data frame
test_that("df must be a data frame", {
  # call function with a vector
  expect_error(mtscr_model(1:10, id, item, score), regexp = "must be a data frame.")
})

# Test that all columns exist in the data
# id_column
test_that("id_column must exist in the data", {
  # create a test data frame without the id column
  df_no_id <- df[, c("item", "score")]

  # call function with test data frame and no id column
  expect_error(mtscr_model(df_no_id, id, item, score), regexp = "does not exist.")
})

# item_column
test_that("item_column must exist in the data", {
  # create a test data frame without the item column
  df_no_item <- df[, c("id", "score")]

  # call function with test data frame and no item column
  expect_error(mtscr_model(df_no_item, id, item, score), regexp = "does not exist.")
})

# score_column
test_that("score_column must exist in the data", {
  # create a test data frame without the score column
  df_no_score <- df[, c("id", "item")]

  # call function with test data frame and no score column
  expect_error(mtscr_model(df_no_score, id, item, score), regexp = "does not exist.")
})

# Test that score_column must be numeric
test_that("score_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df_string_scores <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_model(df_string_scores, id, item, value), regexp = "must be numeric.")
})

# Test that warning is thrown when data is unprepared and prepared = TRUE
test_that("warning is thrown when data is unprepared and prepared = TRUE", {
  # call function with prepared = TRUE
  expect_warning(mtscr_model(df, id, item, score, prepared = TRUE))
})

# Test that no error when there's only one item type
test_that("no error when there's only one item type", {
  # create a test data frame with only one item type
  df_one_item <- data.frame(id = c(1, 2), item = c("apple", "apple"), score = c(0.5, 0.6))

  # call function with test data frame
  expect_no_error(mtscr_model(df_one_item, id, item, score))
})

# Test that works the same with quoted and unquoted column names
test_that("works the same with quoted and unquoted column names", {
  # call function with quoted column names
  res_quoted <- mtscr_model(mtscr_creativity, "id", "item", "SemDis_MEAN")

  # call function with unquoted column names
  res_unquoted <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN)

  # check that the two results are the same
  expect_equal(mtscr_model_summary(res_quoted), mtscr_model_summary(res_unquoted), tolerance = 0.0001)
})

# Test that works if item column is omitted
test_that("works if item column is omitted", {
  # create filtered dataframe
  df_filtered <- dplyr::filter(df, item == "a")

  # call function with filtered dataframe with item column
  result_item <- mtscr_model(df_filtered, id, item, score)

  # call function with filtered dataframe without item column
  result_no_item <- mtscr_model(df_filtered, id, score_column = score)

  # check that results are the same
  expect_equal(glmmTMB::ranef(result_item), glmmTMB::ranef(result_no_item))
})

# Test that top1 models are the same when top = 1 and top = 1:2
test_that("top1 models are the same when top = 1 and top = 1:2", {
  # call function with top = 1
  res_top1 <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1)

  # call function with top = 1:2
  res_top1_3 <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:2)

  # check that the two results are the same
  expect_equal(mtscr_model_summary(res_top1), mtscr_model_summary(res_top1_3[["top1"]]), tolerance = 0.0001)
})
