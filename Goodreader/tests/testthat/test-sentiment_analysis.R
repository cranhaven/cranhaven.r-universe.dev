library(testthat)
library(Goodreader)  # Assuming your package is named Goodreader

# Create a sample dataset for testing
sample_reviews <- data.frame(
  book_id = c("1", "2", "3"),
  reviewer_id = c("A", "B", "C"),
  review_content = c(
    "This book was great. I loved it.",
    "Not a fan of this one. It was boring.",
    "An interesting read, but not amazing."
  ),
  review_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))
)

test_that("analyze_sentiment function works correctly with AFINN lexicon", {
  result <- analyze_sentiment(sample_reviews, lexicon = "afinn")
  expect_equal(nrow(result), 3)
  expect_true("sentiment_score" %in% names(result))
  expect_type(result$sentiment_score, "double")
})

test_that("analyze_sentiment function works correctly with Bing lexicon", {
  result <- analyze_sentiment(sample_reviews, lexicon = "bing")
  expect_equal(nrow(result), 3)
  expect_true("sentiment_score" %in% names(result))
  expect_true("positive" %in% names(result))
  expect_true("negative" %in% names(result))
})

test_that("analyze_sentiment function works correctly with NRC lexicon", {
  result <- analyze_sentiment(sample_reviews, lexicon = "nrc")
  expect_equal(nrow(result), 3)
  expect_gte(ncol(result), ncol(sample_reviews) + 1)  # At least one sentiment column
  # Check if any NRC sentiment categories are present
  nrc_sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
  expect_true(any(nrc_sentiments %in% names(result)))
  # Check if all new columns (except book_id, reviewer_id, and review_date) are numeric
  original_cols <- c("book_id", "reviewer_id", "review_content", "review_date")
  new_cols <- setdiff(names(result), original_cols)
  expect_true(all(sapply(result[new_cols], is.numeric)))
})

test_that("average_book_sentiment function works correctly", {
  sentiment_df <- analyze_sentiment(sample_reviews, lexicon = "afinn")
  result <- average_book_sentiment(sentiment_df)
  expect_equal(nrow(result), 3)
  expect_true("avg_sentiment" %in% names(result))
  expect_type(result$avg_sentiment, "double")
})
