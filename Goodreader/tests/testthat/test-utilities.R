# Load required libraries
library(rvest)
library(stringr)
library(testthat)

# Create a sample data frame with book IDs
sample_books <- data.frame(
  book_id = c("1420", "2767052", "10210", "6148028", "11127")
)

# Write book IDs to a file for testing
writeLines(sample_books$book_id, "test_book_ids.txt")

# Test suite
test_that("Goodreads scraping functions work correctly", {
  # These tests are skipped on CRAN because they require an internet connection
  # and rely on the structure of an external website (Goodreads) which may change.
  skip_on_cran()

  # Skip long-running tests if the environment variable is set
  if (Sys.getenv("SKIP_LONG_TESTS") == "true") {
    skip("Skipping long-running tests")
  }

  # Test get_book_ids function
  test_that("get_book_ids creates a file", {
    get_book_ids(sample_books, "test_book_ids_output.txt")
    expect_true(file.exists("test_book_ids_output.txt"))
  })

  # Test get_book_summary function
  test_that("get_book_summary returns correct structure", {
    summaries <- get_book_summary("test_book_ids.txt")
    expect_equal(length(summaries), 5)
    expect_true(all(sapply(summaries, is.character)))
  })

  # Test get_format_info function
  test_that("get_format_info returns correct structure", {
    format_info <- get_format_info("test_book_ids.txt")
    expect_equal(length(format_info), 5)
    expect_true(all(sapply(format_info, is.character)))
  })

  # Test get_genres function
  test_that("get_genres returns correct structure", {
    genres <- get_genres("test_book_ids.txt")
    expect_equal(length(genres), 5)
    expect_true(all(sapply(genres, is.character)))
  })

  # Test get_author_info function
  test_that("get_author_info returns correct structure", {
    author_info <- get_author_info("test_book_ids.txt")
    expect_equal(length(author_info), 5)
    expect_true(all(sapply(author_info, function(x) inherits(x, "xml_node") || is.na(x))))
  })

  # Test get_num_pages function
  test_that("get_num_pages returns correct structure", {
    num_pages <- get_num_pages("test_book_ids.txt")
    expect_equal(length(num_pages), 5)
    expect_true(all(sapply(num_pages, function(x) is.character(x) || is.na(x))))
  })

  # Test get_published_time function
  test_that("get_published_time returns correct structure", {
    published_time <- get_published_time("test_book_ids.txt")
    expect_equal(length(published_time), 5)
    expect_true(all(sapply(published_time, is.character)))
  })

  # Test get_rating_distribution function
  test_that("get_rating_distribution returns correct structure", {
    rating_distribution <- get_rating_distribution("test_book_ids.txt")
    expect_equal(length(rating_distribution), 5)
    expect_true(all(sapply(rating_distribution, is.list)))
    expect_true(all(sapply(rating_distribution, function(x) all(names(x) %in% as.character(1:5)))))
    expect_true(all(sapply(rating_distribution, function(x) all(sapply(x, is.character)))))
  })
})
