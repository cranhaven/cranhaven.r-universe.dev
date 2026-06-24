# test_scrape_books.R
library(testthat)
library(httr)
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
library(parallel)

# Mock data
mock_book_ids <- c(1234, 5678)
mock_html <- '<html><body>
  <h1 data-testid="bookTitle">Test Book</h1>
  <div class="DetailsLayoutRightParagraph">Book details</div>
  <div class="FeaturedDetails">
    <p data-testid="publicationInfo">Published 2023</p>
    <p data-testid="pagesFormat">300 pages, Paperback</p>
  </div>
  <a class="ContributorLink" href="/author/show/12345">
    <span class="ContributorLink__name">Test Author</span>
  </a>
  <div data-testid="genresList">
    <a class="Button--tag-inline">Fiction</a>
    <a class="Button--tag-inline">Drama</a>
  </div>
  <span data-testid="ratingsCount">1000 ratings</span>
  <span data-testid="reviewsCount">500 reviews</span>
  <div class="RatingStatistics__rating">4.5</div>
  <div class="RatingsHistogram__bar" aria-label="5 stars">
    <div class="RatingsHistogram__labelTotal">500</div>
  </div>
</body></html>'

# Mock functions
mock_GET <- function(url) {
  structure(
    list(
      status_code = 200,
      content = charToRaw(mock_html)
    ),
    class = "response"
  )
}

test_that("scrape_books function works correctly", {
  skip_on_cran()
  # Create a temporary file with mock book IDs
  temp_file <- tempfile()
  writeLines(as.character(mock_book_ids), temp_file)

  # Mock the necessary functions
  with_mock(
    `httr::GET` = mock_GET,
    {
      result <- scrape_books(temp_file)
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), length(mock_book_ids))
      expect_equal(result$book_id, as.character(mock_book_ids))
      expect_equal(result$book_title, rep("Test Book", length(mock_book_ids)))
      expect_equal(result$author, rep("Test Author", length(mock_book_ids)))
      expect_equal(result$num_pages, rep("300", length(mock_book_ids)))
      expect_equal(result$genres, rep("Fiction, Drama", length(mock_book_ids)))
      expect_equal(result$num_ratings, rep("1000", length(mock_book_ids)))
      expect_equal(result$num_reviews, rep("500", length(mock_book_ids)))
      expect_equal(result$average_rating, rep("4.5", length(mock_book_ids)))
    }
  )

  # Clean up the temporary file
  unlink(temp_file)
})
