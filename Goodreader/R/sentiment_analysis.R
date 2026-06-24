#' Perform sentiment analysis on book reviews with negation handling
#'
#' This function takes the output from scrape_reviews and performs sentiment analysis,
#' including basic negation scope detection.
#'
#' @param reviews_df A data frame containing the output from scrape_reviews.
#' @param lexicon The sentiment lexicon to use. Options are "afinn", "bing", or "nrc".
#'
#' @return A data frame with sentiment scores for each review.
#'
#' @import dplyr tidyr tidytext stringr
#' @importFrom rlang sym !!
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the scrape_reviews function
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Check if reviews were successfully scraped
#' if (nrow(reviews) > 0) {
#'   # Perform sentiment analysis
#'   sentiment_results <- analyze_sentiment(reviews, lexicon = "afinn")
#'
#'   # Display the first few rows of the results
#'   print(head(sentiment_results))
#' } else {
#'   cat("No reviews found. Cannot perform sentiment analysis.\n")
#' }
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
analyze_sentiment <- function(reviews_df, lexicon = "afinn") {
  # Ensure the lexicon is valid
  if (!lexicon %in% c("afinn", "bing", "nrc")) {
    stop("Invalid lexicon. Choose 'afinn', 'bing', or 'nrc'.")
  }

  # Load the lexicon from the local RDS file
  lexicon_path <- system.file("extdata", paste0(lexicon, ".rds"), package = "Goodreader")
  if (lexicon_path == "") {
    stop(paste("Lexicon file", lexicon, ".rds not found. Make sure it's in the inst/extdata directory."))
  }
  sentiment_lex <- readRDS(lexicon_path)

  # Define negation words
  negation_words <- c("not", "no", "never", "without")

  # Tokenize the reviews and handle negations
  tokens <- reviews_df %>%
    dplyr::mutate(review_content = stringr::str_replace_all(!!rlang::sym("review_content"), "[[:punct:]]", " ")) %>%
    tidytext::unnest_tokens(word, !!rlang::sym("review_content")) %>%
    dplyr::mutate(word_index = dplyr::row_number()) %>%
    dplyr::group_by(!!rlang::sym("book_id"), !!rlang::sym("reviewer_id")) %>%
    dplyr::mutate(
      negation = !!rlang::sym("word") %in% negation_words,
      negation_index = cumsum(!!rlang::sym("negation")),
      word = dplyr::if_else(dplyr::lag(!!rlang::sym("negation"), default = FALSE) & !!rlang::sym("negation"), paste("NOT", !!rlang::sym("word")), !!rlang::sym("word"))
    ) %>%
    dplyr::ungroup()

  # Add negated words to the lexicon with inverted sentiment
  if (lexicon == "afinn") {
    negated_lex <- sentiment_lex %>%
      dplyr::mutate(
        word = paste("NOT", !!rlang::sym("word")),
        value = -!!rlang::sym("value")
      )
  } else {
    negated_lex <- sentiment_lex %>%
      dplyr::mutate(
        word = paste("NOT", !!rlang::sym("word")),
        sentiment = dplyr::case_when(
          !!rlang::sym("sentiment") == "positive" ~ "negative",
          !!rlang::sym("sentiment") == "negative" ~ "positive",
          TRUE ~ !!rlang::sym("sentiment")
        )
      )
  }

  sentiment_lex <- dplyr::bind_rows(sentiment_lex, negated_lex)

  # Join tokens with sentiment lexicon
  sentiment_scores <- dplyr::inner_join(tokens, sentiment_lex, by = "word")

  # Calculate sentiment scores
  if (lexicon == "afinn") {
    result <- sentiment_scores %>%
      dplyr::group_by(!!rlang::sym("book_id"), !!rlang::sym("reviewer_id")) %>%
      dplyr::summarize(sentiment_score = sum(!!rlang::sym("value"), na.rm = TRUE))
  } else if (lexicon == "bing") {
    result <- sentiment_scores %>%
      dplyr::count(!!rlang::sym("book_id"), !!rlang::sym("reviewer_id"), !!rlang::sym("sentiment")) %>%
      tidyr::spread(!!rlang::sym("sentiment"), !!rlang::sym("n"), fill = 0) %>%
      dplyr::mutate(sentiment_score = !!rlang::sym("positive") - !!rlang::sym("negative"))
  } else { # nrc
    result <- sentiment_scores %>%
      dplyr::count(!!rlang::sym("book_id"), !!rlang::sym("reviewer_id"), !!rlang::sym("sentiment")) %>%
      tidyr::spread(!!rlang::sym("sentiment"), !!rlang::sym("n"), fill = 0)
  }

  # Join the results back to the original data frame
  dplyr::left_join(reviews_df, result, by = c("book_id", "reviewer_id"))
}

#' Calculate average sentiment score per book
#'
#' This function calculates the average sentiment score for each book.
#'
#' @param sentiment_df A data frame containing the output from analyze_sentiment.
#'
#' @return A data frame with average sentiment scores for each book.
#'
#' @import dplyr
#' @importFrom rlang sym !!
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the scrape_reviews function
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Check if reviews were successfully scraped
#' if (nrow(reviews) > 0) {
#'   # Perform sentiment analysis
#'   sentiment_results <- analyze_sentiment(reviews, lexicon = "afinn")
#'
#'   # Calculate average sentiment score per book
#'   avg_senti <- average_book_sentiment(sentiment_results)
#'
#'   # Display the results
#'   print(avg_senti)
#' } else {
#'   cat("No reviews found. Cannot calculate average sentiment.\n")
#' }
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
average_book_sentiment <- function(sentiment_df) {
  sentiment_df %>%
    dplyr::group_by(!!rlang::sym("book_id")) %>%
    dplyr::summarize(avg_sentiment = mean(!!rlang::sym("sentiment_score"), na.rm = TRUE))
}
