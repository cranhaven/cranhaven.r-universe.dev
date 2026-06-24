#' Preprocess review text for topic modeling
#'
#' This function preprocesses the review text by optionally filtering non-English reviews,
#' removing punctuation, converting to lowercase, removing stopwords, and stemming.
#'
#' @param reviews A data frame containing the scraped reviews
#' @param english_only A logical value indicating whether to filter out non-English reviews. Default is TRUE
#' @return A list containing the following elements:
#'   - `corpus`: The preprocessed corpus object.
#'   - `dtm`: The document-term matrix.
#'   - `filtered_reviews`: The filtered reviews data frame.
#'
#' @import tm stringr cld2
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Preprocess the reviews
#' preprocessed <- preprocess_reviews(reviews, english_only = TRUE)
#'
#' # Print the document-term matrix
#' print(preprocessed$dtm)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
preprocess_reviews <- function(reviews, english_only = TRUE) {
  if (english_only) {
    # Detect languages
    languages <- cld2::detect_language(reviews$review_content)
    # Filter English reviews
    english_indices <- which(languages == "en")
    reviews <- reviews[english_indices, ]
    if (nrow(reviews) == 0) {
      stop("No English reviews found after filtering.")
    }
  }

  # Create a custom preprocessing function
  preprocess_text <- function(x) {
    x <- tolower(x)
    x <- removePunctuation(x)
    x <- removeNumbers(x)
    x <- removeWords(x, stopwords("english"))
    x <- stripWhitespace(x)
    x <- stemDocument(x)
    return(x)
  }

  # Apply preprocessing directly to the review content
  preprocessed_content <- sapply(reviews$review_content, preprocess_text)

  # Remove empty documents
  non_empty <- which(nchar(preprocessed_content) > 0)
  preprocessed_content <- preprocessed_content[non_empty]
  reviews <- reviews[non_empty, ]

  # Create the corpus from preprocessed content
  corpus <- Corpus(VectorSource(preprocessed_content))

  # Create the DocumentTermMatrix
  dtm <- DocumentTermMatrix(corpus)

  list(corpus = corpus, dtm = dtm, filtered_reviews = reviews)
}

#' Perform topic modeling on preprocessed reviews
#'
#' This function performs LDA topic modeling on the preprocessed reviews.
#'
#' @param dtm A document-term matrix
#' @param k The number of topics to extract
#' @param method The method to use for fitting the model (default: Gibbs)
#' @return An LDA model
#'
#' @import topicmodels
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Preprocess the reviews
#' preprocessed <- preprocess_reviews(reviews, english_only = TRUE)
#'
#' # Fit LDA model
#' lda_model <- fit_lda(preprocessed$dtm, k = 2)
#'
#' # Print model summary
#' print(lda_model)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
fit_lda <- function(dtm, k, method = "Gibbs") {
  lda_model <- LDA(dtm, k = k, method = method)
  lda_model
}

#' Extract and print top terms for each topic
#'
#' This function extracts the top terms for each topic in the LDA model and optionally prints them.
#'
#' @param lda_model An LDA model
#' @param n The number of top terms to extract for each topic
#' @param verbose Logical; if TRUE, print the top terms to the console (default is TRUE)
#'
#' @return A list of character vectors, each containing the top terms for a topic.
#'
#' @import topicmodels
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Preprocess the reviews
#' preprocessed <- preprocess_reviews(reviews, english_only = TRUE)
#'
#' # Fit LDA model
#' lda_model <- fit_lda(preprocessed$dtm, k = 2)
#'
#' # Print top terms
#' top_terms(lda_model, n = 5)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
top_terms <- function(lda_model, n = 10, verbose = TRUE) {
  top_terms <- terms(lda_model, n)
  result <- vector("list", ncol(top_terms))

  for (i in 1:ncol(top_terms)) {
    result[[i]] <- top_terms[,i]
    if(verbose) {
      cat(paste0("Topic ", i, ": "), "\n")
      cat(paste(result[[i]], collapse = ", "), "\n")
      cat("\n")
    }
  }

  names(result) <- paste0("Topic_", 1:ncol(top_terms))
  return(result)
}

#' Analyze topics in Goodreads reviews
#'
#' This function takes the output from scrape_reviews, preprocesses the data,
#' performs topic modeling, and prints the results.
#'
#' @param reviews A data frame containing the scraped reviews
#' @param num_topics The number of topics to extract
#' @param num_terms The number of top terms to display for each topic
#' @param english_only A logical value indicating whether to filter out non-English reviews. Default is FALSE.
#'
#' @return A list containing the following elements:
#'   - `model`: The fitted LDA model object.
#'   - `filtered_reviews`: The preprocessed and filtered reviews data frame.
#'
#' @import tm topicmodels cld2
#' @export
#'
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#'
#' # Model topics
#' topic_results <- model_topics(reviews, num_topics = 2, num_terms = 5, english_only = TRUE)
#'
#' # Print model summary
#' print(topic_results$model)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
model_topics <- function(reviews, num_topics = 3, num_terms = 10, english_only = TRUE) {
  preprocessed <- preprocess_reviews(reviews, english_only = english_only)
  lda_model <- fit_lda(preprocessed$dtm, k = num_topics)
  top_terms(lda_model, n = num_terms)

  # Return a list containing the model and filtered reviews for further analysis if needed
  return(list(model = lda_model, filtered_reviews = preprocessed$filtered_reviews))
}
