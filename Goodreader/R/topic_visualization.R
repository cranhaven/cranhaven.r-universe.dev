#' @importFrom utils globalVariables
utils::globalVariables(c(".", "book_id"))

#' Replace special characters and remove non-ASCII characters
#'
#' @param x A character vector
#' @return A character vector with special characters replaced and non-ASCII characters removed
replace_special_chars <- function(x) {
  x <- gsub("\u2013", "-", x)  # Replace em dash with hyphen
  x <- iconv(x, "UTF-8", "ASCII", sub = "")  # Remove non-ASCII characters
  return(x)
}

#' Visualize top terms for each topic
#'
#' This function creates a bar plot of the top terms for each topic.
#'
#' @param model_output The output from model_topics function
#' @param n The number of top terms to visualize for each topic
#'
#' @return A ggplot object representing the bar plot of top terms for each topic.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap coord_flip labs theme_minimal theme element_text
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data !!
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 10, use_parallel = FALSE)
#'
#' # Model topics
#' topic_results <- model_topics(reviews, num_topics = 2, num_terms = 5, english_only = TRUE)
#'
#' # Visualize top terms for each topic
#' plot_topic_terms(topic_results, n = 5)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
plot_topic_terms <- function(model_output, n = 10) {
  lda_model <- model_output$model

  beta <- exp(lda_model@beta)
  terms <- lda_model@terms
  top_terms <- apply(beta, 1, function(x) terms[order(x, decreasing = TRUE)][1:n])
  top_probs <- apply(beta, 1, function(x) sort(x, decreasing = TRUE)[1:n])

  topic_term_df <- data.frame(
    topic = rep(1:nrow(beta), each = n),
    term = as.vector(top_terms),
    probability = as.vector(top_probs)
  )

  ggplot2::ggplot(topic_term_df, ggplot2::aes(x = stats::reorder(.data$term, .data$probability),
                                              y = .data$probability,
                                              fill = factor(.data$topic))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ .data$topic, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Terms", y = "Probability", title = "Top Terms by Topic") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
}

#' Visualize topic distribution
#'
#' This function creates a heatmap of the topic distribution across documents.
#'
#' @param model_output The output from model_topics function
#'
#' @return A ggplot object representing the topic distribution heatmap.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient labs theme_minimal theme element_blank
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 10, use_parallel = FALSE)
#'
#' # Model topics
#' topic_results <- model_topics(reviews, num_topics = 2, num_terms = 5, english_only = TRUE)
#'
#' # Visualize topic distribution
#' plot_topic_heatmap(topic_results)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
plot_topic_heatmap <- function(model_output) {
  lda_model <- model_output$model
  filtered_reviews <- model_output$filtered_reviews

  topic_probs <- lda_model@gamma
  topic_probs_df <- as.data.frame(topic_probs)
  colnames(topic_probs_df) <- paste0("Topic", 1:ncol(topic_probs_df))

  topic_probs_df$book_id <- replace_special_chars(filtered_reviews$book_id)

  topic_probs_long <- tidyr::pivot_longer(topic_probs_df,
                                          cols = -book_id,
                                          names_to = "Topic",
                                          values_to = "Probability")

  ggplot2::ggplot(topic_probs_long, ggplot2::aes(x = .data$Topic, y = .data$book_id, fill = .data$Probability)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::labs(x = "Topics", y = "Book ID", title = "Topic Distribution Across Books") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
}

#' Visualize topic prevalence
#'
#' This function creates a bar plot of the overall prevalence of each topic.
#'
#' @param model_output The output from model_topics function
#'
#' @return A ggplot object representing the bar plot of topic prevalence.
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 10, use_parallel = FALSE)
#'
#' # Model topics
#' topic_results <- model_topics(reviews, num_topics = 2, num_terms = 5, english_only = TRUE)
#'
#' # Visualize topic distribution
#' plot_topic_prevalence(topic_results)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
plot_topic_prevalence <- function(model_output) {
  lda_model <- model_output$model

  topic_probs <- lda_model@gamma
  topic_prevalence <- colMeans(topic_probs)
  topic_prevalence_df <- data.frame(
    Topic = factor(1:length(topic_prevalence)),
    Prevalence = topic_prevalence
  )

  ggplot2::ggplot(topic_prevalence_df, ggplot2::aes(x = .data$Topic, y = .data$Prevalence, fill = .data$Topic)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Topic", y = "Prevalence", title = "Overall Topic Prevalence") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' Create word cloud for topics
#'
#' This function creates a word cloud for each topic.
#'
#' @param model_output The output from model_topics function
#' @param n The number of top terms to include in the word cloud
#'
#' @return A list of ggplot objects, where each element represents a word cloud for a topic.
#'
#' @importFrom wordcloud2 wordcloud2
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Scrape reviews
#' reviews <- scrape_reviews(temp_file, num_reviews = 30, use_parallel = FALSE)
#'
#' # Model topics
#' topic_results <- model_topics(reviews, num_topics = 3, num_terms = 50, english_only = TRUE)
#'
#' # Generate word clouds for each topic
#' wordcloud_plots <- gen_topic_clouds(topic_results, n = 20)
#'
#' # Display the word cloud for the first topic
#' if (interactive()) {
#'   print(wordcloud_plots[[1]])
#' }
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
gen_topic_clouds <- function(model_output, n = 50) {
  lda_model <- model_output$model

  beta <- exp(lda_model@beta)
  terms <- lda_model@terms

  wordcloud_data <- lapply(1:nrow(beta), function(i) {
    topic_terms <- sort(beta[i,], decreasing = TRUE)
    data.frame(word = terms[order(beta[i,], decreasing = TRUE)][1:n],
               freq = sort(topic_terms, decreasing = TRUE)[1:n],
               topic = i)
  })

  wordcloud_data <- dplyr::bind_rows(wordcloud_data)

  # Create a list to store wordcloud plots
  wordcloud_plots <- lapply(1:nrow(beta), function(topic) {
    topic_data <- wordcloud_data[wordcloud_data$topic == topic, ]
    wordcloud2::wordcloud2(topic_data[, c("word", "freq")], size = 0.5, shape = "circle")
  })

  return(wordcloud_plots)
}
