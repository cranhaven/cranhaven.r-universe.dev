#' Create a histogram of sentiment scores
#'
#' This function creates a histogram of sentiment scores for all reviews.
#'
#' @param sentiment_df A data frame containing the output from analyze_sentiment.
#'
#' @return A ggplot object representing the histogram.
#'
#' @import ggplot2
#' @importFrom rlang sym !!
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the scrape_reviews function
#' reviews <- scrape_reviews(temp_file, num_reviews = 10, use_parallel = FALSE)
#'
#' # Check if reviews were successfully scraped
#' if (nrow(reviews) > 0) {
#'   # Perform sentiment analysis
#'   sentiment_results <- analyze_sentiment(reviews, lexicon = "afinn")
#'
#'   # Create histogram of sentiment scores
#'   sentiment_hist <- sentiment_histogram(sentiment_results)
#'
#'   # Display the plot
#'   print(sentiment_hist)
#'
#'   # Optionally, save the plot
#'   # ggsave("sentiment_hist.png", sentiment_hist, width = 8, height = 6)
#' } else {
#'   cat("No reviews found. Cannot create sentiment histogram.\n")
#' }
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
sentiment_histogram <- function(sentiment_df) {
  ggplot2::ggplot(sentiment_df, ggplot2::aes(x = !!rlang::sym("sentiment_score"))) +
    ggplot2::geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    ggplot2::labs(title = "Distribution of Sentiment Scores",
                  x = "Sentiment Score",
                  y = "Count") +
    ggplot2::theme_minimal()
}

#' Plot sentiment trend over time
#'
#' This function plots the average sentiment score over time.
#'
#' @param sentiment_df A data frame containing the output from analyze_sentiment.
#' @param time_period A string specifying the time period for grouping ("day", "week", "month", "year").
#' @param show_smooth_trend A logical value indicating whether to show the overall smooth trend line (default: TRUE).
#'
#' @return A ggplot object representing the sentiment trend.
#'
#' @import ggplot2 dplyr lubridate
#' @importFrom rlang sym !!
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the scrape_reviews function
#' reviews <- scrape_reviews(temp_file, num_reviews = 10, use_parallel = FALSE)
#'
#' # Check if reviews were successfully scraped
#' if (nrow(reviews) > 0) {
#'   # Perform sentiment analysis
#'   sentiment_results <- analyze_sentiment(reviews, lexicon = "afinn")
#'
#'   # Create histogram of sentiment scores
#'   senti_trend <- sentiment_trend(sentiment_results)
#'
#'   # Display the plot
#'   print(senti_trend)
#'
#'   # Optionally, save the plot
#'   # ggsave("senti_trend.png", senti_trend, width = 8, height = 6)
#' } else {
#'   cat("No reviews found. Cannot create sentiment trend\n")
#' }
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
sentiment_trend <- function(sentiment_df, time_period = "month", show_smooth_trend = FALSE) {
  # Convert review_date to Date type, handling the "Month Day, Year" format
  sentiment_df <- sentiment_df %>%
    dplyr::mutate(review_date = lubridate::mdy(.data$review_date))

  # Check if the conversion was successful
  if (all(is.na(sentiment_df$review_date))) {
    stop("Failed to parse review_date. Please ensure the dates are in the format 'Month Day, Year' (e.g., 'May 7, 2023').")
  }

  grouped_df <- sentiment_df %>%
    dplyr::mutate(period = dplyr::case_when(
      time_period == "day" ~ as.character(.data$review_date),
      time_period == "week" ~ as.character(lubridate::floor_date(.data$review_date, "week")),
      time_period == "month" ~ as.character(lubridate::floor_date(.data$review_date, "month")),
      time_period == "year" ~ as.character(lubridate::floor_date(.data$review_date, "year")),
      TRUE ~ as.character(lubridate::floor_date(.data$review_date, "month"))
    )) %>%
    dplyr::group_by(.data$period) %>%
    dplyr::summarize(avg_sentiment = mean(.data$sentiment_score, na.rm = TRUE))

  # Get the range of years for x-axis breaks
  year_range <- range(lubridate::year(as.Date(grouped_df$period)), na.rm = TRUE)

  # Check if year_range is valid
  if (any(is.infinite(year_range)) || any(is.na(year_range))) {
    stop("Unable to determine valid date range from the data.")
  }

  year_breaks <- seq(year_range[1], year_range[2], by = 1)

  plot <- ggplot2::ggplot(grouped_df, ggplot2::aes(x = as.Date(.data$period), y = .data$avg_sentiment)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = as.Date(c(paste0(year_range[1], "-01-01"), paste0(year_range[2], "-12-31")))
    ) +
    ggplot2::labs(title = paste("Sentiment Trend by", time_period),
                  x = "Time",
                  y = "Average Sentiment Score") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (show_smooth_trend) {
    plot <- plot +
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "solid")
  }

  return(plot)
}
