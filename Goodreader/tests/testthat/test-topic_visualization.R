# test_topic_visualization.R

library(testthat)

# Create a sample dataset for testing
sample_reviews <- data.frame(
  book_id = c("1", "2", "3"),
  reviewer_id = c("A", "B", "C"),
  review_content = c(
    "This book was great. I loved the characters and plot.",
    "Not a fan of this one. The story was too slow.",
    "An interesting read with complex themes."
  )
)

model_output <- model_topics(sample_reviews, num_topics = 2, num_terms = 5, english_only = FALSE)

test_that("plot_topic_terms function returns a ggplot object", {
  plot <- plot_topic_terms(model_output, n = 5)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_topic_heatmap function returns a ggplot object", {
  plot <- plot_topic_heatmap(model_output)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_topic_prevalence function returns a ggplot object", {
  plot <- plot_topic_prevalence(model_output)
  expect_s3_class(plot, "ggplot")
})

test_that("gen_topic_clouds function returns a list of wordcloud2 objects", {
  wordcloud_plots <- gen_topic_clouds(model_output, n = 10)
  expect_type(wordcloud_plots, "list")
  expect_length(wordcloud_plots, 2)  # We specified 2 topics in model_topics
  expect_s3_class(wordcloud_plots[[1]], "htmlwidget")
})
