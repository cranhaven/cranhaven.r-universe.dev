library(miRetrieve)
library(testthat)

df_topic <- df_test %>%
    add_col_topic(topic.name = "TopicTest")

colnames(df_topic)[7]

topic_in <- unique(df_topic$Topic)

test_that("Tests that topic columns are created", {
    expect_equal(colnames(df_topic)[7], "Topic")
    expect_equal(topic_in, "TopicTest")
})
