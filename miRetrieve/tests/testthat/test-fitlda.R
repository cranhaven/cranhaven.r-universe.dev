library(miRetrieve)
library(testthat)

test_lda <- fit_lda(df_test, 2)


test_that("Tests that 'fit_lda' actually returns an object", {
    expect_equal(test_lda@k, 2)
    expect_type(test_lda@terms, "character")
    expect_type(test_lda@wordassignments, "list")
})

test_that("Tests that 'fit_lda' actually returns an object", {
    expect_equal(test_lda@k, 2)
    expect_type(test_lda@terms, "character")
    expect_type(test_lda@wordassignments, "list")
})

perplexity_plot <- plot_perplexity(df_test,
                                   start = 2,
                                   end = 3)

test_that("Tests that 'perplexity_plot' returns a plot", {
    expect_s3_class(perplexity_plot, "ggplot")
    expect_equal(perplexity_plot$labels$title, "Perplexity plot")
})


plot_lda <- plot_lda_term(test_lda,
                          top.terms = 5,
                          title = "Test_lda")


test_that("Tests that 'plot_lda_term' returns a plot", {
    expect_type(plot_lda, "list")
    expect_equal(length(unique(plot_lda$data$topic)), 2)
    expect_equal(plot_lda$labels$title, "Test_lda")
})


df_assigned <- assign_topic_lda(df_test,
                                test_lda,
                                topic.names = c("Topic_1", "Topic_2"))

test_that("Tests that 'assign_topic_lda' assigns topics", {
    expect_equal(colnames(df_assigned)[7], "Topic")
    expect_type(df_assigned, "list")
    expect_equal(length(unique(df_assigned[["Topic"]])), 2)
})
