library(miRetrieve)
library(testthat)

df_merged <- rbind(df_crc, df_panc)

compare_terms <- compare_mir_terms(df_merged,
                                   "miR-21",
                                   title = "Test_title")

compare_terms_top <- compare_mir_terms(df_merged,
                                       "miR-21",
                                       top = 3)

compare_terms_n <- compare_mir_terms(df_merged,
                                     "miR-21",
                                     token = "ngrams",
                                     n = 2)

compare_terms_normalize <- compare_mir_terms(df_merged,
                                             "miR-21",
                                             normalize = FALSE)

test_that("Tests that terms are compared between dataframes", {
    expect_s3_class(compare_terms, "ggplot")
    expect_equal(compare_terms$labels$title, "Test_title")
    expect_lte(length(compare_terms_top$data$word),
               length(compare_terms$data$word))
    expect_gte(lengths(strsplit(as.character(compare_terms_n$data$word[1]), " ")),
               lengths(strsplit(as.character(compare_terms$data$word[1]), " ")))
    expect_type(compare_terms$data$no_of_abstract, "double")
    expect_type(compare_terms_normalize$data$no_of_abstract, "integer")
})

compare_terms_log2 <- compare_mir_terms_log2(df_merged,
                                   "miR-21",
                                   title = "Test_title")

compare_terms_top_log2 <- compare_mir_terms_log2(df_merged,
                                       "miR-21",
                                       top = 3)

compare_terms_n_log2 <- compare_mir_terms_log2(df_merged,
                                     "miR-21",
                                     token = "ngrams",
                                     n = 2)

test_that("Tests that terms are compared between dataframes on a log2-scale", {
    expect_s3_class(compare_terms_log2$plot, "ggplot")
    expect_equal(compare_terms_log2$plot$labels$title, "Test_title")
    expect_lte(length(compare_terms_top_log2$plot$data$word),
               length(compare_terms_log2$plot$data$word))
    expect_gte(lengths(strsplit(as.character(compare_terms_n_log2$plot$data$word[1]), " ")),
               lengths(strsplit(as.character(compare_terms_log2$plot$data$word[1]), " ")))
})

compare_terms_scatter <- compare_mir_terms_scatter(df_merged,
                                             "miR-21",
                                             title = "Test_title")

compare_terms_top_scatter <- compare_mir_terms_scatter(df_merged,
                                                 "miR-21",
                                                 top = 3)

test_that("Tests that terms are compared between dataframes in a scatter plot", {
    expect_s3_class(compare_terms_scatter, "plotly")
    expect_equal(compare_terms_scatter$x$layout$title$text,
                 "Test_title")
})

compare_terms_unique <- compare_mir_terms_unique(df_merged,
                                   "miR-21",
                                   title = "Test_title")

compare_terms_top_unique <- compare_mir_terms_unique(df_merged,
                                       "miR-21",
                                       top = 3)

compare_terms_n_unique <- compare_mir_terms_unique(df_merged,
                                     "miR-21",
                                     token = "ngrams",
                                     n = 2)

compare_terms_normalize_unique <- compare_mir_terms_unique(df_merged,
                                             "miR-21",
                                             normalize = FALSE)

test_that("Tests that unique terms are compared between dataframes", {
    expect_s3_class(compare_terms_unique, "ggplot")
    expect_equal(compare_terms_unique$labels$title, "Test_title")
    expect_lte(length(compare_terms_top_unique$data$word),
               length(compare_terms_unique$data$word))
    expect_gte(lengths(strsplit(as.character(compare_terms_n_unique$data$word[1]), " ")),
               lengths(strsplit(as.character(compare_terms_unique$data$word[1]), " ")))
    expect_type(compare_terms_unique$data$no_per_topic, "double")
    expect_type(compare_terms_normalize_unique$data$no_per_topic, "integer")
})
