library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = sample(c("miR-30", "miR-29", "miR-28"),
                                       size = nrow(df_test),
                                       replace = TRUE),
                     "PMID_" = seq(1:nrow(df_test)),
                     "Abstract" = df_test["Abstract"],
                     stringsAsFactors = FALSE)

plot_terms <- plot_mir_terms(toy_df,
                             mir = "miR-30",
                             col.mir = miRNA_,
                             col.pmid = PMID_,
                             col.abstract = Abstract,
                             title = "Test_title")

plot_terms_top <- plot_mir_terms(toy_df,
                                 mir = "miR-30",
                                 top = 2,
                                 col.mir = miRNA_,
                                 col.pmid = PMID_,
                                 col.abstract = Abstract)

test_that("Tests that terms are plotted per miRNA", {
    expect_s3_class(plot_terms, "ggplot")
    expect_equal(plot_terms$labels$title, "Test_title")
    expect_lte(length(plot_terms_top$data$word),
               length(plot_terms$data$word))
    expect_equal(plot_terms_top$labels$title,
                 "Top terms for miR-30")
})


plot_terms_ngram2 <- plot_mir_terms(toy_df,
                                    mir = "miR-30",
                                    token = "ngrams",
                                    n = 2,
                                    col.mir = miRNA_,
                                    col.pmid = PMID_,
                                    col.abstract = Abstract)

plot_terms_ngram3 <- plot_mir_terms(toy_df,
                                    mir = "miR-30",
                                    token = "ngrams",
                                    n = 3,
                                    col.mir = miRNA_,
                                    col.pmid = PMID_,
                                    col.abstract = Abstract)

test_that("Tests that terms are presented as ngrams", {
    expect_gte(lengths(strsplit(as.character(plot_terms_ngram2$data$word[1]), " ")),
               lengths(strsplit(as.character(plot_terms$data$word[1]), " ")))
    expect_gte(lengths(strsplit(as.character(plot_terms_ngram3$data$word[1]), " ")),
               lengths(strsplit(as.character(plot_terms_ngram2$data$word[1]), " ")))
})


plot_terms_tfidf <- suppressWarnings(
    plot_mir_terms(toy_df,
                   mir = "miR-30",
                   tf.idf = TRUE,
                   col.mir = miRNA_,
                   col.pmid = PMID_,
                   col.abstract = Abstract)
)

test_that("Tests that at least a tfidf plot is created", {
    expect_s3_class(plot_terms_tfidf, "ggplot")
    expect_gte(length(plot_terms_tfidf$data$miRNA_),
               0)
    expect_gte(length(plot_terms_tfidf$data$word),
               0)
    expect_gte(length(plot_terms_tfidf$data$mirna_count),
               0)
})
