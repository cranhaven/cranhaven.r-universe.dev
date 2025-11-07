library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = c(sample(c("miR-1", "miR-2", "miR-3", "miR-4", "miR-5"),
                                         size = 20,
                                         replace = TRUE),
                                  sample(c("miR-3", "miR-4", "miR-5", "miR-6", "miR-7"),
                                         size = 20,
                                         replace = TRUE)),
                     "Topic_" = rep(c("Topic1", "Topic2"), each = 20),
                     "PMID_" = seq(1:40))

shared_mir <- get_shared_mir_df(toy_df,
                                col.topic = Topic_,
                                col.mir = miRNA_,
                                col.pmid = PMID_)

shared_mir_top <- get_shared_mir_df(toy_df,
                                    top = 3,
                                    col.topic = Topic_,
                                    col.mir = miRNA_,
                                    col.pmid = PMID_)

test_that("Tests that shared miRNAs are received from a dataframe", {
    expect_type(shared_mir, "character")
    expect_length(shared_mir, 3)
    expect_lte(length(shared_mir_top),
               length(shared_mir))
})

toy_df_3 <- data.frame("miRNA_" = c(sample(c("miR-1", "miR-2", "miR-3", "miR-4", "miR-5"),
                                           size = 20,
                                           replace = TRUE),
                                    sample(c("miR-3", "miR-4", "miR-5", "miR-6", "miR-7"),
                                           size = 20,
                                           replace = TRUE),
                                    sample(c("miR-4", "miR-5", "miR-6", "miR-7", "miR-8"),
                                           size = 20,
                                           replace = TRUE)),
                       "Topic_" = rep(c("Topic1", "Topic2", "Topic3"), each = 20),
                       "PMID_" = seq(1:60))

test_that("Tests that get_mir_shared_vec throws an error if more than
          two topics are included", {
              expect_error(get_shared_mir_df(toy_df_3,
                                             col.topic = Topic_,
                                             col.mir = miRNA_,
                                             col.pmid = PMID_))
          })


distinct_df_1 <- get_distinct_mir_df(toy_df,
                                     "Topic1",
                                     col.topic = Topic_,
                                     col.mir = miRNA_)

distinct_df_1_top <- get_distinct_mir_df(toy_df,top = 1,
                                         "Topic1",
                                         col.topic = Topic_,
                                         col.mir = miRNA_)

test_that("Tests that shared miRNAs are received from a dataframe", {
    expect_type(distinct_df_1, "character")
    expect_length(distinct_df_1, 2)
    expect_lte(length(distinct_df_1_top),
               length(distinct_df_1))
})
