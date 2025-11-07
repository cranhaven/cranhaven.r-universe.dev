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

pmid_vec <- get_pmid(toy_df,
                     col.pmid = PMID_,
                     copy = FALSE)

test_that("Tests that PMIDs are received from a dataframe", {
    expect_type(pmid_vec, "integer")
    expect_length(pmid_vec, 40)
})
