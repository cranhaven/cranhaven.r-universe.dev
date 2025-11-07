library(miRetrieve)
library(testthat)

# Test PubMed file with 20 abstracts
df <- read_pubmed("pubmed_test.txt")
colnames_df <- colnames(df)
expect_colnames <- c("PMID", "Year", "Title", "Abstract", "Language", "Type")

test_that("Tests that PubMed Files are read as data.frames", {
    expect_type(df, "list")
    expect_equal(colnames_df, expect_colnames)
    expect_equal(nrow(df), 20)
})
