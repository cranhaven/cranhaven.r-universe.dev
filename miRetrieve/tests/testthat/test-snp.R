library(miRetrieve)
library(testthat)

df_snp <- data.frame("SNP" = c("rs4544 rs3655 test",
                                  "test test",
                                  "rs3655"),
                     "PMID" = seq(1:3))

df_snp_extract <- extract_snp(df_snp,
                              col.abstract = SNP)

df_snp_extract_i <- extract_snp(df_snp,
                              col.abstract = SNP,
                              indicate = TRUE)

df_snp_extract_d <- extract_snp(df_snp,
                                col.abstract = SNP,
                                discard = TRUE)

test_that("Tests that SNPs are correctly recognized in data.frames", {
    expect_gte(ncol(df_snp_extract), ncol(df_snp))
    expect_gte(ncol(df_snp_extract_i), ncol(df_snp_extract))
    expect_lte(nrow(df_snp_extract_d), nrow(df_snp))
})

df_snp_count <- count_snp(df_snp_extract_d)

test_that("Tests that SNPs are counted", {
    expect_equal(typeof(df_snp_count), "list")
    expect_equal(ncol(df_snp_count), 2)
})

top_snp <- get_snp(df_snp_extract_d, top = 1)

test_that("Tests that SNPs are extracted from data.frame", {
    expect_equal(typeof(top_snp), "character")
    expect_equal(length(top_snp), 1)
})

df_snp_subset <- subset_snp(df_snp_extract,
                            top_snp)

test_that("Tests that SNPs are extracted from data.frame", {
    expect_equal(nrow(df_snp_subset), 2)
    expect_equal(length(unique(df_snp_subset[["SNPs"]])), 1)
})
