library(miRetrieve)
library(testthat)

df_mir_1 <- df_test %>%
    extract_mir_df()

df_mir_3 <- df_test %>%
    extract_mir_df(threshold = 3)

df_mir_1_let <- df_test %>%
    extract_mir_df(extract_letters = TRUE)

test_that("Tests that topic columns are created", {
    expect_equal(colnames(df_mir_1)[7], "miRNA")
    expect_equal(nrow(df_mir_1), 6)
    expect_equal(nrow(df_mir_3), 4)
    # Expect no miRNAs with a trailing letter
    expect_equal(sum(grepl("\\d[a-z]", df_mir_1$miRNA)), 0)
    # Expect 3 miRNAs with a trailing letter in _let
    expect_equal(sum(grepl("\\d[a-z]", df_mir_1_let$miRNA)), 2)
})
