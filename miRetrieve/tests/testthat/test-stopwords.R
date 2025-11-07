library(miRetrieve)
library(testthat)

sw_1 <- c("stop1", "stop2")
sw_2 <- c("stop3", "stop4")
sw_3 <- c("stop5", "stop6")

df_sw1 <- generate_stopwords(sw_1)
df_sw2 <- generate_stopwords(sw_2)

df_sw_c <- generate_stopwords(sw_2, combine_with = df_sw1)
df_sw_c2 <- combine_stopwords(df_sw1, df_sw2)

df_sw3 <- generate_stopwords(sw_3)
colnames(df_sw3) <- c("Col1", "Col2")

test_that("Tests that miRNA vectors are combined", {
    expect_equal(typeof(df_sw1), "list")
    expect_gte(nrow(df_sw_c), nrow(df_sw1))
    expect_equal(nrow(df_sw_c), nrow(df_sw_c2))
    expect_error(combine_stopwords(df_sw3, df_sw2))
})
