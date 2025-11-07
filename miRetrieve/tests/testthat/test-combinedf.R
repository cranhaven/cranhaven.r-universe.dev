library(miRetrieve)
library(testthat)

df1_ <- data.frame("Col1" = seq(1:2))

df2_ <- data.frame("Col1" = seq(1:3))

df3_ <- data.frame("Col2" = seq(1:3))

test_that("Tests that data frame can be combined", {
    expect_equal(nrow(combine_df(df1_, df2_)), nrow(df1_) + nrow(df2_))
    expect_error(combine_df(df1_, df3_))
    expect_equal(typeof(combine_df(df1_, df2_)), "list")
})
