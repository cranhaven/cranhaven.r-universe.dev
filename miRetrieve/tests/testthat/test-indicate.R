library(miRetrieve)
library(testthat)

toy_df <- data.frame("miRNA_" = c("miR-30", "miR-30", "miR-29"),
                     "Abstract_" = c("Metformin tschakalaka atherosclerosis",
                                    "metformin Atherosclerosis multiple",
                                    "diabetes Diabetes SGLT-2"))

df_indicate_mir <- indicate_mir(toy_df,
                                "miR-30",
                                col.mir = miRNA_)

test_that("Tests that miRNA are indicated in df", {
    expect_s3_class(df_indicate_mir, "data.frame")
    expect_gte(ncol(df_indicate_mir), ncol(toy_df))
    expect_equal(sum(df_indicate_mir$`miR-30_present` == "Yes"), 2)
})

df_indicate_term <- indicate_term(toy_df,
                                  "diabetes",
                                  col.abstract = Abstract_)

df_indicate_term_dis <- indicate_term(toy_df,
                                      "diabetes",
                                      col.abstract = Abstract_,
                                      discard = TRUE)

df_indicate_term_thresh <- indicate_term(toy_df,
                                         "diabetes",
                                         threshold = 3,
                                         col.abstract = Abstract_)

df_indicate_term_case <- indicate_term(toy_df,
                                       "diabetes",
                                       threshold = 2,
                                       case = TRUE,
                                       col.abstract = Abstract_)

test_that("Tests that data frames are indicated for a term", {
    expect_s3_class(df_indicate_term, "data.frame")
    expect_gte(ncol(df_indicate_term), ncol(toy_df))
    expect_lte(nrow(df_indicate_term_dis), nrow(df_indicate_term))
    expect_lte(sum(df_indicate_term_thresh$Term_diabetes  == "Yes"),
               sum(df_indicate_term$Term_diabetes  == "Yes"))
    expect_lte(sum(df_indicate_term_case$Term_diabetes  == "Yes"),
               sum(df_indicate_term$Term_diabetes  == "Yes"))
})
