library(miRetrieve)
library(testthat)



df_mirtar <- join_mirtarbase(df_crc)

df_mirtarbase_na <- join_mirtarbase(df_crc,
                                    filter_na = FALSE)

df_mirtarbase_red <- join_mirtarbase(df_crc,
                                     reduce = TRUE)

test_that("Tests that miRTarBase is added to dataframe", {
    expect_s3_class(df_mirtar, "data.frame")
    expect_type(df_mirtar, "list")
    expect_true("miRNA_tarbase" %in% colnames(df_mirtar))
    expect_true("Target" %in% colnames(df_mirtar))
    expect_gte(nrow(df_mirtarbase_na), nrow(df_mirtar))
    expect_lte(nrow(df_mirtarbase_red), nrow(df_mirtar))
    expect_lte(ncol(df_mirtarbase_red), ncol(df_mirtar))
})
