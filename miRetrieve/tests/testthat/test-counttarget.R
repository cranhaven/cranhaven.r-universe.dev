library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("PMID" = c(34051305, 34033143, 34032694, 34028994, 34024846,
                                34017372, 34016957, 34014023, 34009437, 34009437,
                                34007244, 34006268, 34006268, 1, 2, 3),
                     "miRNA_" = c("miR-16", "miR-1", "miR-96", "miR-205",
                                  "miR-125", "miR-423", "let-7", "miR-17",
                                  "miR-195", "miR-27", "miR-6869", "miR-1275",
                                  "miR-181", "miR-181", "miR-181", "miR-181"),
                     "Target" = sample(c("Target1", "Target2", "Target3"),
                                       size = 16, replace = TRUE))


df_count_target <- count_target(toy_df,
                                add.df = FALSE)

df_count_target_add <- count_target(toy_df,
                                add.df = TRUE)

test_that("Tests that targets are counted in a data frame", {
    expect_equal(typeof(df_count_target), "list")
    expect_equal(ncol(df_count_target), 2)
    expect_equal(typeof(df_count_target_add), "list")
    expect_gte(ncol(df_count_target_add), ncol(toy_df))
})
