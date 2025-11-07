library(miRetrieve)
library(testthat)

df_merged <- rbind(df_crc, df_panc)

compare_count <- compare_mir_count(df_merged,
                                   "miR-21",
                                   title = "Test_title")

length_unique <- length(unique(compare_count$data$miRNA))

compare_count_mult <- compare_mir_count(df_merged,
                                         mir = c("miR-21", "miR-27"))

length_unique_multiple <- length(unique(compare_count_mult$data$miRNA))

compare_count_normalize <- compare_mir_count(df_merged,
                                             "miR-21",
                                             normalize = FALSE)

test_that("Tests that miRNA counts are compared between topics", {
    expect_s3_class(compare_count, "ggplot")
    expect_equal(compare_count$labels$title, "Test_title")
    expect_gte(length_unique_multiple,
               length_unique)
    expect_type(compare_count$data$count_mir, "double")
    expect_type(compare_count_normalize$data$count_mir, "integer")
})

compare_count_uni <- compare_mir_count_unique(df_merged,
                                              title = "Test_title")

compare_count_uni_top <- compare_mir_count_unique(df_merged,
                                                  top = 1)


compare_count_uni_normalize <- compare_mir_count_unique(df_merged,
                                                        top = 1, #set top = 1 for faster computation
                                                        normalize = FALSE)

test_that("Tests that unique miRNA counts are compared between dataframes", {
    expect_s3_class(compare_count_uni, "ggplot")
    expect_equal(compare_count_uni$labels$title, "Test_title")
    expect_lte(length(compare_count_uni_top$data$miRNA),
               length(compare_count_uni$data$miRNA))
    expect_type(compare_count_uni$data$count_mir, "double")
    expect_type(compare_count_uni_normalize$data$count_mir, "integer")
})

compare_count_log2 <- compare_mir_count_log2(df_merged,
                                             "miR-21",
                                             title = "Test_title")

length_unique_log2 <- length(unique(compare_count_log2$plot$data$miRNA))

compare_count_mult_log2 <- compare_mir_count_log2(df_merged,
                                                  mir = c("miR-21", "miR-27"))

length_unique_multiple_log2 <- length(unique(compare_count_mult_log2$plot$data$miRNA))

test_that("Tests that miRNA counts are compared between topics on a log2 scale", {
    expect_s3_class(compare_count_log2$plot, "ggplot")
    expect_equal(compare_count_log2$plot$labels$title, "Test_title")
    expect_gte(length_unique_multiple_log2,
               length_unique_log2)
})
