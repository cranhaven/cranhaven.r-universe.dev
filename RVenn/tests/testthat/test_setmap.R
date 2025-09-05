foo = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7),
           C = c(5, 6, 7, 8, 9), D = c(7, 8, 9, 10, 11))


# Testing heatmap function ======================
test_that("overlap, unite and discern: all and slice", {
  expect_equal(class(setmap(Venn(foo), slice = 1:3,
                            element_clustering = FALSE,
                            set_clustering = FALSE,
                            element_fontsize = 12,
                            set_fontsize = 12,
                            method = "complete",
                            legend = FALSE, title = "Heatmap")),
               "pheatmap")

}
)


