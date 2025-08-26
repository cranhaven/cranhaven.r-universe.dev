context("autoplot biclustermd")


test_that("autoplot_biclustermd() plots column clusters in correct clusters", {

  sbc <- biclustermd(synthetic)
  ap <- ggplot2::ggplot_build(autoplot(sbc))

  plot_df <- data.frame(
    col_clust = rep(1:ncol(sbc$P), colSums(sbc$P)),
    col_name = ap$layout$panel_scales_x[[1]]$get_breaks()
  )
  plot_df <- plot_df[order(plot_df$col_clust, plot_df$col_name),]

  expected_df <- data.frame(
    col_clust = drop(sbc$P %*% matrix(1:ncol(sbc$P), nrow = ncol(sbc$P))),
    col_name = colnames(synthetic)
  )
  expected_df <- expected_df[order(expected_df$col_clust, expected_df$col_name),]

  expect_equivalent(split(plot_df, plot_df$col_clust), split(expected_df, expected_df$col_clust))

})


test_that("autoplot_biclustermd() plots row clusters in correct clusters", {

  sbc <- biclustermd(synthetic)
  ap <- ggplot2::ggplot_build(autoplot(sbc))

  plot_df <- data.frame(
    row_clust = rep(1:ncol(sbc$Q), colSums(sbc$Q)),
    row_name = ap$layout$panel_scales_y[[1]]$get_breaks()
  )
  plot_df <- plot_df[order(plot_df$row_clust, plot_df$row_name),]

  expected_df <- data.frame(
    row_clust = drop(sbc$Q %*% matrix(1:ncol(sbc$Q), nrow = ncol(sbc$Q))),
    row_name = rownames(synthetic)
  )
  expected_df <- expected_df[order(expected_df$row_clust, expected_df$row_name),]

  expect_equivalent(split(plot_df, plot_df$row_clust), split(expected_df, expected_df$row_clust))

})
