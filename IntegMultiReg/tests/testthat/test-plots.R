test_that("plot_top_features draws and returns the ranked feature table", {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)

  tab <- plot_top_features(fit_bin, top = 8)
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 8L)
  expect_named(tab, c("platform", "feature", "mpip", "subgroup"))
  # returned table is sorted by decreasing mPIP and the values are valid
  expect_true(all(diff(tab$mpip) <= 0))
  expect_true(all(tab$mpip >= 0 & tab$mpip <= 1))
  expect_true(all(tab$platform %in% fit_bin$platform_names))
})

test_that("plot_top_features caps 'top' at the number of features", {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)
  total_features <- sum(vapply(coef(fit_bin), ncol, integer(1)))
  tab <- plot_top_features(fit_bin, top = 1000)
  expect_equal(nrow(tab), total_features)
})

test_that("plot_subgroup_sizes draws and returns the subgroup sizes", {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)

  sizes <- plot_subgroup_sizes(fit_bin)
  expect_equal(sizes[c("011", "100", "101", "111")],
               stats::setNames(c(120L, 60L, 60L, 60L),
                               c("011", "100", "101", "111")))
})

test_that("plot font-size controls accept underscore and base-R aliases", {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)

  expect_silent(plot(fit_bin, type = "selection",
                     cex_axis = 1.1, cex_lab = 1.1, cex_main = 1.1))
  expect_silent(plot(fit_bin, type = "selection", base_cex = 0.9,
                     legend = FALSE, col = c("white", "grey80", "black"),
                     mar = c(5, 5, 3, 1), mgp = c(3, 1, 0)))
  expect_silent(plot(fit_bin, type = "theta", platform = 1,
                     cex.axis = 0.9, cex.lab = 0.9, cex.main = 0.9))
  expect_silent(plot(fit_bin, type = "theta", platform = 1,
                     palette = "heatmap"))
  expect_silent(plot(fit_bin, type = "trace", cex_axis = 0.9, col = "grey20"))

  expect_silent(plot_top_features(
    fit_bin, top = 5, cex_names = 0.9, cex_axis = 0.9,
    cex_lab = 0.9, cex_main = 0.9, cex_legend = 0.9
  ))
  expect_silent(plot_top_features(
    fit_bin, top = 5, cex.names = 0.9, cex.axis = 0.9,
    cex.lab = 0.9, cex.main = 0.9, cex.legend = 0.9
  ))
  expect_silent(plot_top_features(
    fit_bin, top = 5, base_cex = 0.9, legend = FALSE, reference = NULL,
    col = c("#4E79A7", "#59A14F", "#E15759"), xlim = c(0, 1)
  ))
  expect_silent(plot_top_features(fit_bin, top = 5, show_source = FALSE))
  expect_silent(plot_top_features(
    fit_bin, top = 5, palette = "platform"
  ))

  expect_silent(plot_subgroup_sizes(
    fit_bin, cex_axis = 0.9, cex_lab = 0.9,
    cex_main = 0.9, cex_values = 0.9
  ))
  expect_silent(plot_subgroup_sizes(
    fit_bin, cex.axis = 0.9, cex.lab = 0.9,
    cex.main = 0.9, cex.values = 0.9
  ))
  expect_silent(plot_subgroup_sizes(
    fit_bin, base_cex = 0.9, show_values = FALSE,
    col = "#4E79A7", ylim = c(0, 150)
  ))
  expect_silent(plot_subgroup_sizes(
    fit_bin, palette = "subgroup"
  ))
})

test_that("the stand-alone plot helpers reject non-imr input", {
  expect_error(plot_top_features(list(1)), "imr")
  expect_error(plot_subgroup_sizes(list(1)), "imr")
})

test_that("plot helpers validate user-facing controls", {
  expect_error(plot_top_features(fit_bin, top = 0), "`top`")
  expect_error(plot_top_features(fit_bin, cex_axis = 0), "`cex_axis`")
  expect_error(plot_top_features(fit_bin, base_cex = 0), "`base_cex`")
  expect_error(plot_top_features(fit_bin, reference = 2), "`reference`")
  expect_error(plot_top_features(fit_bin, legend = NA), "`legend`")
  expect_error(plot_top_features(fit_bin, show_source = NA), "`show_source`")
  expect_error(plot_top_features(fit_bin, xlim = c(1, 0)), "`xlim`")
  expect_error(plot_subgroup_sizes(fit_bin, cex_values = -1), "`cex_values`")
  expect_error(plot_subgroup_sizes(fit_bin, show_values = NA), "`show_values`")
  expect_error(plot_subgroup_sizes(fit_bin, ylim = c(1, 0)), "`ylim`")
  expect_error(plot(fit_bin, type = "selection", platform = 99), "`platform`")
  expect_error(plot(fit_bin, type = "selection", legend_width = 0),
               "`legend_width`")
})
