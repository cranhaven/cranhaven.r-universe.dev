# Extracted from test-bootstrap-ega.R:40

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
item_plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
bootega_object <- list(
    summary.table = data.frame(
      n.Boots = 500,
      median.dim = 2,
      SE.dim = 0.12,
      check.names = FALSE
    ),
    frequency = data.frame(
      `# of Factors` = c(1, 2),
      Frequency = c(25, 475),
      check.names = FALSE
    )
  )
stability_object <- list(
    dimension.stability = list(
      structural.consistency = c(`1` = 0.90, `2` = 0.88),
      average.item.stability = c(`1` = 0.94, `2` = 0.91)
    ),
    item.stability = list(
      membership = list(structure = c(item_1 = 1, item_2 = 1, item_3 = 2)),
      item.stability = list(
        empirical.dimensions = c(item_1 = 0.95, item_2 = 0.90, item_3 = 0.92)
      ),
      plot = item_plot
    )
  )
result <- FAfA:::prepare_bootega_results(bootega_object, stability_object)
