test_that("ggplotly_with_legend adds formatted legend", {

  df <- data.frame(
    grp = c("D", "B"),
    val = 1:2
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(grp, val, color = grp)) +
    ggplot2::geom_point()

  # Default ggplot legend
  expect_null(ggplot2::ggplot_build(p)$data[[1]]$name)

  # Convert to plotly
  p2 <- ggplotly_with_legend(p, color = "grp", mapping_table = list(grp = "Group"))

  # Check legend title
  legend_text <- p2$x$layoutAttrs[[1]]$legend$title$text
  expect_equal(legend_text, "Group")

})
