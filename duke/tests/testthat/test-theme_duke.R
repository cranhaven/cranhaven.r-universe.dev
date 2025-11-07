test_that("using theme_duke produces desired stylistic changes", {
  p <- ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x, x)) +
    ggplot2::labs(title = "Test Name")

  # Adding theme_duke() makes the plot title Duke Blue
  p <- p + theme_duke()
  expect_true(p$theme$plot.title$colour == "#00539B")

  # Commenting these out until font issues are fixed
  ## Adding theme_duke() makes the plot title have Atkinson Hyperlegible font
  #p <- p + theme_duke()
  #expect_true(p$theme$plot.title$family %in% c("Atkinson Hyperlegible"))
  #
  ## Adding theme_duke() makes the caption have Atkinson Hyperlegible font
  #p <- p + theme_duke()
  #expect_true(p$theme$plot.caption$family %in% c("Atkinson Hyperlegible"))

  # Adding theme_duke() makes the axis text lines navy blue
  p <- p + theme_duke()
  expect_true(p$theme$axis.text$colour == "#012169")
})

test_that("modifying theme element properties with + operator works", {
  # Adding element_blank replaces element
  t <- theme_duke() + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  expect_identical(t$axis.text.y, ggplot2::element_blank())

  # Adding a non-blank element to an element_blank() replaces it
  t <- t + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "red"))
  expect_identical(t$axis.text.y, ggplot2::element_text(colour = "red"))

  # Adding empty theme() has no effect
  t <- theme_duke() + ggplot2::theme()
  expect_identical(t, theme_duke())

  expect_error(theme_duke() + "asdf")
})

test_that("adding theme object to ggplot object with + operator works", {
  ## test with complete theme
  p <- ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x, x)) +
    ggplot2::geom_point() +
    theme_duke()
  p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = 20))
  expect_true(p$theme$axis.title$size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + ggplot2::theme(text = ggplot2::element_text(colour = "red"))
  expect_true(p$theme$text$colour == "red")
  tt <- theme_duke()$text
  tt$colour <- "red"
  expect_true(tt$inherit.blank)
  tt$inherit.blank <- FALSE
  expect_identical(p$theme$text, tt)

  ## test without complete theme
  p <- ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x, x)) +
    ggplot2::geom_point()
  p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = 20))
  expect_true(p$theme$axis.title$size == 20)

  ## stepwise addition of partial themes is identical to one-step addition
  p <- ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x, x)) +
    ggplot2::geom_point()
  p1 <- p + theme_duke() +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = "blue")) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = "red"))

  p2 <- p + theme_duke() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = "blue"),
      axis.ticks.x = ggplot2::element_line(color = "red")
    )

  expect_identical(p1$theme, p2$theme)
})

test_that("replacing theme elements with %+replace% operator works", {
  # Changing a "leaf node" works
  t <- theme_duke() %+replace% ggplot2::theme(axis.title.x = ggplot2::element_text(colour = "red"))
  expect_identical(t$axis.title.x, ggplot2::element_text(colour = "red"))
  # Make sure the class didn't change or get dropped
  expect_true(ggplot2::is.theme(t))

  # Changing an intermediate node works
  t <- theme_duke() %+replace% ggplot2::theme(axis.title = ggplot2::element_text(colour = "red"))
  expect_identical(t$axis.title, ggplot2::element_text(colour = "red"))
  # Descendent is unchanged
  expect_identical(t$axis.title.x, theme_duke()$axis.title.x)

  # Adding empty theme() has no effect
  t <- theme_duke() %+replace% ggplot2::theme()
  expect_identical(t, theme_duke())

  expect_error(theme_duke() + "asdf")
})
