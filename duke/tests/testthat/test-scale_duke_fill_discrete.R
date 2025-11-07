test_that("factors assume appropriate fill for 3-category variable", {
  df <- data.frame(x = c(rep("A", 10), rep("b", 5), rep("C", 8)))
  p <- ggplot2::ggplot(df,
              ggplot2::aes(x, fill = x)) +
    ggplot2::geom_bar() +
    scale_duke_fill_discrete()

  correct_fill <- c("#012169", "#C84E00", "#00539B")
  expect_equal(ggplot2::layer_data(p)$fill, correct_fill)
})

test_that("factors assume appropriate fill for 8-category variable", {
  df <- data.frame(x = c(rep("A", 10), rep("B", 5), rep("C", 8), rep("D", 4),
                         rep("E", 11), rep("F", 2), rep("G", 7), rep("H", 5)))
  p2 <- ggplot2::ggplot(df,
              ggplot2::aes(x, fill = x)) +
    ggplot2::geom_bar() +
    scale_duke_fill_discrete()

  correct_fill <- c("#012169", "#C84E00", "#00539B", "#339898", "#A1B70D",
                             "#E89923", "#FFD960", "#262626")
  expect_equal(ggplot2::layer_data(p2)$fill, correct_fill)
})

test_that("appropriate fill for factors over 8-levels", {
  df <- data.frame(x = c(rep("A", 10), rep("B", 5), rep("C", 8), rep("D", 4),
                         rep("E", 11), rep("F", 2), rep("G", 7), rep("H", 5),
                         rep("I", 3)))
  p3 <- ggplot2::ggplot(df,
                        ggplot2::aes(x, fill = x)) +
    ggplot2::geom_bar() +
    scale_duke_fill_discrete()

  correct_fill <- "#B5B5B5"

  expect_warning(expect_equal(ggplot2::layer_data(p3)$fill[9], "#B5B5B5"))
  # This manual palette can handle a maximum of 8 values. You have supplied 9.
})

test_that("appropriate fill for factors over 8-levels", {
  df <- data.frame(x = c(rep("A", 10), rep("B", 5), rep("C", 8), rep("D", 4),
                         rep("E", 11), rep("F", 2), rep("G", 7), rep("H", 5),
                         rep("I", 3)))
  p4 <- ggplot2::ggplot(df,
                        ggplot2::aes(x, fill = x)) +
    ggplot2::geom_bar() +
    scale_duke_fill_discrete()

  expect_warning(ggplot2::ggplot_build(p4))
  # This manual palette can handle a maximum of 8 values. You have supplied 9.
})

