test_that("factors assume appropriate colors for 3-category variable", {
  df <- data.frame(x = 1:3, y = 3:1, z = factor(letters[1:3]))
  p <- ggplot2::ggplot(df,
               ggplot2::aes(x, y, color = z, shape = z)) +
    ggplot2::geom_point() +
    duke::scale_duke_color_discrete()

  correct_color <- c("#012169", "#C84E00", "#00539B")
  expect_equal(ggplot2::layer_data(p)$colour, correct_color)
})

test_that("factors assume appropriate colors for 8-category variable", {
  df <- data.frame(x = 1:8, y = 8:1, z = factor(letters[1:8]))
  p2 <- ggplot2::ggplot(df,
              ggplot2::aes(x, y, color = z)) +
    ggplot2::geom_point() +
    scale_duke_color_discrete()

  correct_color <- c("#012169", "#C84E00", "#00539B", "#339898", "#A1B70D",
                             "#E89923", "#FFD960", "#262626")
  expect_equal(ggplot2::layer_data(p2)$colour, correct_color)
})

test_that("use of colour does not affect implementation", {
  df <- data.frame(x = 1:4, y = 4:1, z = factor(letters[1:4]))
  p3 <- ggplot2::ggplot(df,
              ggplot2::aes(x, y, color = z, shape = z)) +
    ggplot2::geom_point() +
    scale_duke_colour_discrete()

  correct_color <- c("#012169", "#C84E00", "#00539B", "#339898")
  expect_equal(ggplot2::layer_data(p3)$colour, correct_color)
})

test_that("appropriate fill for factors over 8-levels", {
  df <- data.frame(x = 1:9, y = 9:1, z = factor(letters[1:9]))
  p4 <- ggplot2::ggplot(df,
                        ggplot2::aes(x, y, color = z)) +
    ggplot2::geom_point() +
    scale_duke_color_discrete()

  expect_warning(ggplot2::ggplot_build(p4))
  # This manual palette can handle a maximum of 8 values. You have supplied 9.
})
