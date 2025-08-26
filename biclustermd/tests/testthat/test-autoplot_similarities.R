context("autoplot similarities")

test_that("test that the correct number of iterations are plotted and are in order", {
  sbc <- biclustermd(synthetic)
  aps <- ggplot2::ggplot_build(autoplot(sbc$Similarities))
  
  expect_equal(sapply(aps$data, function(z) max(z$x) + 1), rep(sbc$iteration, 2))
  expect_equal(lapply(aps$data, function(z) unique(z$x)), lapply(1:2, function(i) (1:sbc$iteration) - 1))
})

test_that("test if facet = TRUE, three panels are plotted", {
  sbc <- biclustermd(synthetic)
  taps <- ggplot2::ggplot_build(autoplot(sbc$Similarities, facet = TRUE))
  
  expect_equal(nrow(taps$layout$layout), 3)
})

test_that("test if facet = FALSE, one panel is plotted", {
  sbc <- biclustermd(synthetic)
  faps <- ggplot2::ggplot_build(autoplot(sbc$Similarities, facet = FALSE))
  
  expect_equal(nrow(faps$layout$layout), 1)
})

test_that("test if facet = FALSE, three line types are used", {
  sbc <- biclustermd(synthetic)
  faps <- ggplot2::ggplot_build(autoplot(sbc$Similarities, facet = FALSE))
  
  expect_equal(length(unique(faps$data[[1]]$linetype)), 3)
})

test_that("test that two colors are used, regardless of facetting", {
  sbc <- biclustermd(synthetic)
  taps <- ggplot2::ggplot_build(autoplot(sbc$Similarities, facet = TRUE))
  faps <- ggplot2::ggplot_build(autoplot(sbc$Similarities, facet = FALSE))
  
  expect_equal(length(unique(taps$data[[2]]$colour)), 2)
  expect_equal(length(unique(faps$data[[2]]$colour)), 2)
})