context("autoplot sse")

test_that("test that the correct number of iterations are plotted and are in order", {
  sbc <- biclustermd(synthetic)
  aps <- ggplot2::ggplot_build(autoplot(sbc$SSE))
  
  expect_equal(sapply(aps$data, function(z) max(z$x) + 1), rep(sbc$iteration, 2))
  expect_equal(lapply(aps$data, function(z) unique(z$x)), lapply(1:2, function(i) (1:sbc$iteration) - 1))
})