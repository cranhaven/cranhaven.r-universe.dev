# test_file('~/dev/HTSSIP/tests/testthat/test-BD_shift.R')

test_that('Percent overlap is working',{
  x = perc_overlap(0, 1, 0, 0.5)
  expect_equal(x, 50)
  x = perc_overlap(0, 0.5, 0, 1)
  expect_equal(x, 100)
  x = perc_overlap(0, 0.5, 0.5, 1)
  expect_equal(x, 0)
})

test_that('parse_dist is working',{
  skip_on_cran()
  # calculating beta-diversity values
  physeq_S2D2_d = phyloseq::distance(physeq_S2D2,
                             method='unifrac',
                             weighted=TRUE,
                             fast=TRUE,
                             normalized=FALSE)
  physeq_S2D2_d = parse_dist(physeq_S2D2_d)
  expect_is(physeq_S2D2_d, 'data.frame')
})


expect_wmean = function(wmean){
  expect_is(wmean, 'data.frame')
  expect_gt(wmean %>% nrow, 0)

  wmean_min = wmean$distance %>% min
  expect_gte(wmean_min, 0)
  expect_lt(wmean_min, 1)
  wmean_max = wmean$distance %>% max
  expect_gt(wmean_max, 0)
  expect_lte(wmean_max, 1)

  cat('\n--Weighted mean distance summary--\n')
  wmean$distance %>% summary %>% print
  cat('\n')
}

test_that('BD_shift runs w/ default',{
  skip_on_cran()

  ## basic call
  data(physeq_S2D2_l)

  # dataset 1
  wmean = BD_shift(physeq_S2D2_l[[1]], nperm=3)
  expect_wmean(wmean)

  # dataset 2
  wmean = BD_shift(physeq_S2D2_l[[2]], nperm=3)
  expect_wmean(wmean)

  # ggplot
  p = ggplot2::ggplot(wmean, aes(BD_min.x, wmean_dist)) +
        geom_point()
  expect_is(p, 'ggplot')
})

test_that('BD_shift runs with differing permutation methods',{
  skip_on_cran()
  data(physeq_S2D2_l)

  wmean = BD_shift(physeq_S2D2_l[[1]], nperm=3, perm_method='treatment')
  expect_wmean(wmean)
  wmean = BD_shift(physeq_S2D2_l[[1]], nperm=3, perm_method='adjacent')
  expect_wmean(wmean)
  wmean = BD_shift(physeq_S2D2_l[[1]], nperm=3, perm_method='overlap')
  expect_wmean(wmean)
})

test_that('BD_shift runs w/ Bray-Curtis',{
  skip_on_cran()
  data(physeq_S2D2_l)

  # dataset 1
  wmean = BD_shift(physeq_S2D2_l[[1]], method='bray', nperm=5)
  expect_wmean(wmean)

  # ggplot
  p = ggplot2::ggplot(wmean, aes(BD_min.x, wmean_dist)) +
    geom_point()
  expect_is(p, 'ggplot')
})
