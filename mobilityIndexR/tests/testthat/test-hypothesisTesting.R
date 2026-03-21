test_that("Testing hypothesisTesting",
  {
    set.seed(8322)
    expect_equal(getHypothesisTest(dat_A = incomeMobility, dat_B = incomeMobility, cols_A = c("t0", "t3"), cols_B = c("t5", "t8"), type = "relative", num_ranks = 5),
                 list(prais_bibby = 0.38, average_movement = 0.57, wgm = 0.4, os_total_top = 0.58, os_far_top = 0.94, os_total_bottom = 0.41, os_far_bottom = 0))
  }
)

