set.seed(8322)

dat <- data.frame(x = c(1,2,3,4,1,2,1,4,3,1), y = c(2,1,3,2,1,4,4,2,3,1), id = 1:10)

test_that("Testing makePraisBibby",
  {
    expect_equal(makePraisBibby(dat = dat, rank_x = "x", rank_y = "y"), 0.6)
  }
)

test_that("Testing makeAverageMovement",
  {
    expect_equal(makeAverageMovement(dat = dat, rank_x = "x", rank_y = "y"), 1.1)
  }
)

test_that("Testing makeWGM",
  {
    expect_equal(makeWGM(dat = dat, rank1 = "x", rank2 = "y"), 0.8482143)
  }
)

test_that("Testing makeOriginSpecific",
  {
    expect_equal(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "top", variety = "total"), 1)
    expect_equal(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "top", variety = "far"), 1)
    expect_equal(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "bottom", variety = "total"), 0.5)
    expect_equal(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "bottom", variety = "far"), 0.25)
    expect_error(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "top", variety = 2))
    expect_error(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = "bottom", variety = 2))
    expect_error(makeOriginSpecific(dat = dat, rank1 = "x", rank2 = "y", where = 2, variety = 2))
  }
)

test_that("Testing makeIndex",
  {
    expect_equal(makeIndex(dat = dat, rank_x = "x", rank_y = "y", index = 'prais_bibby'),
                 list(prais_bibby = 0.6))
    expect_equal(makeIndex(dat = dat, rank_x = "x", rank_y = "y", index = 'average_movement'),
                 list(average_movement = 1.1))
    expect_equal(makeIndex(dat = dat, rank_x = "x", rank_y = "y", index = 'wgm'),
                 list(wgm = 0.848214285714286))
    expect_equal(makeIndex(dat = dat, rank_x = "x", rank_y = "y", index = 'origin_specific'),
                 list(os_total_top = 1, os_far_top = 1, os_total_bottom = 0.5, os_far_bottom = 0.25))
    expect_error(makeIndex(dat = dat, rank_x = "x", rank_y = "y", index = 2))
  }
)

rm(dat)

test_that("Testing makeMobilityIndices",
  {
    expect_error(makeMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", indices = 2, num_ranks = 5, strict = TRUE))

    # No Exclude Value
    expect_equal(makeMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE),
                 list(prais_bibby = 0.536, average_movement = 0.704, wgm = 0.67,
                      os_total_top = 0.52, os_far_top = 0.28, os_total_bottom = 0.24, os_far_bottom = 0))
    expect_equal(makeMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "mixed", indices = "wgm", num_ranks = 5, strict = TRUE),
                 list(wgm = 0.561741156550731))
    expect_equal(makeMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "absolute", indices = "average_movement", bounds = seq(0, 500000, 10000), strict = TRUE),
                 list(average_movement = 2.752))
    expect_error(makeMobilityIndices(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE))

    # Exclude Value
    expect_error(makeMobilityIndices(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE, exclude_value = 0))
    expect_equal(makeMobilityIndices(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE, exclude_value = 0, rerank_exclude_value = 'as_existing_rank'),
                 list(prais_bibby = 0.744, average_movement = 1.36, wgm = 0.982104284809399,
                      os_total_top = 0.384615384615385, os_far_top = 0.307692307692308,
                      os_total_bottom = 0.702702702702703, os_far_bottom = 0.432432432432432))
    expect_equal(makeMobilityIndices(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t9", type = "mixed", indices = "all", num_ranks = 5, strict = TRUE, exclude_value = 0, rerank_exclude_value = 'as_existing_rank'),
                 list(prais_bibby = 0.304, average_movement = 0.56, wgm = 0.591005634430861,
                      os_total_top = 0.384615384615385, os_far_top = 0.384615384615385,
                      os_total_bottom = 0.0810810810810811, os_far_bottom = 0))
    expect_equal(makeMobilityIndices(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t9", type = "mixed", indices = "all", num_ranks = 5, strict = TRUE, exclude_value = 0, rerank_exclude_value = 'as_new_rank'),
                 list(prais_bibby = 0.736, average_movement = 1.056, wgm = 0.68714812432561,
                      os_total_top = 0.384615384615385, os_far_top = 0.384615384615385,
                      os_total_bottom = 0.950819672131147, os_far_bottom = 0.0655737704918033))
  }
)

test_that("Testing makeBootstrapSamples",
  {
    set.seed(8322)
    expect_equal(makeBootstrapSamples(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE, bootstrap_iter = 3),
                 structure(list(prais_bibby = c(0.536, 0.416, 0.544), average_movement = c(0.704, 0.6, 0.664),
                                wgm = c(0.67, 0.517386113611361, 0.680411356850433), os_total_top = c(0.52, 0.28, 0.56),
                                os_far_top = c(0.28, 0.28, 0.32), os_total_bottom = c(0.24, 0.208333333333333, 0.32),
                                os_far_bottom = c(0, 0, 0)), row.names = c(NA, 3L), class = "data.frame"))
    expect_error(makeBootstrapSamples(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", indices = "all", num_ranks = 5, strict = TRUE, bootstrap_iter = "hello"))
  }
)

test_that("Testing getMobilityIndices",
  {
    expect_error(getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", num_ranks = 5, intervals = TRUE, interval_pct = "a", bootstrap_iter = 3))
    expect_error(getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", num_ranks = 5, intervals = TRUE, interval_pct = 5, bootstrap_iter = 3))
    set.seed(8322)
    expect_equal(getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", num_ranks = 5, intervals = TRUE, bootstrap_iter = 3),
                 list(average_movement = 0.704, average_movement_intervals = list(lower = c(`2.5%` = 0.6032), upper = c(`97.5%` = 0.702), interval_bounds = c(0.025, 0.975)),
                      os_far_bottom = 0, os_far_bottom_intervals = list(lower = c(`2.5%` = 0), upper = c(`97.5%` = 0), interval_bounds = c(0.025, 0.975)),
                      os_far_top = 0.28, os_far_top_intervals = list(lower = c(`2.5%` = 0.28), upper = c(`97.5%` = 0.318), interval_bounds = c(0.025, 0.975)),
                      os_total_bottom = 0.24, os_total_bottom_intervals = list(lower = c(`2.5%` = 0.209916666666667), upper = c(`97.5%` = 0.316), interval_bounds = c(0.025, 0.975)),
                      os_total_top = 0.52, os_total_top_intervals = list(lower = c(`2.5%` = 0.292), upper = c(`97.5%` = 0.558), interval_bounds = c(0.025,  0.975)),
                      prais_bibby = 0.536, prais_bibby_intervals = list(lower = c(`2.5%` = 0.422), upper = c(`97.5%` = 0.5436), interval_bounds = c(0.025, 0.975)),
                      wgm = 0.67, wgm_intervals = list(lower = c(`2.5%` = 0.525016807930793), upper = c(`97.5%` = 0.679890789007911), interval_bounds = c(0.025, 0.975))))
  }
)


