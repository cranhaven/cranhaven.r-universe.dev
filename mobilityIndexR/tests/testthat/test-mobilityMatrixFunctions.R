set.seed(8322)

test_that("Testing checkInputs",
  {
    expect_error(checkInputs(dat = 1, col_in = "x", col_out = "y"))
    expect_error(checkInputs(dat = data.frame(), col_in = "x", col_out = "y"))
    expect_error(checkInputs(dat = data.frame(x = c(1,2)), col_in = "x", col_out = "y"))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,1)), col_in = "x", col_out = "y"))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = 1, col_out = "y"))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = 1))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", num_ranks = "a"))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", num_ranks = 3))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", mixed_col = 5))
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", bounds = c(2, 3)),
                 regexp ="At least one data value falls below the first bound.")
    expect_error(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", bounds = c(0, 1)),
                 regexp ="At least one data value falls above the last bound.")
    expect_silent(checkInputs(dat = data.frame(x = c(1,2), id = c(1,2)), col_in = "x", col_out = "y", bounds = c(1, 2)))
  }
)

test_that("Testing checkTooManyRanks",
  {
    expect_error(checkTooManyRanks(df = data.frame(x = c(1, 2, 3, 1, 1)), col_in = "x", num_ranks = 3))
    expect_error(checkTooManyRanks(df = data.frame(x = c(1, 2, 3, 1, 1)), col_in = "x", num_ranks = 3, exclude_value = 2))
    expect_silent(checkTooManyRanks(df = data.frame(x = c(1, 2, 3, 1, 2)), col_in = "x", num_ranks = 2))
    expect_silent(checkTooManyRanks(df = data.frame(x = c(1, 2, 3, 1, 2, 3, 4)), col_in = "x", num_ranks = 2, exclude_value = 2))
  }
)

test_that("Testing makeRelativeRanks",
  {
    # No Exclude Value
    expect_equal(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", num_ranks = 2, strict = FALSE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5),
                                            x = c(1.00000000008964, 2.0000000000367, 3.00000000007084, 1.00000000024406, 2.00000000004385),
                                            y = c(1, 2, 2, 1, 2)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(`0%` = 1.00000000008964, `50%` = 2.0000000000367, `100%` = 3.00000000007084)))
    expect_equal(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", num_ranks = 2, strict = TRUE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 1, 2), y = c(1, 2, 2, 1, 2)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(`0%` = 1, `50%` = 2, `100%` = 3)))

    # Exclude Value
    expect_error(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7)), col_in = "x", col_out = "y", num_ranks = 2, strict = TRUE, exclude_value = "a"))
    expect_error(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7)), col_in = "x", col_out = "y", num_ranks = 2, strict = FALSE, exclude_value = "a"))
    expect_equal(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7)), col_in = "x", col_out = "y", num_ranks = 2, strict = TRUE, exclude_value = 2),
                 list(data = structure(list(id = c(2, 5, 1, 3, 4, 6, 7), x = c(2, 2, 1, 3, 1, 3, 4), y = c(0, 0, 1, 2, 1, 2, 2)), row.names = c(2L, 5L, 1L, 3L, 4L, 6L, 7L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 1, `50%` = 3, `100%` = 4)))
    expect_equal(makeRelativeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7)), col_in = "x", col_out = "y", num_ranks = 2, strict = FALSE, exclude_value = 2),
                 list(data = structure(list(id = c(2, 5, 1, 3, 4, 6, 7), x = c(2, 2, 1.00000000004422, 3.00000000005966, 1.00000000013354, 3.00000000002057, 4.0000000000171),
                                            y = c(0, 0, 1, 2, 1, 2, 2)), row.names = c(2L, 5L, 1L, 3L, 4L, 6L, 7L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 1.00000000004422, `50%` = 3.00000000002057, `100%` = 4.0000000000171)))
  }
)

test_that("Testing makeMixedRanks",
  {
    # No Exclude Value
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5), z = c(4, 1, 1, 8, 3)), col_in = "x", col_out = "y", mixed_col = "x", num_ranks = 2, strict = FALSE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5),
                                            x = c(1.00000000002378, 2.00000000009332, 3.00000000002765, 1.00000000002668, 2.00000000005718),
                                            y = c(1, 2, 2, 1, 2)), row.names = c(NA, -5L), class = "data.frame"),
                 bounds = c(`0%` = 2.37772024291871e-11, `50%` = 2.00000000005718, `100%` = 4.00000000002765)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5), z = c(4, 1, 1, 8, 3)), col_in = "x", col_out = "y", mixed_col = "x", num_ranks = 2, strict = TRUE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5),
                                            x = c(1, 2, 3, 1, 2), y = c(1, 2, 2, 1, 2)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(`0%` = 0, `50%` = 2, `100%` = 4)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5), z = c(4, 1, 1, 8, 3)), col_in = "x", col_out = "y", mixed_col = "z", num_ranks = 2, strict = FALSE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 1, 2), y = c(1, 1, 1, 1, 1)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(`0%` = 0, `50%` = 3.00000000007867, `100%` = 9.00000000004326)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5), z = c(4, 1, 1, 8, 3)), col_in = "x", col_out = "y", mixed_col = "z", num_ranks = 2, strict = TRUE),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 1, 2), y = c(1, 1, 2, 1, 1)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(`0%` = 0, `50%` = 3, `100%` = 9)))
    # Exclude Value
    expect_error(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5), z = c(4, 1, 1, 8, 3)), col_in = "x", col_out = "y", mixed_col = "z", num_ranks = 2, strict = TRUE, exclude_value = "a"))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7), z = c(4, 1, 1, 8, 3, 4, 5)), col_in = "x", col_out = "y", mixed_col = "x", num_ranks = 2, strict = FALSE, exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 6, 7, 2, 5), x = c(1, 3, 1, 3, 4, 2, 2), y = c(1, 1, 1, 1, 2, 0, 0)), row.names = c(1L, 3L, 4L, 6L, 7L, 2L, 5L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 0, `50%` = 3.00000000003322, `100%` = 5.00000000004841)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7), z = c(4, 1, 1, 8, 3, 4, 5)), col_in = "x", col_out = "y", mixed_col = "x", num_ranks = 2, strict = TRUE, exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 6, 7, 2, 5), x = c(1, 3, 1, 3, 4, 2, 2), y = c(1, 2, 1, 2, 2, 0, 0)), row.names = c(1L, 3L, 4L, 6L, 7L, 2L, 5L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 0, `50%` = 3, `100%` = 5)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7), z = c(4, 1, 1, 8, 3, 4, 5)), col_in = "x", col_out = "y", mixed_col = "z", num_ranks = 2, strict = FALSE, exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 6, 7, 2, 5), x = c(1, 3, 1, 3, 4, 2, 2), y = c(1, 1, 1, 1, 1, 0, 0)), row.names = c(1L, 3L, 4L, 6L, 7L, 2L, 5L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 0, `50%` = 4.00000000004247, `100%` = 9.00000000005546)))
    expect_equal(makeMixedRanks(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), id = c(1, 2, 3, 4, 5, 6, 7), z = c(4, 1, 1, 8, 3, 4, 5)), col_in = "x", col_out = "y", mixed_col = "z", num_ranks = 2, strict = TRUE, exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 6, 7, 2, 5), x = c(1, 3, 1, 3, 4, 2, 2), y = c(1, 1, 1, 1, 2, 0, 0)), row.names = c(1L, 3L, 4L, 6L, 7L, 2L, 5L), class = "data.frame"),
                      bounds = c(exclude_value = 2, `0%` = 0, `50%` = 4, `100%` = 9)))
  }
)

test_that("Testing makeAbsoluteRanks",
  {
    expect_equal(makeAbsoluteRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", bounds = c(0, 2, 5)),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 1, 2), y = c(1, 2, 2, 1, 2)), row.names = c(NA, -5L), class = "data.frame"),
                      bounds = c(0, 2, 5)))
    expect_equal(makeAbsoluteRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", bounds = c(0, 2, 5), exclude_value = 10),
                 list(data = structure(list(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 1, 2), y = c(1, 2, 2, 1, 2)), row.names = c(NA, 5L), class = "data.frame"),
                      bounds = c(10, 0, 2, 5)))
    expect_equal(makeAbsoluteRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", bounds = c(0, 2, 5), exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 2, 5), x = c(1, 3, 1, 2, 2), y = c(1, 2, 1, 0, 0)), row.names = c(1L, 3L, 4L, 2L, 5L), class = "data.frame"),
                      bounds = c(2, 0, 2, 5)))
  }
)

test_that("Testing makeRanks",
  {
    expect_equal(makeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", type = "absolute", bounds = c(0, 2, 5), exclude_value = 2),
                 list(data = structure(list(id = c(1, 3, 4, 2, 5), x = c(1, 3, 1, 2, 2), y = c(1, 2, 1, 0, 0)), row.names = c(1L, 3L, 4L, 2L, 5L), class = "data.frame"),
                      bounds = c(2, 0, 2, 5)))
    expect_error(makeRanks(dat = data.frame(x = c(1, 2, 3, 1, 2), id = c(1, 2, 3, 4, 5)), col_in = "x", col_out = "y", type = 2, bounds = c(0, 2, 5), exclude_value = 2))
  }
)

test_that("Testing makeTMatrix",
  {
    expect_equal(makeTMatrix(dat = data.frame(x = c(1, 1, 2, 1), y = c(1, 2, 2, 1)), rank_x = "x", rank_y = "y", probs = FALSE),
                 structure(c(2L, 0L, 1L, 1L), .Dim = c(2L, 2L), .Dimnames = structure(list(c("1", "2"), c("1", "2")), .Names = c("", "")), class = "table"))
    expect_equal(makeTMatrix(dat = data.frame(x = c(1, 1, 2, 1), y = c(1, 2, 2, 1)), rank_x = "x", rank_y = "y", probs = TRUE),
                 structure(c(0.5, 0, 0.25, 0.25), class = "table", .Dim = c(2L, 2L), .Dimnames = structure(list(c("1", "2"), c("1", "2")), .Names = c("", ""))))
  }
)

test_that("Testing modifyExcludeValueRank",
  {
    expect_error(modifyExcludeValueRank(rerank_exclude_value = "blah"))
  }
)

test_that("Testing modifyExcludeValueNewRank",
  {
    expect_equal(modifyExcludeValueNewRank(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 0, 2, 1, 0, 2, 2)),
                              col = "x", rank = "rank", bounds = c(2, 1, 3, 4), exclude_value = 2),
                 structure(list(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 2, 4, 1, 2, 4, 4)), row.names = c(NA, -7L), class = "data.frame"))
    expect_equal(modifyExcludeValueNewRank(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 0, 2, 1, 0, 2, 2)),
                                           col = "x", rank = "rank", bounds = c(2, 1, 2, 4), exclude_value = 2),
                 structure(list(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 2, 3, 1, 2, 3, 3)), row.names = c(NA, -7L), class = "data.frame"))
  }
)

test_that("Testing modifyExcludeValueExistingRank",
  {
    expect_equal(modifyExcludeValueExistingRank(dat = data.frame(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 0, 2, 1, 0, 2, 2)),
                                           col = "x", rank = "rank", bounds = c(2, 1, 3, 4), exclude_value = 2),
                 structure(list(x = c(1, 2, 3, 1, 2, 3, 4), rank = c(1, 1, 2, 1, 1, 2, 2)), row.names = c(NA, -7L), class = "data.frame"))
  }
)

test_that("Testing getTMatrix",
  {
    expect_equal(getTMatrix(dat = incomeMobility, col_x = "t0", col_y = "t9", type = "relative", num_ranks = 5),
                 list(tmatrix = structure(c(0.152, 0.04, 0.008, 0, 0, 0.048, 0.08,
                                            0.04, 0.008, 0.024, 0, 0.04, 0.072, 0.056, 0.032, 0, 0.032, 0.056,
                                            0.064, 0.048, 0, 0.008, 0.024, 0.072, 0.096),
                                          class = "table",
                                          .Dim = c(5L, 5L),
                                          .Dimnames = structure(list(c("1", "2", "3", "4", "5"), c("1", "2", "3", "4", "5")), .Names = c("", ""))),
                      col_x_bounds = c(`0%` = 462,`20%` = 21543.4, `40%` = 42469.8, `60%` = 64061.6, `80%` = 77888.4, `100%` = 99557),
                      col_y_bounds = c(`0%` = 203.740677, `20%` = 18956.530796, `40%` = 35915.223828, `60%` = 57868.0499, `80%` = 89189.130402, `100%` = 446670.7755)))
  }
)
