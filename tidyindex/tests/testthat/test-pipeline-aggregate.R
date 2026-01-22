# library(testthat)
# library(dplyr)
# # test on ts_aggregation
# test_that("aggregation works for the base case - one station", {
#   res <- tenterfield |>
#     init(id = id, time = ym, indicators = prcp:tavg) |>
#     aggregate(.scale = 12,  .var = prcp)
#   # With a scale of 12, the first period is "1990 Dec"
#   expect_equal(res$data$ym[1], tsibble::yearmonth("1990 Dec"))
#   expect_equal(res$data$.agg[1], sum(tenterfield$prcp[1:12]))
#
# })
#
# test_that("aggregate() works for multiple scales - one station", {
#   res <- tenterfield |>
#     init(id = id, time = ym, indicators = prcp:tavg) |>
#     aggregate(.scale = c(12, 24),  .var = prcp)
#   # two new columns are created: .scale, .agg
#   expect_equal(ncol(tenterfield) + 2, ncol(res$data))
#
#   # the aggregation work
#   expect_equal(res$data$.agg[1], sum(tenterfield$prcp[1:12]))
#   expect_equal(res$data |> filter(.scale == 24) |> pull(.agg) |> .[[1]],
#                sum(tenterfield$prcp[1:24]))
#
# })
#
# test_that("na.rm argument in aggregate() work", {
#   # with na.rm
#   res <- tenterfield |>
#     init(id = id, time = ym, indicators = prcp:tavg) |>
#     aggregate(.scale = 12, .var = prcp)
#   expect_equal(nrow(tenterfield)  - 11, nrow(res$data))
#
#   # without na.rm
#   res <- tenterfield |>
#     init(id = id, time = ym, indicators = prcp:tavg) |>
#     aggregate(.scale = 12,  .var = prcp, na.rm = FALSE)
#   expect_equal(nrow(tenterfield),  nrow(res$data))
#   expect_true(all(is.na(head(res$data$.agg, 11))))
# })
#
#
# # test on ts_aggregation
# # test_that("ts_aggregation() works for the base case - two stations", {
# #   res <- two |>
# #     ts_aggregate(scale = 12, date = ym, id = id, col = prcp) |>
# #     filter(!is.na(prcp_agg))
# #
# #
# #   expect_equal(nrow(two) - 11 * 2, nrow(res))
# #   # With a scale of 12, the first period is "1990 Dec
# #   expect_equal(res$ym[1], yearmonth("1990 Dec"))
# #   # first prcp_agg
# #   from_result <- res |> group_by(id) |>
# #     filter(row_number() == 1) |>
# #     pull(prcp_agg)
# #   from_data <- two |>
# #     group_by(id) |>
# #     filter(row_number() <= 12) |>
# #     summarise(prcp = sum(prcp)) |>
# #     pull(prcp)
# #   expect_equal(from_result, from_data)
# #
# # })
# #
# # test_that("ts_aggregation() works for multiple scales - two stations", {
# #   two_scales <- c(6, 12)
# #   res <- two |>
# #     ts_aggregate(scale = two_scales, date = ym, id = id, col = prcp)
# #   # two new columns are created
# #   expect_equal(ncol(two) + length(two_scales), ncol(res))
# #
# #   # TODO there should be a test on names so they follow the same structure
# #   #setdiff(names(res), names(one))
# #
# #   # the aggregation work
# #   from_result <- res |>
# #     filter(!is.na(prcp_agg_6)) |>
# #     group_by(id) |>
# #     filter(row_number() == 1) |>
# #     pull(prcp_agg_6)
# #   from_data <- two |>
# #     group_by(id) |>
# #     filter(row_number() <= 6) |>
# #     summarise(prcp = sum(prcp)) |>
# #     pull(prcp)
# #   expect_equal(from_result, from_data)
# #
# # })
