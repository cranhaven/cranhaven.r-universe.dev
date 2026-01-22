test_that("temporal_agg() works", {

  expect_snapshot(
    tenterfield |>
      init(id = id, time = ym) |>
      temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12))
  )

  expect_snapshot(
    tenterfield |>
      init(id = id, time = ym) |>
      temporal_aggregate(temporal_rolling_window(prcp, scale = c(12, 24)))
  )


  # works on multiple stations need to supply id with init
  do_temp_agg <- function(data, stations){
    data |>
      dplyr::filter(id %in% stations) |>
      init(id = id, time = ym) |>
      temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12))
  }
  dt1 <- queensland |> do_temp_agg(stations = "ASN00029038")
  dt2 <- queensland |> do_temp_agg(stations = "ASN00029127")
  dt3 <- queensland |>
    dplyr::filter(id %in% c("ASN00029038", "ASN00029127")) |>
    init(id = id, time = ym) |>
    temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12))

  grab_head <- function(data) data  |> dplyr::slice_head(n = 5)|> dplyr::pull(.agg)
  expect_equal(
    dt3$data |> group_by(id) |> grab_head(),
    c(grab_head(dt1$data), grab_head(dt2$data)))
})

test_that("check on temporal order and id", {

  # no id for a single station is good
  expect_snapshot(
    tenterfield |>
      init(time = ym) |>
      temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12)))

  # no time is a no
  expect_snapshot(
    tenterfield |>
      init(id = id) |>
      temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12)),
    error = TRUE)

  # temporal index in the wrong order is a no
  expect_snapshot(
    tenterfield |>
      # perturb the order
      dplyr::slice_sample(n = 369) |>
      init(id = id, time = ym) |>
      temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12)),
    error = TRUE)




})

test_that("on errors", {

  # not an index table object
  expect_snapshot(
    tenterfield |>
      temporal_aggregate(temporal_rolling_window(prcp, scale = 12)),
    error = TRUE)

  # input is not a dimension reduction recipe
  expect_snapshot(
    tenterfield |> init() |> temporal_aggregate(index = rescale_zscore(prcp)),
    error = TRUE)


})
