test_that("normalise() works", {

  dt <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12)) |>
    distribution_fit(.fit = dist_gamma(.agg, method = "lmoms"))

  expect_snapshot(dt |> normalise(index = norm_quantile(.fit)))


})


test_that("on errors", {

  # not an index table object
  expect_snapshot(
    tenterfield |> normalise(index = norm_quantile(.fit)),
    error = TRUE)

  # input is not a dimension reduction recipe
  expect_snapshot(
    hdi |> init() |> normalise(index = rescale_zscore(life_exp)),
    error = TRUE)
})


