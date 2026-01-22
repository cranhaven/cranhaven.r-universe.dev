test_that("trans_thornthwaite() works", {

  expect_snapshot(
    tenterfield |>
      init() |>
      variable_trans(pet = trans_thornthwaite(tavg, lat = -29))
  )

  expect_snapshot(
    tenterfield |>
      init() |>
      variable_trans(pet = trans_thornthwaite(tavg, lat = lat))
  )

  do_var_trans <- function(data, stations){
    data |>
      dplyr::filter(id %in% stations) |>
      init(id = id, time = ym) |>
      variable_trans(pet = trans_thornthwaite(tavg, lat = lat))
  }

  dt1 <- aus_climate |> do_var_trans("ASN00002012")
  dt2 <- aus_climate |> do_var_trans("ASN00003003")
  dt3 <- aus_climate |>
    dplyr::filter(id %in% c("ASN00002012", "ASN00003003")) |>
    init(id = id, time = ym) |>
    variable_trans(pet = trans_thornthwaite(tavg, lat = lat))
  grab_head <- function(data) data  |> dplyr::slice_head(n = 5)|> dplyr::pull(pet)
  expect_equal(
    dt3$data |> group_by(id) |> grab_head(),
    c(grab_head(dt1$data), grab_head(dt2$data)))

})


test_that("idx_spi() works", {
  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_spi(.dist = dist_gamma())
  expect_snapshot(res)

  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_spi(.dist = dist_gamma(), .scale = c(12, 24))
  expect_snapshot(res)

  # case where multiple scales and multiple distributions

})

test_that("idx_spei() works", {

  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_spei(.lat = lat, .tavg = tavg, .scale = 12)

  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_spei(.lat = lat, .tavg = tavg, .scale = c(12, 24))
  expect_snapshot(res)
})


test_that("idx_rdi() works", {
  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_rdi(.lat = lat, .tavg = tavg, .scale = 12)
  expect_snapshot(res)
})

test_that("idx_edi() works", {
  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    idx_edi(.lat = lat, .tavg = tavg, .scale = 12)
  expect_snapshot(res)
})

