library(dplyr)
library(lmomco)
library(generics)
test_that("compute_indexes() works", {

  res <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    compute_indexes(
      spi = idx_spi(),
      spei = idx_spei(.lat = lat, .tavg = tavg),
      edi = idx_edi()
    )

  expect_snapshot(res)

  # with multiple scales
  res2 <- tenterfield |>
    mutate(month = lubridate::month(ym)) |>
    init(id = id, time = ym, group = month) |>
    compute_indexes(
      spi = idx_spi(),
      spei = idx_spei(.lat = lat, .tavg = tavg, .scale = c(12, 24)),
      edi = idx_edi()
    )
  expect_snapshot(res2)
})




