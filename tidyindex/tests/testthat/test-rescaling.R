test_that("error msgs", {
  expect_snapshot(hdi |> rescaling(life_exp2 = rescale_zscore(life_exp)), error = TRUE)

  dt <- hdi |> init()
  expect_snapshot(dt |> rescaling(life_exp2 = scale(dt$data$life_exp)), error = TRUE)
})


test_that("rescale calculation is correct", {
  dt <- hdi |> init()
  expect_snapshot(dt |> rescaling(life_exp2 = rescale_zscore(life_exp)))
  expect_snapshot(dt |> rescaling(life_exp2 = rescale_minmax(life_exp, min = 20, max = 85)))
  expect_snapshot(dt |> rescaling(life_exp2 = rescale_minmax(life_exp)))
  expect_snapshot(dt |> rescaling(life_exp2 = rescale_center(life_exp)))

})

test_that("rescaling() works with symbols", {
  dt <- hdi |>
    init(id = country) |>
    add_paras(hdi_scales, by = var) |>
    rescaling(life_exp = rescale_minmax(life_exp, min = min, max = max))

  expect_snapshot(dt)
})
