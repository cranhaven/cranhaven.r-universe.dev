test_that("variable transformation works", {
  expect_snapshot(
    hdi |> init() |> variable_trans(gni_pc = trans_log10(gni_pc))
    )

  expect_snapshot(
    hdi |> init() |> variable_trans(gni_pc = trans_square_root(gni_pc))
  )

  expect_snapshot(
    hdi |> init() |> variable_trans(gni_pc = trans_cubic_root(gni_pc))
  )

  expect_snapshot(
    hdi |> init() |> variable_trans(gni_pc = trans_quadratic(gni_pc))
  )
})


test_that("on errors", {

  # data is not an index table object
  expect_snapshot(
    hdi |> variable_trans(gni_pc = trans_log10(gni_pc)),
    error = TRUE)


  # input is not a variable transformation recipe
  expect_snapshot(
    hdi |> init() |> variable_trans(index = rescale_zscore(life_exp)),
    error = TRUE)



})
