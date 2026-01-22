test_that("multiplication works", {
  hdi_paras <- hdi_scales |>
    dplyr::add_row(dimension = "Education", name = "Education",
                   var = "sch", min = 0, max = 0) |>
    dplyr::mutate(weight = c(1/3, 0, 0, 1/3, 1/3),
                  weight2 = c(0.1, 0, 0, 0.8, 0.1),
                  weight3 = c(0.8, 0, 0, 0.1, 0.1),
                  weight4 = c(0.1, 0, 0, 0.1, 0.8))


  dt <- hdi |>
    init(id = country) |>
    add_paras(hdi_paras, by = var) |>
    rescaling(life_exp = rescale_minmax(life_exp, min = min, max = max)) |>
    rescaling(exp_sch = rescale_minmax(exp_sch, min = min, max = max)) |>
    rescaling(avg_sch = rescale_minmax(avg_sch, min = min, max = max)) |>
    rescaling(gni_pc = rescale_minmax(gni_pc, min = min, max = max)) |>
    dimension_reduction(sch = aggregate_manual(~(exp_sch + avg_sch)/2)) |>
    dimension_reduction(index = aggregate_linear(~c(life_exp, sch, gni_pc),
                                                 weight = weight))


  dt2 <- dt |>
    swap_values(.var = "index", .param = weight,
                .value = list(weight2, weight3, weight4))
  expect_snapshot(dt2)
  expect_snapshot(augment(dt2))

  dt22 <- dt |>
    swap_values(.var = "index", .param = weight,
                .value = list(weight2))
  expect_snapshot(dt22)
  expect_snapshot(augment(dt22))

  dt3 <- dt |>
    swap_exprs(.var = index, .exprs = list(
               aggregate_geometrical(~c(life_exp, sch, gni_pc))))
  expect_snapshot(dt3)
  expect_snapshot(augment(dt3))
})
