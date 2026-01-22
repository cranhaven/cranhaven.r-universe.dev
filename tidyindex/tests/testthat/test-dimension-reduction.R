test_that("dimension reduction works", {
  tmp <- gggi |>
    init() |>
    add_paras(gggi_weights, by = variable) |>
    dimension_reduction(
      index_new = aggregate_linear(
        ~labour_force_participation:years_with_female_head_of_state,
        weight = weight))
  expect_snapshot(tmp)


  tmp <- hdi |>
    init() |>
    rescaling(life_exp = rescale_minmax(life_exp, min = 20, max = 85)) |>
    rescaling(exp_sch = rescale_minmax(exp_sch, min = 0, max = 18)) |>
    rescaling(avg_sch = rescale_minmax(avg_sch, min = 0, max = 15)) |>
    rescaling(gni_pc = rescale_minmax(gni_pc, min = log10(100), max = log10(75000)))

  tmp2 <- tmp |>
    dimension_reduction(sch = aggregate_manual(~(exp_sch + avg_sch) / 2))
  expect_snapshot(tmp2)

  tmp3 <- tmp2 |>
    dimension_reduction(index = aggregate_geometrical(~c(life_exp, sch, gni_pc)))
  expect_snapshot(tmp3)



})


test_that("on errors", {

  # not an index table object
  expect_snapshot(
    hdi |>
      dimension_reduction(eco = aggregate_manual(
        ~labour_force_participation * 0.199 +
          wage_equality_for_similar_work * 0.31)),
    error = TRUE)


  # input is not a dimension reduction recipe
  expect_snapshot(
    hdi |> init() |> dimension_reduction(index = rescale_zscore(life_exp)),
    error = TRUE)
})
