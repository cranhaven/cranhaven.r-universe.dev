library(testthat)
library(arena2r)
context("Test tests_arena2r()")

my_path = getwd()

# Generate Plots and Dataframes

results = arena2r::get_simulation_results(my_path)

summary = arena2r::get_statistics_summary(sim_results = results)

plot_conf = arena2r::plot_confint(results, "Entity 1.NumberIn")

plot_box = arena2r::plot_box(results, "Entity 1.NumberIn")

plot_scat = arena2r::plot_scatter(results, "Entity 1.NumberIn", "Entity 1.NumberOut")

test_that("get_simulation_results retorna data.frame com 4 colunas", {
  expect_equal(length(names(results)), 4)
  expect_type(object = results, type = "list")
})


test_that("get_statistics_summary retorna data.frame com 10 colunas", {
  expect_equal(length(names(summary)), 10)
  expect_type(object = summary, type = "list")
})

test_that("plots_sao_gerados", {
  expect_type(object = plot_conf, type = "list")
  expect_type(object = plot_scat, type = "list")
  expect_type(object = plot_box, type = "list")
})
