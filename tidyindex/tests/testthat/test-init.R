test_that("create a index table object: init()", {
  expect_snapshot(hdi |> as.data.frame() |> init())
  expect_snapshot(init(gggi))
  expect_snapshot(gggi |> as.list() |> init(), error = TRUE)
})

test_that("can attach metadata: add_paras()", {
  expect_snapshot(gggi|> add_paras(gggi_weights, by = "variable"), error = TRUE)
  expect_snapshot(init(gggi) |> add_paras(gggi_weights, by = "variable"))
})

# more test on print once there are steps to summarise
