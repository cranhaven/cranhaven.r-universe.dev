test_that("get_data works", {
  set.seed(123)
  data1 <- get_data(k = 3, n = 20, p = 0.2, iter = 100)

  expect_true(all(dim(data1) == c(100, 3)))
  expect_equal(attr(data1, "n"), 20)
  expect_equal(attr(data1, "p"), 0.2)
  expect_error(get_data(k = 3, n = 20, p = c(0.2, 0.5), iter = 100))

  set.seed(123)
  data2 <- get_data(k = 3, n = 20, p = 0.2, iter = 100, type = "bhmbasket")

  expect_s3_class(data2, "scenario_list")
  expect_true(all(data1 == data2$scenario_1$n_responders))
})

test_that("check_data_matrix works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  data1 <- get_data(k = 3, n = 20, p = 0.2, iter = 100)
  data2 <- get_data(k = 4, n = 20, p = 0.2, iter = 100)

  expect_error(check_data_matrix(data = data.frame(data1), design = design,
    n = 20, p = NULL, iter = 100))
  expect_error(check_data_matrix(data = data2, design = design, n = 20,
    p = NULL, iter = 100))
  expect_error(check_data_matrix(data = data1, design = design, n = 19,
    p = 0.2, iter = 100))
  expect_error(check_data_matrix(data = data1, design = design, n = 20,
    p = 0.5, iter = 100))
  expect_message(check_data_matrix(data = data1, design = design, n = 20,
    p = NULL, iter = 101))
  expect_equal(data1, check_data_matrix(data = data1, design = design,
    n = 20, p = NULL, iter = 100))
})

test_that("check_data_bhmbasket works", {
  design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
  data1 <- get_data(k = 3, n = 20, p = 0.2, iter = 100, type = "bhmbasket")
  data2 <- get_data(k = 3, n = 20, p = 0.2, iter = 100)
  data3 <- get_data(k = 4, n = 20, p = 0.2, iter = 100, type = "bhmbasket")

  expect_error(check_data_bhmbasket(data = data2, design = design,
    n = 20, p = NULL, iter = 100))
  expect_error(check_data_bhmbasket(data = data3, design = design, n = 20,
    p = NULL, iter = 100))
  expect_error(check_data_bhmbasket(data = data1, design = design, n = 19,
    p = 0.2, iter = 100))
  expect_error(check_data_bhmbasket(data = data1, design = design, n = 20,
    p = 0.5, iter = 100))
  expect_message(check_data_bhmbasket(data = data1, design = design, n = 20,
    p = NULL, iter = 101))
  expect_equal(data1, check_data_bhmbasket(data = data1, design = design, n = 20,
    p = NULL, iter = 100))
})

test_that("check_data_list works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 20, p = 0.2, iter = 100)
  scenarios <- get_scenarios(design, p1 = 0.5)
  scenario_list <- as.list(data.frame(scenarios))
  data_list <- lapply(scenario_list,
    function(x) get_data(k = 3, n = 20, p = x, iter = 1000))

  expect_error(check_data_list(data = data, scenarios = scenarios))
  expect_error(check_data_list(data = data_list[1:3], scenarios = scenarios))
})
