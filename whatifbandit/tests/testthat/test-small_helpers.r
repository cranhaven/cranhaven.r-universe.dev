test_that("Formula Parsing", {
  form <- list(
    form1 = s ~ c + block(a, d) + cluster(g),
    form2 = s ~ a + block(b),
    form3 = s ~ a + cluster(p),
    form4 = s ~ c + d
  )

  truth <- list(
    list(
      condition_col = "c",
      success_col = "s",
      block_cols = c("a", "d"),
      cluster_col = "g"
    ),
    list(
      condition_col = "a",
      success_col = "s",
      block_cols = "b",
      cluster_col = NULL
    ),
    list(
      condition_col = "a",
      success_col = "s",
      block_cols = NULL,
      cluster_col = "p"
    ),
    list(
      condition_col = "c",
      success_col = "s",
      block_cols = NULL,
      cluster_col = NULL
    )
  )
  purrr::walk2(form, truth, \(f, t) {
    expect_equal(formula_parse(f), t)
  })
})

test_that("Finalize Prior List", {
  test_list <- list(
    mab_condition = c("b", "a"),
    n = c(5, 10),
    successes = c(3, 7)
  )
  conditions <- c("d", "a", "b", "c")

  truth <- list(
    n = c(a = 10, b = 5, c = 0, d = 0),
    successes = c(a = 7, b = 3, c = 0, d = 0)
  )

  expect_equal(finalize_prior_list(test_list, conditions = conditions), truth)
})


test_that("Summary to Matrix", {
  set.seed(5)
  fail <- runif(10)
  success <- runif(10)
  treatment_block <- paste0("T", 1:10)

  df <- data.frame(
    failure_rate = fail,
    success_rate = success,
    random = rbinom(1, 10, 0.3),
    random2 = sample(10),
    treatment_block = treatment_block
  )
  expect_equal(
    summary_to_matrix(df),
    matrix(
      c(fail, success),
      ncol = 2,
      nrow = 10,
      dimnames = list(treatment_block, c("failure_rate", "success_rate"))
    )
  )
})

test_that("Period Sizes", {
  data <- data.frame(period_number = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 5))
  truth <- c(3, 3, 2, 1, 1)

  compute_period_sizes(data) |> expect_equal(truth)
  compute_period_sizes(data.table::as.data.table(data)) |>
    expect_equal(truth)
})

test_that("Fill Missing Conditions", {
  df <- data.frame(
    mean = 1,
    se = 5,
    mab_condition = "a",
    estimator = "The Best Estimator"
  )
  filled_df <- dplyr::bind_rows(
    df,
    tibble::tibble(
      mean = NA,
      se = NA,
      mab_condition = "b",
      estimator = "The Best Estimator"
    )
  )
  dt <- data.table::as.data.table(df)
  filled_dt <- data.table::as.data.table(filled_df)

  purrr::walk(list(list(df, filled_df), list(dt, filled_dt)), \(frame) {
    expect_equal(
      fill_missing_conditions(frame[[1]], "a", estimator = "The Best Estmator"),
      frame[[1]]
    )
    expect_equal(
      fill_missing_conditions(
        frame[[1]],
        c("a", "b"),
        estimator = "The Best Estimator"
      ),
      frame[[2]]
    )
  })
})

test_that("As Named Vector", {
  val <- rnorm(26)
  names(val) <- letters
  df <- data.frame(label = letters, value = val)
  expect_equal(as_named_vec(df, "value", "label"), val)
})


test_that("Compute Lookback", {
  expect_equal(compute_lookback(NULL, 5), 1)
  expect_equal(compute_lookback(5, 10), 5)
  expect_equal(compute_lookback(10, 5), 1)
})

test_that("Create Conditions", {
  df <- data.frame(c = sample(c("T", "A", "B"), 15, replace = TRUE))
  f <- purrr::partial(
    create_conditions,
    data = df,
    condition_col = "c",
    control_condition = "T"
  )
  truth <- stats::setNames(
    c("A", "B", "T"),
    c("treatment", "treatment", "control")
  )
  expect_equal(f(), truth)
})


test_that("Extraction Test", {
  p <- matrix(c(1, 2), ncol = 1, dimnames = list(c("A", "B"), NULL))
  expect_equal(
    extract_success_prob(p, c("A", "B", "B", "A")),
    c(1, 2, 2, 1)
  )
  p <- matrix(
    c(1, 2, 3, 4),
    ncol = 2,
    dimnames = list(c("A", "B"), c("C", "D"))
  )
  expect_equal(
    extract_success_prob(p, c("A", "B", "B", "A"), c("C", "D", "C", "D")),
    c(1, 4, 2, 3)
  )
})

test_that("group_prop returns correct proportions", {
  df <- data.frame(
    grp = c("a", "a", "b", "c"),
    y = c(1, 0, 1, 1)
  )

  dt <- data.table::as.data.table(df)

  expected <- c(a = 0.5, b = 0.25, c = 0.25)

  expect_equal(group_prop(df, "grp"), expected)
  expect_equal(group_prop(dt, "grp"), expected)
})

test_that("boot_null_counts returns correct grouped counts", {
  df <- data.frame(
    grp = c("a", "a", "b", "b", "b"),
    y = c(1, 0, 1, 1, 0)
  )

  dt <- data.table::as.data.table(df)

  expected <- data.frame(
    grp = c("a", "b"),
    n = c(2, 3),
    s = c(1, 2)
  )

  expect_equal(
    as.data.frame(boot_null_counts(df, "y", "grp")),
    expected
  )

  expect_equal(
    as.data.frame(boot_null_counts(dt, "y", "grp")),
    expected
  )
})

test_that("boot_null_counts returns correct overall counts", {
  df <- data.frame(
    grp = c("a", "a", "b", "b", "b"),
    y = c(1, 0, 1, 1, 0)
  )

  dt <- data.table::as.data.table(df)

  expected <- data.frame(
    n = 5,
    s = 3
  )

  expect_equal(
    boot_null_counts(df, "y"),
    expected
  )

  expect_equal(
    as.data.frame(boot_null_counts(dt, "y")),
    expected
  )
})

test_that("CR1 function returns proper values", {
  n <- 100
  k <- 5
  g <- 10

  var <- runif(5)

  var1 <- (var * (n - 1) * (g)) / ((n - k) * (g - 1))
  var2 <- cr1(x = var, n = n, k = k, g = g)
  expect_equal(var1, var2)
})
