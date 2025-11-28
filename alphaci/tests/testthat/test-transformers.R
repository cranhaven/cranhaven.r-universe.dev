transformers <- list(
  fisher = transformer_fisher,
  none = transformer_none,
  arcsin = transformer_arcsin,
  log = transformer_log
)


test_that("get_transformer works", {
  expect_error(get_transformer("lol"))
  names <- names(transformers)
  for (i in seq_along(transformers)) {
    expect_equal(get_transformer(names[i]), transformers[[i]])
  }
})

test_that("transformations have correct inverses", {
  for (elem in transformers) {
    expect_equal(elem$inv(elem$est(0.5)), 0.5)
    expect_equal(elem$est(elem$inv(0.5)), 0.5)
  }
})

test_that("transformations have correct formals", {
  for (elem in transformers) {
    expect_equal(names(elem), c("est", "sd", "inv"))
    expect_equal(names(formals(elem$sd)), c("est", "sd"))
  }
})
