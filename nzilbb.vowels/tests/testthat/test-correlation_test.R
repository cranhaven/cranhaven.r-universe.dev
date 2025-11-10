test_that(
  "Invalid cor.methods don't work", {
  expect_error(
    correlation_test(
      onze_intercepts |> select(-speaker),
    cor.method="I'm not a correlation method"),
    "pearson" # error message contains 'pearson'
  )
})

test_that(
  "Error raised for non-natural number value for `n`. (-ve)", {
  expect_error(
    correlation_test(onze_intercepts |> select(-speaker), n=-5),
    'positive integer'
  )
})

test_that(
  "Error raised for non-natural number value for `n`. (Real)", {
  expect_error(
    correlation_test(onze_intercepts |> select(-speaker), n=2.5),
    'positive integer'
  )
})

test_that(
  "Correlation summary returns expected result", {
    set.seed(10)
    expect_snapshot(
      summary(correlation_test(onze_intercepts |> select(-speaker), n=10)),
    )
  }
)
