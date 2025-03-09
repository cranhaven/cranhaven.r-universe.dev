df1 <- data.frame(
  y  = 1:5,
  x1 = rnorm(5),
  x2 = rnorm(5),
  x3 = rnorm(5)
)

nodistribute = c("+", "-", "*", "^")

test_that("Basic matches work", {
  
  expect_equal(
    replace_call(quote(contains("x")), df1, matches = c("contains"), nodistribute),
    quote(x1 + x2 + x3)
  )
  
})

test_that("Nested matches work", {

  expect_equal(
    replace_call(quote(f(starts_with("x"))), df1, .select_helpers, nodistribute),
    quote(f(x1) + f(x2) + f(x3))
  )

})

test_that("Expression replacement works", {

  expect_equal(
    replace_expr(y ~ starts_with("x"), df1, .select_helpers, nodistribute),
    quote(y ~ x1 + x2 + x3)
  )

})
