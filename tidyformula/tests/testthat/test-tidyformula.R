df1 <- data.frame(
  y  = 1:5,
  x1 = rnorm(5),
  x2 = rnorm(5),
  x3 = rnorm(5)
)

test_that("starts_with works correctly", {
  
  expect_equal(
    tidyformula(y ~ starts_with("x") + z, df1),
    y ~ x1 + x2 + x3 + z
  )

  expect_equal(
    tidyformula( ~ starts_with("x") + z, df1),
     ~ x1 + x2 + x3 + z
  )
})

test_that("Distribution of functions works", {

  expect_equal(
    tidyformula(y ~ log(starts_with("x")) + z, df1),
    y ~ log(x1) + log(x2) + log(x3) + z
  )

  expect_equal(
    tidyformula( ~ poly(starts_with("x"), 3) + z, df1),
     ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + z
  )
})

test_that("Distribution of interactions works correctly", {

  expect_equal(
    tidyformula(y ~ starts_with("x"):z, df1),
    y ~ x1:z + x2:z + x3:z
  )

  # This test doesn't use expect_equal(), as there is a non-semantic difference
  # between the structures of the two produced formulas that we don't care
  # about and that `==` doesn't register
  expect_true(
    tidyformula( ~ starts_with("x") * z, df1) == (~ (x1 + x2 + x3)*z)
  )
  
  expect_equal(
    tidyformula( ~ starts_with("x")*z, df1, nodistribute = c("+", "-")),
     ~ x1*z + x2*z + x3*z
  )
})

test_that("tidyformula handles weird data frames", {

  df_onecol <- data.frame(a = 1:5)

  expect_equal(
    tidyformula( ~ everything(), df_onecol),
    ~ a
  )

  df_empty <- data.frame()
  
  expect_error(
    tidyformula( ~ everything(), df_empty)
  )

  expect_no_error(
    tidyformula( ~ x, df_empty)
  )
  
})
