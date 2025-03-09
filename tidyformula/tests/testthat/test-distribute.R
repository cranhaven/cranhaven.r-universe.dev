test_that("Basic distribution works", {

  expect_equal(
    distribute(quote(x + y + z), quote(log)),
    quote(log(x) + log(y) + log(z))
  )

})

test_that("Function is distributed into pairlists", {

  expect_equal(
    distribute(quote(f(x) + f(x, y) + 1), quote(sqrt)),
    quote(f(sqrt(x)) + f(sqrt(x), sqrt(y)) + 1)
  )

})
  
test_that("Functions are not distributed across literals", {

  expect_equal(
    distribute(quote(x + y^2 + 3), quote(h)),
    quote(h(x) + h(y)^2 + 3)
  )

})

test_that("Multivariate distribution works", {

  expect_equal(
    distribute(quote(x + y + z), quote(`^`), 2),
    quote(x^2 + y^2 + z^2)
  )

  expect_equal(
    distribute(quote(x + y^2), quote(h), list(0, quote(arg))),
    quote(h(x, 0, arg) + h(y, 0, arg)^2)
  )
  
})

test_that("Interactions distribute", {

  expect_equal(
    distribute(quote(x + y), quote(`*`), list(quote(z))),
    quote(x*z + y*z)
  )

  expect_equal(
    distribute(quote(x + y), quote(`:`), list(quote(z))),
    quote(x:z + y:z)
  )

})
