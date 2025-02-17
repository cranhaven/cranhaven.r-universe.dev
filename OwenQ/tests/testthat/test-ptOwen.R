context("ptOwen")

test_that("ptOwen = pt", {
  ncp <- 1
  expect_equal(ptOwen(2, nu=3, delta=ncp), pt(2, df=3, ncp=ncp), tolerance=1e-13)
  expect_equal(ptOwen(2, nu=4, delta=ncp), pt(2, df=4, ncp=ncp), tolerance=1e-12)
  expect_equal(ptOwen(2, nu=5, delta=ncp), pt(2, df=5, ncp=ncp), tolerance=1e-12)
  ncp <- 10
  expect_equal(ptOwen(2, nu=3, delta=ncp), pt(2, df=3, ncp=ncp), tolerance=1e-12)
  expect_equal(ptOwen(2, nu=4, delta=ncp), pt(2, df=4, ncp=ncp), tolerance=1e-13)
  expect_equal(ptOwen(2, nu=5, delta=ncp), pt(2, df=5, ncp=ncp), tolerance=1e-12)
  q <- -1; ncp <- 1
  expect_equal(ptOwen(q, nu=3, delta=ncp), pt(q, df=3, ncp=ncp), tolerance=1e-14)
  ncp <- -1
  expect_equal(ptOwen(q, nu=3, delta=ncp), pt(q, df=3, ncp=ncp), tolerance=1e-13)
})

test_that("ptOwen for infinite delta", {
  expect_true(ptOwen(2, nu=3, delta=100) == 0)
  expect_true(ptOwen(2, nu=3, delta=-100) == 1)
})

test_that("ptOwen = Q2 for R=0", {
  skip_on_os("mac")
  expect_equal(ptOwen(2, 5, 1.5), OwenQ2(5, 2, 1.5, 0), tolerance=1e-14)
})
