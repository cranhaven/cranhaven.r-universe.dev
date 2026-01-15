context("Bessel function of Herz")

test_that("Relation with Bessel-J for x scalar", {
  skip_if_not_installed("Bessel")
  nu <- 3
  t <- 1 + 2i
  expected <- Bessel::BesselJ(t, nu)
  obtained <- BesselA(m = 15, t^2/4, nu) * (t/2)^nu
  expect_equal(obtained, expected)
  #
  t <- 5
  expected <- besselJ(t, nu)
  obtained <- BesselA(m = 15, t^2/4, nu) * (t/2)^nu
  expect_equal(obtained, expected)
})
