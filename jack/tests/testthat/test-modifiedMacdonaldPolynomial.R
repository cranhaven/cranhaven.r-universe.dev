test_that("Modified Macdonald polynomial mu (q, t) = Modified Macdonald polynomial mu' (t, q)", {
  n <- 4
  mu <- c(2, 1, 1)
  mup <- c(3, 1)
  macHpoly <- modifiedMacdonaldPol(n, mu)
  macHpolyp <- changeParameters(
    modifiedMacdonaldPol(n, mup),
    list(qlone(2), qlone(1))
  )
  expect_true(macHpoly == macHpolyp)
})
