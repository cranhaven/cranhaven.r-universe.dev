test_that("Skew Kostka-Foulkes polynomial", {
  lambda <- c(3, 2, 1, 1)
  mu <- c(2, 2)
  n <- sum(lambda) - sum(mu)
  nus <- partitions::parts(n)
  symbQSpray <- Reduce(
    `+`,
    apply(nus, 2L, function(nu) {
      SkewKostkaFoulkesPolynomial(lambda, mu, nu) *
        HallLittlewoodPol(n, nu, "P")
    }, simplify = FALSE)
  )
  symbSkewSchurPoly <- as(SkewSchurPol(n, lambda, mu), "symbolicQspray")
  expect_true(symbQSpray == symbSkewSchurPoly)
})
