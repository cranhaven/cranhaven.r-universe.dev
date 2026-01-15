test_that(
  "Skew Jack polynomial branching rule", {
    nx <- 2
    ny <- 2
    n <- nx + ny
    lambda <- c(2, 2)
    alpha <- 2L
    which <- "P"
    mus <- list(integer(0), 1, 2, c(1, 1), c(2, 1), c(2, 2))
    ys <- list(qlone(3), qlone(4))
    jp <- JackPol(n, lambda, alpha, which)
    expected <-
      Reduce(
        `+`,
        lapply(mus, function(mu) {
          JackPol(nx, mu, alpha, which) *
            changeVariables(SkewJackPol(ny, lambda, mu, alpha, which), ys)
        })
      )
    expect_true(jp == expected)
  }
)

test_that(
  "Skew symbolic Jack polynomial branching rule", {
    nx <- 2
    ny <- 2
    n <- nx + ny
    lambda <- c(2, 2)
    which <- "Q"
    mus <- list(integer(0), 1, 2, c(1, 1), c(2, 1), c(2, 2))
    ys <- list(Qlone(3), Qlone(4))
    jp <- JackSymPol(n, lambda, which)
    expected <-
      Reduce(
        `+`,
        lapply(mus, function(mu) {
          JackSymPol(nx, mu, which) *
            changeVariables(SkewJackSymPol(ny, lambda, mu, which), ys)
        })
      )
    expect_true(jp == expected)
  }
)

test_that("Jack combination of skew Jack with Hall inner product", {
  lambda <- c(3, 1, 1)
  mu <- c(2, 1)
  alpha <- "2"
  n <- sum(lambda)
  skewJackPoly <- SkewJackPol(n, lambda, mu, alpha, "Q")
  jackCombo <- JackCombination(skewJackPoly, alpha, "Q")
  nus <- lapply(jackCombo, `[[`, "lambda")
  coeffs <- gmp::c_bigq(lapply(jackCombo, `[[`, "coeff"))
  jackQpoly <- JackPol(n, lambda, alpha, "Q")
  jackPpoly <- JackPol(n, mu, alpha, "P")
  fs <- gmp::c_bigq(lapply(nus, function(nu) {
    HallInnerProduct(jackQpoly, jackPpoly * JackPol(n, nu, alpha, "P"), alpha)
  }))
  expect_true(all(coeffs == fs))
})

test_that("Skew Jack with alpha=0 is same as skew Macdonald with q=1", {
  # I don't know why. In particular, skew Macdonald with q=1 does not depend on t
  n <- 4
  lambda <- c(4, 2)
  mu <- c(1, 1)
  skJackPoly <-
    changeParameters(SkewJackSymPol(n, lambda, mu, "P"), list(qzero()))
  skMacPoly <- changeParameters(
    SkewMacdonaldPol(n, lambda, mu, which = "P"),
    list(qone(), qlone(2))
  )
  expect_true(skJackPoly == skMacPoly)
})
