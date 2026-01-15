test_that(
  "ZonalQ with lambda = (4)", {
    # gmp
    obtained <- ZonalQPolR(4, c(4), algo="naive", basis="MSF")
    expected <-
      "M_(4) + 8/5 M_(3,1) + 9/5 M_(2,2) + 12/5 M_(2,1,1) + 16/5 M_(1,1,1,1)"
    expect_identical(obtained, expected)
    # numeric
    obtained <- ZonalQPolR(4, c(4), algo="naive", basis="MSF", exact = FALSE)
    expected <-
      "M_(4) + 1.6 M_(3,1) + 1.8 M_(2,2) + 2.4 M_(2,1,1) + 3.2 M_(1,1,1,1)"
    expect_identical(obtained, expected)
  }
)

test_that(
  "An example of the quaternionic zonal polynomial", {
    poly <- ZonalQPolR(3, c(2,1), algo = "naive", basis = "MSF")
    expect_identical(poly, "3/2 M_(2,1) + 18/5 M_(1,1,1)")
  }
)

test_that(
  "Quaternionic zonal polynomials sum to the trace - gmp", {
    x <- as.bigq(c(1L,2L,4L,7L), c(2L,3L,1L,2L))
    expected <- sum(x)^3
    obtained_DK <- ZonalQR(x, 3) + ZonalQR(x, c(2,1)) + ZonalQR(x, c(1,1,1))
    obtained_naive <- ZonalQR(x, 3, algorithm = "naive") +
      ZonalQR(x, c(2,1), algorithm = "naive") +
      ZonalQR(x, c(1,1,1), algorithm = "naive")
    expect_identical(obtained_DK, expected)
    expect_identical(obtained_naive, expected)
  }
)

test_that(
  "Quaternionic zonal polynomials sum to the trace - numeric", {
    x <- c(1,2,4,7) / c(2,3,1,2)
    expected <- sum(x)^3
    obtained_DK <- ZonalQR(x, 3) + ZonalQR(x, c(2,1)) + ZonalQR(x, c(1,1,1))
    obtained_naive <- ZonalQR(x, 3, algorithm = "naive") +
      ZonalQR(x, c(2,1), algorithm = "naive") +
      ZonalQR(x, c(1,1,1), algorithm = "naive")
    expect_equal(obtained_DK, expected)
    expect_equal(obtained_naive, expected)
  }
)

test_that(
  "ZonalQPol is correct", {
    lambda <- c(3, 2)
    pol <- ZonalQPolR(4, lambda, algorithm = "naive")
    x <- as.bigq(c(6L,-7L,8L,9L), c(1L,2L,3L,4L))
    polEval <- qspray::evalQspray(pol, x)
    expect_identical(polEval, ZonalQR(as.bigq(x), lambda))
  }
)

test_that(
  "ZonalQ polynomials sum to the trace - polynomial", {
    n <- 4
    expected <- (qlone(1) + qlone(2) + qlone(3) + qlone(4))^3
    obtained <- ZonalQPolR(n, 3) + ZonalQPolR(n, c(2,1)) + ZonalQPolR(n, c(1,1,1))
    expect_true(expected == obtained)
  }
)

test_that(
  "ZonalQCPP is correct", {
    x <- as.bigq(c(6L, -7L, 8L, 9L), c(1L, 2L, 3L, 4L))
    lambda <- c(3, 2)
    res <- ZonalQ(x, lambda)
    expect_identical(res, ZonalQR(x, lambda))
    #
    x <- c(6, -7, 8, 9) / c(1, 2, 3, 4)
    lambda <- c(3, 2)
    res <- ZonalQ(x, lambda)
    expect_equal(res, ZonalQR(x, lambda))
  }
)
