test_that(
  # https://math.stackexchange.com/questions/3335885/expansion-of-sum-x-in-in-schur-polynomials
  "Schur expansion of (sum x_i)^n", {
    # numeric
    x <- c(3,4,5,6)
    e <- SchurR(x, c(4)) + 3*SchurR(x, c(3,1)) + 2*SchurR(x, c(2,2)) +
      3*SchurR(x, c(2,1,1)) + SchurR(x, c(1,1,1,1))
    expect_equal(e, sum(x)^4)
    # gmp
    x <- as.bigq(c(3L,4L,5L,6L), c(4L,5L,6L,7L))
    e <- SchurR(x, c(4)) + 3L*SchurR(x, c(3,1)) + 2L*SchurR(x, c(2,2)) +
      3L*SchurR(x, c(2,1,1)) + SchurR(x, c(1,1,1,1))
    expect_identical(e, sum(x)^4)
    # polynomial
    n <- 4
    P <- SchurPolR(n, c(4)) + 3*SchurPolR(n, c(3, 1)) + 2*SchurPolR(n, c(2, 2)) +
      3*SchurPolR(n, c(2, 1, 1)) + SchurPolR(n, c(1, 1, 1, 1))
    Q <- (mvp("x_1", 1, 1) + mvp("x_2", 1, 1) + mvp("x_3", 1, 1) +
            mvp("x_4", 1, 1))^4
    expect_true(as_mvp_qspray(P) == Q)
  }
)

test_that(
  "Schur = 0 if l(lambda)>l(x)", {
    # numeric
    expect_equal(SchurR(c(1,2), c(3,2,1)), 0)
    expect_equal(SchurR(c(1,2), c(3,2,1), algorithm = "naive"), 0)
    # gmp
    x <- as.bigq(c(1L,2L))
    lambda <- c(3,2,1)
    expect_identical(SchurR(x, lambda), as.bigq(0L))
    expect_identical(SchurR(x, lambda, algorithm = "naive"), as.bigq(0L))
    # polynomial
    n <- 2
    lambda <- c(3,2,1)
    expect_true(SchurPolR(n, lambda) == as.qspray(0))
    expect_identical(SchurPolR(n, lambda, algorithm = "naive"),
                     as.qspray(0))
    expect_identical(SchurPolR(n, lambda, exact = FALSE, algorithm = "naive"),
                     mvp::constant(0))
    expect_identical(SchurPolR(n, lambda, algorithm = "naive",
                             basis = "MSF"),
                     as.qspray(0))
    expect_identical(SchurPolR(n, lambda, exact = FALSE, algorithm = "naive",
                             basis = "MSF"),
                     mvp::constant(0))
  }
)


test_that(
  "Schur (3,2) - gmp", {
    x <- as.bigq(3L:5L, c(10L,2L,1L))
    expected <- x[1]^3*x[2]^2 + x[1]^3*x[3]^2 + x[1]^3*x[2]*x[3] +
      x[1]^2*x[2]^3 + x[1]^2*x[3]^3 + 2*x[1]^2*x[2]*x[3]^2 +
      2*x[1]^2*x[2]^2*x[3] + x[1]*x[2]*x[3]^3 + 2*x[1]*x[2]^2*x[3]^2 +
      x[1]*x[2]^3*x[3] + x[2]^2*x[3]^3 + x[2]^3*x[3]^2
    naive <- SchurR(x, c(3,2), algorithm = "naive")
    DK <- SchurR(x, c(3,2), algorithm = "DK")
    expect_identical(naive, expected)
    expect_identical(DK, expected)
  }
)

test_that(
  "Schur (3,2) - numeric", {
    x <- c(3L:5L) / c(10L,2L,1L)
    expected <- x[1]^3*x[2]^2 + x[1]^3*x[3]^2 + x[1]^3*x[2]*x[3] +
      x[1]^2*x[2]^3 + x[1]^2*x[3]^3 + 2*x[1]^2*x[2]*x[3]^2 +
      2*x[1]^2*x[2]^2*x[3] + x[1]*x[2]*x[3]^3 + 2*x[1]*x[2]^2*x[3]^2 +
      x[1]*x[2]^3*x[3] + x[2]^2*x[3]^3 + x[2]^3*x[3]^2
    naive <- SchurR(x, c(3,2), algorithm = "naive")
    DK <- SchurR(x, c(3,2), algorithm = "DK")
    expect_equal(naive, expected)
    expect_equal(DK, expected)
  }
)

test_that(
  "SchurPol is correct", {
    lambda <- c(3,2)
    pol <- SchurPolR(4, lambda, algorithm = "naive")
    x <- as.bigq(c(6L,-7L,8L,9L), c(1L,2L,3L,4L))
    polEval <- qspray::evalQspray(pol, x)
    expect_identical(polEval, SchurR(as.bigq(x), lambda))
  }
)

test_that(
  "Pieri rule", {
    n <- 3
    P1 <- SchurPolR(n, c(3, 2)) + 2 * SchurPolR(n, c(2, 2, 1)) +
      SchurPolR(n, c(3, 1, 1)) + 2 * SchurPolR(n, c(2, 1, 1, 1)) +
      SchurPolR(n, c(1, 1, 1, 1, 1))
    P2 <- qspray::ESFpoly(n, c(2, 2, 1))
    expect_true(P1 == P2)
  }
)

test_that(
  "SchurPolCPP is correct", {
    lambda <- c(3, 2)
    pol <- SchurPol(4, lambda)
    x <- as.bigq(c(6L,-7L,8L,9L), c(1L,2L,3L,4L))
    polEval <- qspray::evalQspray(pol, x)
    expect_identical(polEval, SchurR(as.bigq(x), lambda))
  }
)

test_that(
  "SchurCPP is correct", {
    x <- as.bigq(c(6L, -7L, 8L, 9L), c(1L, 2L, 3L, 4L))
    lambda <- c(3, 2)
    res <- Schur(x, lambda)
    expect_identical(res, SchurR(x, lambda))
    #
    x <- c(6, -7, 8, 9) / c(1, 2, 3, 4)
    lambda <- c(3, 2)
    res <- Schur(x, lambda)
    expect_equal(res, SchurR(x, lambda))
  }
)

test_that("Schur polynomial and semistandard Young tableaux", {
  skip_if_not_installed("syt")
  lambda <- c(3, 1)
  ssytx <- syt::all_ssytx(lambda, 4)
  wt <- function(ssyt) {
    ssyt <- unlist(ssyt)
    vapply(1:4, function(k) {
      length(which(ssyt == k))
    }, integer(1L))
  }
  qlones <- lapply(1:4, qspray::qlone)
  monomial <- function(ssyt) {
    powers <- wt(ssyt)
    Reduce(`*`, lapply(1:4, function(k) qlones[[k]]^powers[k]))
  }
  monomials <- lapply(ssytx, monomial)
  obtained <- Reduce(`+`, monomials)
  expected <- SchurPolR(4, lambda)
  expect_true(obtained == expected)
})
