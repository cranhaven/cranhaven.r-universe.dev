test_that("Littlewood-Richardson multiplication", {
  mu <- c(2, 1)
  nu <- c(3, 2, 1)
  LR <- LRmult(mu, nu, output = "list")
  LRterms <- lapply(LR, function(lr) {
    lr[["coeff"]] * SchurPol(3, lr[["lambda"]])
  })
  smu_times_snu <- Reduce(`+`, LRterms)
  expect_true(smu_times_snu == SchurPol(3, mu) * SchurPol(3, nu))
})

test_that("LR-rule and Standard Young Tableaux counting", {
  # https://math.stackexchange.com/q/3012744/38217
  skip_if_not_installed("syt")
  mu <- c(3, 2, 2, 1)
  nu <- c(4, 3, 2, 1)
  LR <- LRmult(mu, nu, output = "list")
  h <- function(p) as.integer(syt::count_sytx(p))
  counts <- vapply(LR, function(lr) {
    h(lr[["lambda"]])
  }, integer(1L))
  coeffs <- vapply(LR, function(lr) {
    lr[["coeff"]]
  }, integer(1L))
  rhs <- sum(coeffs * counts)
  lhs <- h(mu) * h(nu) * choose(sum(mu) + sum(nu), sum(mu))
  expect_true(lhs == rhs)
})

test_that("Skew Schur polynomial", {
  sspol <- SkewSchurPol(3, lambda = c(3, 2, 1), mu = c(1, 1))
  expected <- SchurPol(3, c(2, 1, 1)) + SchurPol(3, c(2, 2)) +
    SchurPol(3, c(3, 1))
  expect_true(sspol == expected)
})


test_that("Skew Schur polynomial and skew semistandard tableaux", {
  skip_if_not_installed("syt", minimum_version = "0.4.0")
  lambda <- c(4, 3, 2, 1)
  mu <- c(2, 1)
  n <- 4
  sssktx <- syt::all_ssSkewTableaux(lambda, mu, n)
  wt <- function(ssyt) {
    ssyt <- unlist(ssyt)
    vapply(1:n, function(k) {
      length(which(ssyt == k))
    }, integer(1L))
  }
  qlones <- lapply(1:n, qspray::qlone)
  monomial <- function(ssyt) {
    powers <- wt(ssyt)
    Reduce(`*`, lapply(1:n, function(k) qlones[[k]]^powers[k]))
  }
  monomials <- lapply(sssktx, monomial)
  obtained <- Reduce(`+`, monomials)
  expected <- SkewSchurPol(n, lambda, mu)
  expect_true(obtained == expected)
})
