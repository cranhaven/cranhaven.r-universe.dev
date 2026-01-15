test_that("Kostka numbers for alpha=1", {
  expect_identical(KostkaJackNumbers(4), KostkaJackNumbers(4, alpha = 1))
})

test_that("Kostka-Jack numbers for alpha=0", {
  n <- 6
  cparts <- vapply(listOfPartitions(n), function(p) {
    partitionAsString(partitions::conjugate(p))
  }, character(1L))
  kn1 <- KostkaJackNumbers(n, "2")
  kn2 <- KostkaJackNumbers(n, "1/2")
  kn2 <- kn2[cparts, cparts]
  obtained <- t(gmp::`%*%`(gmp::as.bigq(t(kn1)), gmp::as.bigq(kn2)))
  expected <- KostkaJackNumbers(n, "0")
  expect_true(all(as.character(obtained) == expected))
})

test_that("Kostka numbers are the coefficients of Jack P-polynomials", {
  n <- 4L
  alpha <- "3/2"
  Knumbers <- KostkaJackNumbers(n, alpha = alpha)
  lambda <- c(3L, 1L)
  jp <- JackPol(n, lambda, alpha, which = "P")
  lambda <- partitionAsString(lambda)  #paste0("(", toString(lambda), ")")
  qspray <- qzero()
  for(mu in colnames(Knumbers)) {
    coeff <- Knumbers[lambda, mu]
    mu <- fromString(gsub("(\\[|\\])", "", mu)) #fromString(gsub("(\\(|\\))", "", mu))
    qspray <- qspray + coeff * MSFpoly(n, mu)
  }
  expect_true(qspray == jp)
})

test_that("Symbolic Kostka numbers are the coefficients of symbolic Jack P-polynomials", {
  n <- 4L
  alpha <- "3/2"
  Knumbers <- symbolicKostkaJackNumbers(n)
  lambda <- c(3L, 1L)
  jp <- JackSymPol(n, lambda, which = "P")
  lambda <- paste0("[", toString(lambda), "]")
  Knumbers <- Knumbers[[lambda]]
  Qspray <- Qzero()
  for(mu in names(Knumbers)) {
    coeff <- Knumbers[[mu]]
    mu <- fromPartitionAsString(mu)
    Qspray <- Qspray + coeff * as(MSFpoly(n, mu), "symbolicQspray")
  }
  expect_true(Qspray == jp)
})

test_that("Skew Kostka-Jack numbers for alpha=1 are ordinary skew Kostka numbers", {
  lambda <- c(3, 2, 1)
  mu <- c(2, 1)
  sKJnumbers <- skewKostkaJackNumbers(lambda, mu, alpha = "1", output = "vector")
  sKnumbers <- syt::skewKostkaNumbers(lambda, mu, output = "vector")
  nus <- names(sKnumbers)
  expect_true(length(setdiff(nus, names(sKJnumbers))) == 0L)
  expect_true(all(sKJnumbers[nus] == sKnumbers))
})

test_that("Symbolic skew Kostka-Jack numbers", {
  lambda <- c(3, 2, 1)
  mu <- c(2, 1)
  symsKJnumbers <- symbolicSkewKostkaJackNumbers(lambda, mu)
  alpha <- "2"
  sKJnumbers <- skewKostkaJackNumbers(lambda, mu, alpha, output = "vector")
  nus <- names(sKJnumbers)
  expect_true(length(setdiff(nus, names(symsKJnumbers))) == 0L)
  evals <- vapply(symsKJnumbers, function(lst) {
    as.character(evalRatioOfQsprays(lst[["value"]], alpha))
  }, character(1L), USE.NAMES = TRUE)
  expect_true(all(evals[nus] == sKJnumbers))
})
