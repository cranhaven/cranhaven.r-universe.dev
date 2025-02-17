context("powen algo1")

algo <- 1

test_that("powen4", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ1(nu, t2, delta2, R, algo) - OwenQ1(nu, t1, delta1, R, algo)
  owen4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen4, tolerance=1e-16)
  wolfram <- 0.01785518085912236
  expect_equal(owen4, wolfram, tolerance=1e-11)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ1(nu, t2, delta2, R, algo) - OwenQ1(nu, t1, delta1, R, algo)
  owen4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen4, tolerance=1e-16)
  wolfram <- 0.01868982415809893
  expect_equal(owen4, wolfram, tolerance=1e-9)
})

test_that("Comparison with PASS", {
  skip_if(.Machine$sizeof.pointer < 8L) # skip if 32 bits
  powerTOST <- function(alpha, delta0, Delta, sigma, n1, n2){
    se <- sqrt(1/n1 + 1/n2)*sigma
    delta1 <- (delta0+Delta)/se
    delta2 <- (delta0-Delta)/se
    dof <- n1+n2-2
    q <- qt(1-alpha, dof)
    powen4(dof, q, -q, delta1, delta2, algo)
  }
  PASS <- structure(list(power = c(0.3909, 0.6954, 0.8558, 0.9343, 0.9709,
                                   0.3827, 0.6784, 0.8366, 0.9178, 0.9589, 0.3591, 0.6295, 0.7807,
                                   0.868, 0.9202, 0.3229, 0.5554, 0.6932, 0.7847, 0.8491, 0.2782,
                                   0.4653, 0.5831, 0.6717, 0.7426, 0.2297, 0.3697, 0.4621, 0.5388,
                                   0.606, 0.7037, 0.8576, 0.9232, 0.9545, 0.9709, 0.6865, 0.8385,
                                   0.906, 0.94, 0.9589, 0.637, 0.7826, 0.8544, 0.895, 0.9202, 0.5619,
                                   0.695, 0.7694, 0.8168, 0.8491, 0.4706, 0.5847, 0.6561, 0.7059,
                                   0.7426, 0.3736, 0.4634, 0.5247, 0.5705, 0.606),
                         n1 = c(10L, 15L,
                                20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L,
                                10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L,
                                25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L,
                                15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L,
                                30L, 10L, 15L, 20L, 25L, 30L),
                         n2 = c(10L, 15L, 20L, 25L, 30L,
                                10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L,
                                25L, 30L, 10L, 15L, 20L, 25L, 30L, 10L, 15L, 20L, 25L, 30L, 30L,
                                30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L,
                                30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L,
                                30L, 30L, 30L),
                         delta0 = c(0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1,
                                    0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.4,
                                    0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0.1, 0.1,
                                    0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3,
                                    0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5)),
                    .Names = c("power", "n1", "n2", "delta0"),
                    row.names = c(NA, 60L),
                    class = "data.frame")
  owen <- numeric(nrow(PASS))
  for(i in 1:nrow(PASS)){
    owen[i] <- powerTOST(alpha=0.05, delta0=PASS$delta0[i], Delta=1, sigma=1,
                         n1=PASS$n1[i], n2=PASS$n2[i])

  }
  expect_true(identical(round(owen,4), PASS$power))
})

test_that("powen4 - infinite nu", {
  t1 <- 2; t2 <- 1; delta1 <- 4; delta2 <- 2
  expect_equal(powen4(2000, t1, t2, delta1, delta2, algo),
               powen4(Inf, t1, t2, delta1, delta2),
               tolerance=1e-4)
})

test_that("powen4 - delta2=-Inf", {
  # the result does not depend on t2
  nu <- 2; t1 <- 3; delta1 <- 2 # rk: works only for nu=1 and nu=2
  x1 <- OwenQ:::RcppOwenCDF4(nu, t1, 1, delta1, -Inf, algo)
  x2 <- OwenQ:::RcppOwenCDF4(nu, t1, 2, delta1, -Inf, algo)
  expect_true(x1==x2)
  # so the result is 1-P1 (because T2=-Inf)
  expect_equal(x1, 1-ptOwen(t1, nu, delta1), tolerance=1e-15)
  # now this is implemented in powen4
  x <- powen4(nu, t1, 2, delta1, -Inf)
  expect_equal(x, x1, tolerance=1e-15)
})

test_that("powen4 - delta1=Inf", {
  # the result does not depend on t1
  nu <- 2; t2 <- 1; delta2 <- 2
  x1 <- powen4(nu, 3, t2, 100, delta2, algo)
  x2 <- powen4(nu, 13, t2, 100, delta2, algo)
  expect_true(x1==x2)
  # so the result is P2 (rather because T1=Inf)
  expect_equal(x1, ptOwen(t2, nu, delta2), tolerance=1e-16)
  # boost version: delta1 = Inf ne marche plus
  # BIZARRE
  expect_equal(OwenQ:::RcppOwenCDF4(1, 3, t2, 1000, delta2, algo),
               ptOwen(t2, 1, delta2),
               tolerance=1e-15)
  # now this is implemented in powen4
  x <- powen4(nu, 3, t2, Inf, delta2)
  expect_equal(x1, x, tolerance=1e-16)
})

test_that("powen4 - delta1=Inf and delta2=-Inf", {
  # the result is 1
  expect_true(powen4(2, 3, 1, 100, -100, algo) == 1)
  expect_true(powen4(Inf, 3, 1, 100, -100, algo) == 1)
  # => what if t1=Inf ? for instance t1=delta1 -> Inf
  # now this is implemented in powen4
  expect_true(powen4(2, 3, 1, Inf, -Inf) == 1)
})

test_that("powen4 - delta1=Inf and delta2=Inf", {
  # the result is 0
  expect_true(powen4(2, 3, 1, 101, 100, algo) == 0)
  expect_true(powen4(Inf, 3, 1, 101, 100) == 0)
  # now this is implemented in powen4
  expect_true(psbt4(2, 3, 1, Inf, Inf) == 0)
})

test_that("powen3", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- 1 - OwenQ1(nu, t2, delta2, R, algo) - OwenQ2(nu, t1, delta1, R, algo)
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen3, tolerance=1e-15)
  wolfram <- 0.809137482066635
  expect_equal(owen3, wolfram, tolerance=1e-9)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- 1 - OwenQ1(nu, t2, delta2, R, algo) - OwenQ2(nu, t1, delta1, R, algo)
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen3, tolerance=1e-15)
  wolfram <- 0.806507459306199
  expect_equal(owen3, wolfram, tolerance=1e-9)
})

test_that("powen3+powen4=1-P1", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  owen4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  P1 <- ptOwen(t1, nu, delta1)
  expect_equal(owen3+owen4, 1-P1, tolerance=1e-15)
  nu <- 5
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  owen4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  P1 <- ptOwen(t1, nu, delta1)
  #expect_true(owen3+owen4 == 1-P1)
  expect_equal(owen3+owen4, 1-P1, tolerance=1e-15)
})

test_that("powen3 - infinite nu", {
  t1 <- 2; t2 <- 1; delta1 <- 4; delta2 <- 2
  expect_equal(powen3(2000, t1, t2, delta1, delta2, algo),
               powen3(Inf, t1, t2, delta1, delta2),
               tolerance=1e-8)
})

test_that("powen3 - delta2=-Inf", {
  # the result is 0
  nu <- 2; t1 <- 3; delta1 <- 2 # rk: works only for nu=1 and nu=2
  x1 <- OwenQ:::RcppOwenCDF3(nu, t1, 1, delta1, -Inf, algo)
  expect_true(x1==0)
  # now this is implemented in powen3
  x <- powen3(nu, t1, 2, delta1, -Inf)
  expect_true(x==0)
})

test_that("powen3 - delta1=Inf", {
  # the result does not depend on t1
  nu <- 2; t2 <- 1; delta2 <- 2
  x1 <- powen3(nu, 3, t2, 100, delta2, algo)
  x2 <- powen3(nu, 13, t2, 100, delta2, algo)
  expect_true(x1==x2)
  # so the result is 1-P1-P2 = 1-P2
  expect_equal(x1, 1-ptOwen(99, nu, Inf)-ptOwen(t2, nu, delta2), tolerance=1e-15)
  #expect_true(OwenQ:::RcppOwenCDF3(1, 3, t2, Inf, delta2) == 1-ptOwen(t2, 1, delta2))
  # boost version: delta1=Inf gives NaN
  expect_true(OwenQ:::RcppOwenCDF3(1, 3, t2, 1000, delta2, algo) == 1-ptOwen(t2, 1, delta2))
  # now this is implemented in powen3
  x <- powen3(nu, 3, t2, Inf, delta2)
  expect_equal(x1, x, tolerance=1e-15)
})

test_that("powen3 - delta1=Inf and delta2=-Inf", {
  # the result is 0
  expect_true(powen3(2, 3, 1, 100, -100, algo) == 0)
  expect_true(powen3(Inf, 3, 1, 100, -100, algo) == 0)
  # now this is implemented in powen3
  expect_true(powen3(2, 3, 1, Inf, -Inf) == 0)
})

test_that("powen3 - delta1=Inf and delta2=Inf", {
  # the result is 1
  expect_true(powen3(2, 3, 1, 101, 100, algo) == 1)
  expect_true(powen3(Inf, 3, 1, 101, 100, algo) == 1)
  # now this is implemented in powen3
  expect_true(psbt3(2, 3, 1, Inf, Inf) == 1)
})

test_that("powen3 - delta1=-Inf and delta2=-Inf", {
  # the result is 0
  expect_true(powen3(2, 3, 1, -100, -101, algo) == 0)
  expect_true(powen3(Inf, 3, 1, -100, -101, algo) == 0)
  # now this is implemented in powen3
  expect_true(psbt3(2, 3, 1, -Inf, -Inf) == 0)
})


test_that("powen2", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ2(nu, t1, delta1, R, algo) - OwenQ2(nu, t2, delta2, R, algo)
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen2, tolerance=1e-16)
  wolfram <- 0.03257737810540227
  expect_equal(owen2, wolfram, tolerance=1e-10)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ2(nu, t1, delta1, R, algo) - OwenQ2(nu, t2, delta2, R, algo)
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen2, tolerance=1e-16)
  wolfram <- 0.0353568969628651
  expect_equal(owen2, wolfram, tolerance=1e-9)
})


test_that("powen2+powen3=1-P2", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  P2 <- ptOwen(t2, nu, delta2)
  expect_equal(owen2+owen3, 1-P2, tolerance=1e-15)
  nu <- 5
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  owen3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  P2 <- ptOwen(t2, nu, delta2)
  expect_equal(owen2+owen3, 1-P2, tolerance=1e-15)
})

test_that("powen2 - infinite nu", {
  t1 <- 2; t2 <- 1; delta1 <- 4; delta2 <- 2
  expect_true(powen2(Inf, t1, t2, delta1, delta2) == 0)
  t1 <- 4; t2 <- 1; delta1 <- 4; delta2 <- 2
  expect_equal(powen2(2000, t1, t2, delta1, delta2, algo),
               powen2(Inf, t1, t2, delta1, delta2),
               tolerance=1e-3)
})

test_that("powen2 - delta2=-Inf", {
  # the result is 0
  nu <- 2; t1 <- 3; delta1 <- 2 # rk: works only for nu=1 and nu=2
  x1 <- OwenQ:::RcppOwenCDF2(nu, t1, 1, delta1, -Inf, algo)
  expect_true(x1==0)
  # now this is implemented in powen2
  x <- powen2(nu, t1, 2, delta1, -Inf)
  expect_true(x==0)
})

test_that("powen2 - delta1=Inf", {
  # the result is 0
  nu <- 2; t2 <- 1; delta2 <- 2
  x1 <- powen2(nu, 3, t2, 100, delta2, algo)
  x2 <- powen2(nu, 13, t2, 100, delta2, algo)
  expect_true(x1==0)
  expect_equal(x2, 0, tolerance=1e-30)
  # now this is implemented in powen2
  x <- powen2(nu, 3, t2, Inf, delta2)
  expect_true(x==0)
})

test_that("powen2 - delta1=Inf and delta2=-Inf", {
  # the result is 0
  expect_true(powen2(2, 3, 1, 100, -100, algo) == 0)
  expect_true(powen2(Inf, 3, 1, 100, -100) == 0)
  # now this is implemented in powen2
  expect_true(powen2(2, 3, 1, Inf, -Inf) == 0)
})

test_that("powen2 - delta1=Inf and delta2=Inf", {
  # the result is 0
  expect_true(powen2(2, 3, 1, 101, 100, algo) == 0)
  expect_true(powen2(Inf, 3, 1, 101, 100) == 0)
  # now this is implemented in powen2
  expect_true(psbt2(2, 3, 1, Inf, Inf) == 0)
})

test_that("powen2 - delta1=-Inf and delta2=-Inf", {
  # the result is 0
  expect_true(powen2(2, 3, 1, -100, -101, algo) == 0)
  expect_true(powen2(Inf, 3, 1, -100, -101) == 0)
  # now this is implemented in powen2
  expect_true(psbt2(2, 3, 1, -Inf, -Inf) == 0)
})


test_that("powen1", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ1(nu, t1, delta1, R, algo) + OwenQ2(nu, t2, delta2, R, algo)
  owen1 <- powen1(nu, t1, t2, delta1, delta2)
  expect_equal(diff, owen1, tolerance=1e-16)
  wolfram <- 0.1404299569049368
  expect_equal(owen1, wolfram, tolerance=1e-8)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  diff <- OwenQ1(nu, t1, delta1, R, algo) + OwenQ2(nu, t2, delta2, R, algo)
  owen1 <- powen1(nu, t1, t2, delta1, delta2, algo)
  expect_equal(diff, owen1, tolerance=1e-16)
  wolfram <- 0.1394458271284726
  expect_equal(owen1, wolfram, tolerance=1e-8)
})


test_that("powen1+powen2=P1", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  owen1 <- powen1(nu, t1, t2, delta1, delta2, algo)
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  P1 <- ptOwen(t1, nu, delta1)
  expect_equal(owen1+owen2, P1, tolerance=1e-15)
  nu <- 5
  owen1 <- powen1(nu, t1, t2, delta1, delta2, algo)
  owen2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  P1 <- ptOwen(t1, nu, delta1)
  expect_equal(owen1+owen2, P1, tolerance=1e-15)
})

test_that("powen1 - infinite nu", {
  t1 <- 4; t2 <- 1; delta1 <- 4; delta2 <- 2
  expect_equal(powen1(2000, t1, t2, delta1, delta2, algo),
               powen1(Inf, t1, t2, delta1, delta2),
               tolerance=1e-8)
})

test_that("powen1 - delta2=-Inf", {
  # the result is P1
  nu <- 2; t1 <- 3; delta1 <- 2 # rk: works only for nu=1 and nu=2
  x1 <- OwenQ:::RcppOwenCDF1(nu, t1, 1, delta1, -Inf, algo)
  x2 <- ptOwen(t1, nu, delta1)
  expect_equal(x1, x2, tolerance=1e-15)
  # now this is implemented in powen1
  x <- powen1(nu, t1, 2, delta1, -Inf)
  expect_true(x==x2)
})

test_that("powen1 - delta1=Inf", {
  # the result is 0
  nu <- 2; t2 <- 1; delta2 <- 2
  x1 <- powen1(nu, 3, t2, 100, delta2, algo)
  x2 <- powen1(nu, 13, t2, 100, delta2, algo)
  expect_true(x1==0)
  expect_equal(x2, 0, tolerance=1e-25)
  # now this is implemented in powen1
  x <- powen1(nu, 3, t2, Inf, delta2)
  expect_true(x==0)
})

test_that("powen1 - delta1=Inf and delta2=-Inf", {
  # the result is 0
  expect_true(powen1(2, 3, 1, 100, -100, algo) == 0)
  expect_true(powen1(Inf, 3, 1, 100, -100) == 0)
  # now this is implemented in powen1
  expect_true(powen1(2, 3, 1, Inf, -Inf) == 0)
})

test_that("powen1 - delta1=Inf and delta2=Inf", {
  # the result is 0
  expect_true(powen1(2, 3, 1, 101, 100, algo) == 0)
  expect_true(powen1(Inf, 3, 1, 101, 100) == 0)
  # now this is implemented in powen1
  expect_true(psbt1(2, 3, 1, Inf, Inf) == 0)
})

test_that("powen1 - delta1=-Inf and delta2=-Inf", {
  # the result is 1
  expect_true(powen1(2, 3, 1, -100, -101, algo) == 1)
  expect_true(powen1(Inf, 3, 1, -100, -101) == 1)
  # now this is implemented in powen1
  expect_true(psbt1(2, 3, 1, -Inf, -Inf) == 1)
})

test_that("The powen's sum to 1", {
  nu=5; t1=2; t2=1; delta1=3; delta2=2
  p1 <- powen1(nu, t1, t2, delta1, delta2, algo)
  p2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  p3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  p4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  expect_true(p1+p2+p3+p4 == 1)
  # case t2>t1
  t2=3
  p1 <- powen1(nu, t1, t2, delta1, delta2, algo)
  p2 <- powen2(nu, t1, t2, delta1, delta2, algo)
  p3 <- powen3(nu, t1, t2, delta1, delta2, algo)
  p4 <- powen4(nu, t1, t2, delta1, delta2, algo)
  expect_true(p1+p2+p3+p4 == 1)
})
