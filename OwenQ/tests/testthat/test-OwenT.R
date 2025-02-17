context("OwenT")

test_that("Owen T - reflection properties", {
  expect_true(OwenT(2,1) == OwenT(-2,1))
  expect_true(OwenT(2,1) == -OwenT(2,-1))
  expect_true(OwenT(2,0.1) == OwenT(-2,0.1))
  expect_true(OwenT(2,0.1) == -OwenT(2,-0.1))
  expect_true(OwenT(2,11) == OwenT(-2,11))
  expect_true(OwenT(2,11) == -OwenT(2,-11))
})

test_that("OwenT(0,a)", {
  a <- 1
  expect_equal(OwenT(0,a), atan(a)/(2*pi), tolerance=1e-18)
  a <- 0.9
  expect_equal(OwenT(0,a), atan(a)/(2*pi), tolerance=1e-16)
  a <- 2
  expect_equal(OwenT(0,a), atan(a)/(2*pi), tolerance=1e-15)
})

test_that("OwenT(h,1)", {
  h <- 2
  expect_equal(OwenT(h,1), pnorm(h)*(1-pnorm(h))/2, tolerance=1e-16)
  h <- 100 # >cut.point
  expect_true(OwenT(h,1) == pnorm(h)*(1-pnorm(h))/2) # this is 0
})

test_that("OwenT(h,Inf)", {
  h <- 1
  expect_equal(OwenT(h,Inf), (1-pnorm(abs(h)))/2, tolerance=1e-16)
  h <- 0.5
  expect_equal(OwenT(h,Inf), (1-pnorm(abs(h)))/2, tolerance=1e-16)
  h <- 10
  expect_equal(OwenT(h,Inf), (1-pnorm(abs(h)))/2, tolerance=1e-16)
  h <- 100
  expect_equal(OwenT(h,Inf), (1-pnorm(abs(h)))/2, tolerance=1e-16)
})

test_that("OwenT(Inf,a) = 0", {
  a <- 30
  expect_true(OwenT(Inf,a) == 0) # NaN
})

test_that("Relation OwenT Cauchy", {
  h <- 2; a <- 2
  expect_equal(OwenT(h, a), 1/2*(pt(a, 1, h*sqrt(1+a^2)) - pnorm(-h)),
               tolerance=1e-12)
})


test_that("Comparison Wolfram", {
  expect_equal(OwenT(1,2), 0.078468186993084, tolerance=1e-16)
  expect_equal(OwenT(1,0.5), 0.0430646911207853656324, tolerance=1e-15)
  expect_equal(OwenT(2,0.9), 0.0109285988291624569525, tolerance=1e-15)
  # h=1/2
  wolfram <- c(0.013993020236280154243,0.027679288269477873649,0.040786707344250107440,0.053103310576947631095,
               0.064488602847503757028,0.074871390139948867182,0.084238500228436371909,
               0.092619672935840401367,0.10007270175061385845)
  owenT <- sapply(1:9, function(i) OwenT(1/2, i/10))
  expect_equal(owenT, wolfram, tolerance=1e-16)
  # h=0.1
  wolfram <- c(0.015783380517183599184,0.031257726663751529270,0.046148670805548151646,0.060241997652666154605,
               0.073394997126852609976,0.085534304368681660207,0.096644630685100142608,0.10675366357368805933,
               0.11591721752536029998)
  owenT <- sapply(1:9, function(i) OwenT(1/10, i/10))
  expect_equal(owenT, wolfram, tolerance=1e-16)
  # h=0.9
  wolfram <- c(0.010565864256575396551,0.020842633054101222357,0.030575567646615198620,0.039569470725646486297,
               0.047699756280050998141,0.054910032422892097125,0.061200607819032770796,
               0.066613205295307675358,0.071215968202819446493)
  owenT <- sapply(1:9, function(i) OwenT(9/10, i/10))
  expect_equal(owenT, wolfram, tolerance=1e-16)
  #
  wolfram <- c(0.10667106296144851629,0.14158060365397839347,0.15108404307601841107,0.15355639134293798378,
               0.15413219366878640005,0.15424687184357583192,0.15426588057198986118,0.15426845932112990841,
               0.15426874251307358385,0.15426876749825401454)
  owenT <- sapply(1:10, function(i) OwenT(1/2, i))
  expect_equal(owenT, wolfram, tolerance=1e-16)
})

test_that("Relation T(h,a) T(ah,1/a)", {
  h <- 0.5; a <- 2
  expect_equal(OwenT(h,a)+OwenT(a*h,1/a),
                (pnorm(h)+pnorm(a*h))/2-pnorm(h)*pnorm(a*h),
               tolerance=1e-15)
  a <- -2
  expect_equal(OwenT(h,a)+OwenT(a*h,1/a),
               (pnorm(h)+pnorm(a*h))/2-pnorm(h)*pnorm(a*h)-0.5,
               tolerance=1e-15)
  h <- 100; a <- 2 # this gives 0
  expect_true(OwenT(h,a)+OwenT(a*h,1/a) ==
                (pnorm(h)+pnorm(a*h))/2-pnorm(h)*pnorm(a*h))
})
