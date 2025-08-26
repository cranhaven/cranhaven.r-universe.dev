context("test-estimate.sample.R")

test_that("mle for uniform sample", {
  x = c(0.3103205, -0.1386720, -0.5988813,  0.9483934, -0.5213117,
        0.7555062, -0.6821612,  0.7957394, -0.7387161,  0.1910647)
  par = estimate.uniform(x)

  expect_equal(length(par),2)
  expect_equal(par,c(-0.7387161,  0.9483934))
})

test_that("mle for normal sample", {
  x = c(0.25024690, -0.33712454, -0.11335370, -0.09888291, 0.26408682,
        0.13898369, -0.24226950, 0.05903138, -0.17727187, 0.79468027)
  par = estimate.normal(x)

  expect_equal(length(par),2)
  expect_equal(par,c(0.05381265, 0.33067690))
})

test_that("mle for gamma sample", {
  x = c(0.5047757, 0.1538300, 0.5704100, 0.3013008, 1.2775724,
        1.0468233, 0.6525627, 0.4376768, 2.4700737, 1.0944885)
  par.scale = estimate.gamma(x)
  par.rate = estimate.gamma(x,use.rate = TRUE)

  expect_equal(length(par.scale),2)
  expect_equal(length(par.rate),2)
  expect_equal(par.scale[2],1/par.rate[2])
  expect_equal(par.scale,c(2.0231691, 0.4206032))
  expect_equal(par.rate,c(2.0231691, 2.3775378))
})

test_that("mle for logistic sample", {
  x = c(-1.1263960,  0.9562103, -3.3860294,  0.1980448,  0.7667096,
        -0.8461510, -0.4524666,  1.0070690,  3.2450939,  1.1559508)
  par = estimate.logistic(x,verbose = FALSE)

  expect_equal(length(par),2)
  expect_equal(par,c(0.22044945, 0.91277022))
})

test_that("mle for laplace sample", {
  x = c(-0.23539279,  0.16009027,  2.84634962,  0.35710312, -0.40466195,
         -0.41113889,  2.16169132, -0.27151351,  0.13770907,  0.02330074)
  par.scale = estimate.laplace(x)
  par.sd = estimate.laplace(x,use.sd=TRUE)

  expect_equal(length(par.scale),2)
  expect_equal(par.sd[2],par.scale[2]*sqrt(2))
  expect_equal(par.scale,c(0.080504905, 0.696234980))
  expect_equal(par.sd,c(0.080504905, 0.984624951))
})

test_that("mle for weibull sample", {
  x = c(0.36218715, 0.16506700, 0.16757965, 0.93681048, 1.87396510,
        0.44718470, 1.24767735, 0.07435952, 1.86023456, 0.03682825)
  par = estimate.weibull(x)

  expect_equal(length(par),2)
  expect_equal(par,c(0.92994712, 0.69388615))
})

test_that("mle for exponential sample", {
  x = c(13.0581121,  0.8301048,  0.5207504,  1.0923122,  0.7086793,
        0.1271974,  3.9326089,  0.0510448,  4.3839846,  3.4396530)
  par.rate = estimate.exp(x,use.rate=TRUE)
  par.scale = estimate.exp(x,use.rate=FALSE)

  expect_equal(length(par.rate),1)
  expect_equal(par.rate,1/par.scale)
  expect_equal(par.rate,0.35530987)
  expect_equal(par.scale,2.81444475)
})

