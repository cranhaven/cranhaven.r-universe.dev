context("test-AD.pvalue.R")

test_that("AD P-value for uniform sample", {
  x = c(0.3103205, -0.1386720, -0.5988813,  0.9483934, -0.5213117,
        0.7555062, -0.6821612,  0.7957394, -0.7387161,  0.1910647)
  asq = AD.uniform(x)
  p_value = AD.uniform.pvalue(a=asq)$P

  expect_equal(p_value,0.68906061)
})

test_that("AD P-value for normal sample", {
  x = c(0.25024690, -0.33712454, -0.11335370, -0.09888291, 0.26408682,
        0.13898369, -0.24226950, 0.05903138, -0.17727187, 0.79468027)
  asq = AD.normal(x)
  p_value = AD.normal.pvalue(a=asq)$P

  expect_equal(p_value,0.39529586)
})

test_that("AD for gamma sample", {
  x = c(0.5047757, 0.1538300, 0.5704100, 0.3013008, 1.2775724,
        1.0468233, 0.6525627, 0.4376768, 2.4700737, 1.0944885)
  asq = AD.gamma(x)
  shape = estimate.gamma(x)[2]
  p_value = AD.gamma.pvalue(a=asq,shape=shape)$P

  expect_equal(p_value,0.88814846)
})

test_that("AD for logistic sample", {
  x = c(-1.1263960,  0.9562103, -3.3860294,  0.1980448,  0.7667096,
        -0.8461510, -0.4524666,  1.0070690,  3.2450939,  1.1559508)
  asq = AD.logistic(x)
  p_value = AD.logistic.pvalue(a=asq)$P

  expect_equal(p_value,0.61998034)
})

test_that("AD for laplace sample", {
  x = c(-0.23539279,  0.16009027,  2.84634962,  0.35710312, -0.40466195,
        -0.41113889,  2.16169132, -0.27151351,  0.13770907,  0.02330074)
  asq = AD.laplace(x)
  p_value = AD.laplace.pvalue(a=asq)$P

  expect_equal(p_value,0.067560437)
})

test_that("AD for weibull sample", {
  x = c(0.36218715, 0.16506700, 0.16757965, 0.93681048, 1.87396510,
        0.44718470, 1.24767735, 0.07435952, 1.86023456, 0.03682825)
  asq = AD.weibull(x)
  p_value = AD.weibull.pvalue(a=asq)$P

  expect_equal(p_value,0.57984488)
})

test_that("AD for exponential sample", {
  x = c(13.0581121,  0.8301048,  0.5207504,  1.0923122,  0.7086793,
        0.1271974,  3.9326089,  0.0510448,  4.3839846,  3.4396530)
  asq = AD.exp(x)
  p_value = AD.exp.pvalue(a=asq)$P

  expect_equal(p_value,0.14574441)
})
