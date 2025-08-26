context("test-gof.bootstrap.R")

test_that("gof bootstrap method for uniform sample", {
  x = c(0.3103205, -0.1386720, -0.5988813,  0.9483934, -0.5213117,
        0.7555062, -0.6821612,  0.7957394, -0.7387161,  0.1910647)
  result = gof.uniform.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.135374876)
  expect_equal(result$Asq,0.55305697)
  expect_equal(result$Usq,0.1168008)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for normal sample", {
  x = c(0.25024690, -0.33712454, -0.11335370, -0.09888291, 0.26408682,
        0.13898369, -0.24226950, 0.05903138, -0.17727187, 0.79468027)
  result = gof.normal.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.051555557)
  expect_equal(result$Asq,0.380504793)
  expect_equal(result$Usq,0.044353005)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for gamma sample", {
  x = c(0.5047757, 0.1538300, 0.5704100, 0.3013008, 1.2775724,
        1.0468233, 0.6525627, 0.4376768, 2.4700737, 1.0944885)
  result = gof.gamma.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.029695982)
  expect_equal(result$Asq,0.201390974)
  expect_equal(result$Usq,0.027784624)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for logistic sample", {
  x = c(-1.1263960,  0.9562103, -3.3860294,  0.1980448,  0.7667096,
        -0.8461510, -0.4524666,  1.0070690,  3.2450939,  1.1559508)
  result = gof.logistic.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.039618567)
  expect_equal(result$Asq,0.27184557)
  expect_equal(result$Usq,0.039618567)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for Laplace sample", {
  x = c(-0.23539279,  0.16009027,  2.84634962,  0.35710312, -0.40466195,
        -0.41113889,  2.16169132, -0.27151351,  0.13770907,  0.02330074)
  result = gof.laplace.bootstrap(x,M=100)

  expect_equal(result$Wsq, 0.147393763)
  expect_equal(result$Asq, 0.91031712)
  expect_equal(result$Usq, 0.131789187)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for Weibull sample", {
  x = c(0.36218715, 0.16506700, 0.16757965, 0.93681048, 1.87396510,
        0.44718470, 1.24767735, 0.07435952, 1.86023456, 0.03682825)
  result = gof.weibull.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.046013393)
  expect_equal(result$Asq,0.30993253)
  expect_equal(result$Usq, 0.045718390)

  expect_output(str(result), "List of 6")
})

test_that("gof bootstrap method for Exponential sample", {
  x = c(13.0581121,  0.8301048,  0.5207504,  1.0923122,  0.7086793,
        0.1271974,  3.9326089,  0.0510448,  4.3839846,  3.4396530)
  result = gof.exp.bootstrap(x,M=100)

  expect_equal(result$Wsq,0.141736477)
  expect_equal(result$Asq,0.93011782)
  expect_equal(result$Usq,0.088244806)

  expect_output(str(result), "List of 6")
})
