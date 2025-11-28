library(pipenostics)

test_that("*m278hlair* errs in calculation", {
  expect_equal(
    m278hlair(),
    138.7736,
    tolerance = 1e-5
  )
})


test_that("*m278hlcha* errs in calculation", {
  expect_equal(
    m278hlcha(),
    86.92977,
    tolerance = 1e-5
  )

  expect_equal(
    optim(
      par = c(1.5, 1.5),
      fn = function(x) {
        abs(
          m278hlcha(k1 = x[1], k2 = x[2]) -
            m325nhl(year = 1980, laying = "channel", d = 250, temperature = 110)
        )
      },
      method = "L-BFGS-B",
      lower = 1.01, upper = 4.4
    )$par,
    c(4.285442, 4.323628),
    tolerance = 1e-6
  )
})


test_that("*m278hlund* errs in calculation", {
  expect_equal(
    m278hlund(),
    102.6226,
    tolerance = 1e-5
  )
})


test_that("*m278inshcm* errs in thermal conductivity", {
  data(m278insdata)
  expect_equal(
    m278inshcm(
      temperature = rep(c(110, 0), each = nrow(m278insdata)),
      material    = rep.int(m278insdata[["material"]], 2)
    ),
    c(
      c(0.096000, 0.075250, 0.149500, 0.143250, 0.149500, 0.108000, 0.096000,
        0.104250, 0.137250, 0.147250, 0.147250, 0.085250,
        0.133250, 0.157250, 0.080250, 0.060000, 0.064000, 0.059500, 0.059500,
        0.063000, 0.132500, 0.057250, 0.070000, 0.050000, 0.089875, 0.094875,
        0.059750, 0.060875, 0.069875, 0.061250, 0.064000, 0.065875, 0.089875,
        0.091875, 0.083250, 0.050000, 0.069875, 0.071875, 0.074875
      )
      ,m278insdata[["lambda"]]*1e-3 + m278insdata[["k"]]*1e-6*20
    ),
    tolerance = 1e-6
  )
})