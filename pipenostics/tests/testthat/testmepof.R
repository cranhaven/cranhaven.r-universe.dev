library(pipenostics)

test_that("*mepof* produces valuable probability assesment", {

  pof <- mepof(
    c(2.45,  7.86,   7.93,   8.15),
    rep(200, 4),
    rep(762, 4),
    rep(10, 4),
    rep(434.3697, 4),
    rep(0.588399, 4),
    rep(95, 4),
   method = "dnv", days = -2 * 365)

  expect_equal(
    mepof(8.2, 200, 762, 10, 289.5798, 0.588399, 95, method = "b31g"),
    0.843593,
    tolerance = 1e-2
  )

  expect_equal(
    mepof(8.2, 200, 762, 10, 289.5798, 0.588399, 95, method = "b31gmod"),
    0.843534,
    tolerance = 1e-2
  )

})
