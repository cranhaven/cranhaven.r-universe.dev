test_that("qevbs and pevbs are inverses", {
  eta <- 2.58; a <- 0.19; g <- -0.16
  p <- c(0.5, 0.9, 0.99)
  expect_equal(pevbs(qevbs(p, eta, a, g), eta, a, g), p, tolerance = 1e-8)
})
test_that("EVBS density integrates to one", {
  eta <- 2.58; a <- 0.19; g <- -0.16
  E <- evbs_endpoint(eta, a, g)
  I <- integrate(function(t) devbs(t, eta, a, g), 0.01, E)$value
  expect_equal(I, 1, tolerance = 1e-4)
})
test_that("endpoint is finite iff gama < 0", {
  expect_true(is.finite(evbs_endpoint(2.58, 0.19, -0.16)))
  expect_identical(evbs_endpoint(2.58, 0.19, 0.10), Inf)
  expect_equal(devbs(evbs_endpoint(2.58, 0.19, -0.16) + 1, 2.58, 0.19, -0.16), 0)
})
test_that("return level reproduces the Itajai analysis", {
  data(itajai)
  f <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
  rl <- evbs_return_level(f, c(1, mean(itajai$pressure)), 12 * 50)
  expect_equal(unname(rl["return_level"]), 27.61, tolerance = 0.05)
})
test_that("deleting obs 82 pushes the endpoint below the observed event", {
  data(itajai)
  pe <- itajai$pressure[82]
  ff <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
  fr <- evbsreg.fit(cbind(1, itajai$pressure[-82]), itajai$wind[-82])
  ef <- evbs_endpoint(ff$betahat[1] + ff$betahat[2] * pe, ff$alphahat, ff$gamahat)
  er <- evbs_endpoint(fr$betahat[1] + fr$betahat[2] * pe, fr$alphahat, fr$gamahat)
  expect_gt(ef, itajai$wind[82])   # possible with the event
  expect_lt(er, itajai$wind[82])   # impossible without it
})
