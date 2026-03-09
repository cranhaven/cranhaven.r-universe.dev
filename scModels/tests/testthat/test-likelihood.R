test_that("Consistency of Poisson likelihoods", {
  p <- 13
  rand_pois <- rpois(100, lambda = p)
  nl_pois <- nlogL_pois(rand_pois, p)
  expect_equal(nl_pois, nlogL_zipois(rand_pois, c(0, p)))
  expect_equal(nl_pois, nlogL_pois2(rand_pois, c(1, p, 10)))
  expect_equal(nl_pois, nlogL_pois2(rand_pois, c(0, 10, p)))
  expect_equal(nl_pois, nlogL_zipois2(rand_pois, c(0, 1, p, 10)))
  expect_equal(nl_pois, nlogL_zipois2(rand_pois, c(0, 0, 10, p)))
})

test_that("Consistency of Negative-Binomial likelihoods", {
  p <- c(7, 13)
  rand_nb <- rnbinom(100, size = p[1], mu = p[2])
  nl_nb <- nlogL_nb(rand_nb, p)
  expect_equal(nl_nb, nlogL_zinb(rand_nb, c(0, p)))
  expect_equal(nl_nb, nlogL_nb2(rand_nb, c(1, p, 11, 19)))
  expect_equal(nl_nb, nlogL_nb2(rand_nb, c(0, 11, 19, p)))
  expect_equal(nl_nb, nlogL_zinb2(rand_nb, c(0, 1, p, 11, 19)))
  expect_equal(nl_nb, nlogL_zinb2(rand_nb, c(0, 0, 11, 19, p)))
})

test_that("Consistency of Poisson-Beta likelihoods", {
  p <- c(5,7,13)
  rand_pb <- rpb(100, p[1], p[2], p[3])
  nl_pb <- nlogL_pb(rand_pb, p)
  expect_equal(nl_pb, nlogL_zipb(rand_pb, c(0, p)))
  expect_equal(nl_pb, nlogL_pb2(rand_pb, c(1, p, 11, 19, 23)))
  expect_equal(nl_pb, nlogL_pb2(rand_pb, c(0, 11, 19, 23, p)))
  expect_equal(nl_pb, nlogL_zipb2(rand_pb, c(0, 1, p, 11, 19, 23)))
  expect_equal(nl_pb, nlogL_zipb2(rand_pb, c(0, 0, 11, 19, 23, p)))
})
