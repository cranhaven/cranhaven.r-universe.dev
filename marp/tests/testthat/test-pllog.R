test_that("pllog", {
  q <- c(1, 2, 3, 4)
  shape <- 5
  scale <- 3
  log <- FALSE
  result <- marp::pllog(q, shape, scale, log)
  expected_result <- as.numeric(c(0.995901639344262 , 0.883636363636364 , 0.500000000000000 , 0.191791633780584))
  expect_true(all.equal(result, expected_result))

  q <- c(1, 2, 3, 4)
  shape <- 5
  scale <- 3
  log <- TRUE
  result <- marp::pllog(q, shape, scale, log)
  expected_result <-  as.numeric(c(0.0040983606557377 , 0.1163636363636363 , 0.5000000000000000 , 0.8082083662194159))
  expect_true(all.equal(result, expected_result, tolerance = 1.5e-7))
})
