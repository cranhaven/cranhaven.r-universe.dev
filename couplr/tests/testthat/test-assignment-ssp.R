test_that("ssp == jv on small squares", {
  set.seed(1)
  for (n in 2:7) {
    M <- matrix(runif(n*n), n)
    a <- assignment(M, method = "jv")
    s <- assignment(M, method = "sap")
    expect_equal(a$total_cost, s$total_cost, tolerance = 1e-10)
  }
})

test_that("ssp handles rectangular with many NA", {
  set.seed(2)
  n <- 8; m <- 20
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = floor(0.5*length(M)))] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

  a <- assignment(M, method = "jv")
  s <- assignment(M, method = "sap")
  expect_equal(a$total_cost, s$total_cost, tolerance = 1e-8)
})

test_that("ssp errors when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[3, ] <- NA
  expect_error(assignment(M, method = "sap"), "Infeasible")
})

test_that("ssp terminates when tied costs price a residual arc pair below zero", {
  # A padded caliper problem, cut down to the four rows and seven columns that
  # reproduce it. Both directions of one arc round a few ulps below zero at the
  # same time, which is a cycle of negative reduced cost for the search to
  # circle. The values are exact: rounding them changes what the search finds.
  s <- 53.545017732743361
  M <- matrix(s, 4, 7)
  M[1, 2] <- 1.2751063165128582
  M[3, 2] <- 0.93851628210106453
  M[4, 2] <- 0.87495344339692427
  M[3, 7] <- 1.0984672308296912
  M[4, 7] <- 1.0349043921255510

  s_res <- assignment(M, method = "sap")
  expect_equal(s_res$status, "optimal")
  expect_equal(s_res$total_cost, assignment(M, method = "jv")$total_cost,
               tolerance = 1e-12)
})

test_that("maximize works with ssp", {
  set.seed(3)
  n <- 6; m <- 10
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = 10)] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

  a_max <- assignment(M, maximize = TRUE,  method = "sap")
  a_min <- assignment(-M, maximize = FALSE, method = "sap")
  expect_equal(a_max$total_cost, -a_min$total_cost, tolerance = 1e-8)
})
