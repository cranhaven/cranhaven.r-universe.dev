test_that("auction == jv on small square & rectangular with NA", {
  set.seed(1)
  for (n in 2:8) {
    m <- n + 2
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.2*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    a1 <- assignment(M, method = "jv")
    a2 <- assignment(M, method = "auction")
    expect_equal(a1$total_cost, a2$total_cost, tolerance = 1e-8)

    a1 <- assignment(M, maximize = TRUE, method = "jv")
    a2 <- assignment(M, maximize = TRUE, method = "auction")
    expect_equal(a1$total_cost, a2$total_cost, tolerance = 1e-8)
  }
})

test_that("auction(errors) when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[2,] <- NA
  expect_error(assignment(M, method = "auction"), "Infeasible")
})

test_that("auction with explicit epsilon matches JV", {
  set.seed(2)
  for (n in 3:6) {
    m <- n + 1
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.15*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    a_jv <- assignment(M, method = "jv")
    for (eps in c(1e-6, 1e-9, 1e-12)) {
      a_auc <- assignment(M, method = "auction", auction_eps = eps)
      expect_equal(a_auc$total_cost, a_jv$total_cost, tolerance = 1e-8)
    }

    a_jv <- assignment(M, maximize = TRUE, method = "jv")
    for (eps in c(1e-6, 1e-9, 1e-12)) {
      a_auc <- assignment(M, method = "auction", maximize = TRUE, auction_eps = eps)
      expect_equal(a_auc$total_cost, a_jv$total_cost, tolerance = 1e-8)
    }
  }
})

test_that("auction_scaled(pow2) == JV on small square & rectangular with NA", {
  set.seed(3)
  for (n in 2:7) {
    m <- n + 3
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.25*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    # direct call to the scaled interface
    s_min <- lap_solve_auction_scaled(M, maximize = FALSE, schedule = "pow2")
    s_max <- lap_solve_auction_scaled(M, maximize = TRUE,  schedule = "pow2")

    a_min <- assignment(M, method = "jv")
    a_max <- assignment(M, maximize = TRUE, method = "jv")

    expect_equal(sum(M[cbind(seq_len(n), s_min$match)]), a_min$total_cost, tolerance = 1e-8)
    expect_equal(sum(M[cbind(seq_len(n), s_max$match)]), a_max$total_cost, tolerance = 1e-8)
  }
})

test_that("every auction variant certifies optimal on costs finer than its final epsilon", {
  set.seed(11)
  for (shape in list(c(40, 40), c(30, 45))) {
    n <- shape[1]
    m <- shape[2]
    cost <- matrix(rlnorm(n * m, sdlog = 3), n, m) / 1e3
    cost[sample.int(n * m, floor(0.1 * n * m))] <- NA
    for (i in seq_len(n)) if (all(is.na(cost[i, ]))) cost[i, sample.int(m, 1)] <- 1e-4

    for (maximize in c(FALSE, TRUE)) {
      for (method in c("auction", "auction_gs", "auction_scaled")) {
        res <- assignment(cost, method = method, maximize = maximize)
        cert <- verify_assignment(res, cost, maximize = maximize)
        expect_true(cert$certified_optimal,
                    info = sprintf("%s, %dx%d, maximize = %s", method, n, m, maximize))
      }
    }
  }
})

test_that("the lazy auction certifies optimal on distances finer than its final epsilon", {
  set.seed(12)
  left <- data.frame(x = runif(60) * 1e-5, y = runif(60) * 1e-5)
  right <- data.frame(x = runif(90) * 1e-5, y = runif(90) * 1e-5)
  spec <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  dense <- compute_distance_matrix(as.matrix(left), as.matrix(right), distance = "euclidean")

  res <- assignment(spec, method = "auction")
  expect_true(verify_assignment(res, dense)$certified_optimal)
})

test_that("the auction's bids are unchanged when every cost is multiplied by a constant", {
  set.seed(13)
  for (gen in list(function(k) runif(k), function(k) rlnorm(k, sdlog = 3),
                   function(k) as.double(sample.int(5L, k, replace = TRUE)))) {
    cost <- matrix(gen(80 * 80), 80, 80)
    base <- couplr:::lap_solve_auction_gs(cost, maximize = FALSE)
    scaled <- couplr:::lap_solve_auction_gs(cost * 2^20, maximize = FALSE)
    expect_identical(scaled$match, base$match)
    expect_identical(scaled$bids, base$bids)
  }
})

test_that("the auction certifies optimal on a rectangular problem at a large cost magnitude", {
  set.seed(14)
  cost <- matrix(runif(60 * 120), 60, 120) * 1e8
  for (method in c("auction", "auction_gs", "csa")) {
    res <- assignment(cost, method = method)
    expect_true(verify_assignment(res, cost)$certified_optimal, info = method)
  }
})

test_that("auction_scaled errors on forbidden row", {
  M <- matrix(1, 4, 6)
  M[4,] <- NA
  expect_error(lap_solve_auction_scaled(M, maximize = FALSE, schedule = "pow2"), "Infeasible")
})
