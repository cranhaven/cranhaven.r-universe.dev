# Potentials on every design, and exact certificates where they are recovered.
#
# Every design that has a linear program returns its dual potentials, so a
# result can be re-checked without solving again, and the implicit loops price
# the pairs they omit at zero against exact potentials, so their certificates
# carry no tolerance term.

euclid <- function(a, b) {
  as.matrix(dist(rbind(a, b)))[seq_len(nrow(a)), nrow(a) + seq_len(nrow(b))]
}

reduced <- function(cost, pot, left_ids, right_ids) {
  cost - outer(pot$left[left_ids], pot$right[right_ids], "+")
}

couples_data <- function(n, m, seed) {
  set.seed(seed)
  list(left = data.frame(id = paste0("L", seq_len(n)), x = rnorm(n), y = rnorm(n)),
       right = data.frame(id = paste0("R", seq_len(m)), x = rnorm(m), y = rnorm(m)))
}

test_that("assignment() keeps the duals its solver computed", {
  set.seed(1)
  cost <- euclid(matrix(rnorm(40), 20), matrix(rnorm(60), 30))
  for (method in c("jv", "hungarian")) {
    res <- assignment(cost, method = method)
    expect_length(res[["u"]], nrow(cost))
    expect_length(res[["v"]], ncol(cost))
    cert <- verify_assignment(res, cost)
    expect_true(cert$certified_optimal)
  }
  tall <- assignment(t(cost))
  expect_length(tall[["u"]], ncol(cost))
  expect_null(assignment(cost, method = "auction")[["u"]])
  # The padded problem's duals are not duals of `cost`.
  expect_null(assignment(cost, cardinality = "maximum")[["u"]])
})

test_that("1:1 matching potentials are tight on pairs and feasible elsewhere", {
  d <- couples_data(12, 30, 11)
  cost <- euclid(as.matrix(d$left[, c("x", "y")]), as.matrix(d$right[, c("x", "y")]))
  for (method in c("auto", "jv", "hungarian", "auction")) {
    res <- match_couples(d$left, d$right, vars = c("x", "y"), method = method)
    rc <- reduced(cost, res$potentials, d$left$id, d$right$id)
    key <- cbind(match(res$pairs$left_id, d$left$id),
                 match(res$pairs$right_id, d$right$id))
    expect_lt(max(abs(rc[key])), 1e-12)
    expect_gt(min(rc), -1e-12)
  }
})

test_that("k:1 potentials leave matched pairs at or below zero", {
  d <- couples_data(10, 40, 12)
  cost <- euclid(as.matrix(d$left[, c("x", "y")]), as.matrix(d$right[, c("x", "y")]))
  res <- match_couples(d$left, d$right, vars = c("x", "y"), ratio = 3)
  rc <- reduced(cost, res$potentials, d$left$id, d$right$id)
  key <- cbind(match(res$pairs$left_id, d$left$id),
               match(res$pairs$right_id, d$right$id))
  off <- rc
  off[key] <- NA
  expect_lt(max(rc[key]), 1e-12)
  expect_gt(min(off, na.rm = TRUE), -1e-12)
})

test_that("with-replacement potentials are each row's k-th cheapest cost", {
  d <- couples_data(15, 25, 13)
  cost <- euclid(as.matrix(d$left[, c("x", "y")]), as.matrix(d$right[, c("x", "y")]))
  res <- match_couples(d$left, d$right, vars = c("x", "y"), replace = TRUE,
                       ratio = 3)
  kth <- apply(cost, 1, function(r) sort(r)[3])
  expect_equal(unname(res$potentials$left), unname(kth))
  expect_true(all(res$potentials$right == 0))

  # The same potentials certify the matching as a flow, with every pair arc
  # carrying at most one unit and every column any number of rows.
  n <- nrow(cost)
  m <- ncol(cost)
  arcs <- data.frame(tail = rep(seq_len(n), times = m),
                     head = n + rep(seq_len(m), each = n),
                     lower = 0, upper = 1, cost = as.vector(cost))
  arcs <- rbind(arcs, data.frame(tail = n + seq_len(m), head = n + m + 1,
                                 lower = 0, upper = n, cost = 0))
  prob <- list(n_nodes = n + m + 1, supply = c(rep(3, n), rep(0, m), -3 * n),
               arcs = arcs)
  taken <- paste(match(res$pairs$left_id, d$left$id),
                 n + match(res$pairs$right_id, d$right$id))
  flow <- c(as.numeric(paste(arcs$tail[seq_len(n * m)], arcs$head[seq_len(n * m)]) %in% taken),
            tabulate(match(res$pairs$right_id, d$right$id), m))
  potential <- c(-unname(res$potentials$left), rep(0, m), 0)
  cert <- verify_flow(flow, prob, potential = potential)
  expect_true(cert$certified_optimal)
})

test_that("blocked matching merges its blocks' potentials", {
  d <- couples_data(12, 30, 14)
  d$left$g <- rep(c("a", "b"), 6)
  d$right$g <- rep(c("a", "b"), 15)
  cost <- euclid(as.matrix(d$left[, c("x", "y")]), as.matrix(d$right[, c("x", "y")]))
  cost[outer(d$left$g, d$right$g, "!=")] <- NA
  res <- match_couples(d$left, d$right, vars = c("x", "y"), block_id = "g")
  expect_named(res$potentials$left, d$left$id)
  rc <- reduced(cost, res$potentials, d$left$id, d$right$id)
  expect_gt(min(rc, na.rm = TRUE), -1e-12)
})

test_that("greedy matching carries no potentials", {
  d <- couples_data(8, 12, 15)
  res <- match_couples(d$left, d$right, vars = c("x", "y"), method = "greedy")
  expect_null(res$potentials)
})

test_that("cardinality_match() returns the potentials of its certified solve", {
  set.seed(42)
  L <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20),
                  region = rep(c("A", "B"), length.out = 20))
  R <- data.frame(id = 21:50, x = rnorm(30, 0.5), y = rnorm(30, 0.3),
                  region = rep(c("A", "B"), length.out = 30))
  fit <- cardinality_match(L, R, vars = c("x", "y"), fine = "region")
  expect_true(fit$cardinality$certified)
  cost <- euclid(as.matrix(L[, c("x", "y")]), as.matrix(R[, c("x", "y")]))
  rc <- cost - outer(fit$potentials$left, fit$potentials$right, "+")
  key <- cbind(match(fit$pairs$left_id, L$id), match(fit$pairs$right_id, R$id))
  expect_lt(max(rc[key]), 1e-9)
})

test_that("implicit assignment certifies exactly and prices at zero", {
  set.seed(3)
  a <- data.frame(id = 1:300, matrix(rnorm(600), 300))
  b <- data.frame(id = 1:500, matrix(rnorm(1000), 500))
  spec <- compute_distances(a, b, vars = c("X1", "X2"), memory_mode = "lazy")
  dense <- assignment(spec$cost_matrix)

  res <- assignment(spec$cost_matrix, memory_mode = "implicit")
  expect_identical(res$certificate$arithmetic, "exact")
  expect_equal(res$certificate$max_suboptimality, 0)
  expect_equal(res$total_cost, dense$total_cost, tolerance = 1e-12)

  # A certifying loop prices at zero against exact potentials, so a threshold
  # loose enough to stop a loop pricing at -tol short does not stop it: it
  # reaches the optimum the complete solve reaches.
  loose <- couplr:::.assignment_implicit(spec$cost_matrix, tol = 0.05, width = 1,
                                         keep_per_row = 1)
  priced <- loose$search$rounds$kind == "priced"
  expect_true(all(loose$search$rounds$exact_pricing[priced]))
  expect_identical(loose$certificate$arithmetic, "exact")
  expect_true(loose$certificate$certified_optimal)
  expect_equal(loose$total_cost, dense$total_cost, tolerance = 1e-13)

  # The same threshold on a loop that does not certify stops where the doubles
  # say nothing prices below -tol, which is short of the optimum here.
  bare <- couplr:::.assignment_implicit(spec$cost_matrix, tol = 0.05, width = 1,
                                        keep_per_row = 1, certify = FALSE)
  expect_false(any(bare$search$rounds$exact_pricing))
  expect_gt(bare$total_cost, dense$total_cost)
})

test_that("an implicit certificate re-checks from its exact potentials", {
  set.seed(2)
  cm <- matrix(sample(1:5, 150 * 200, replace = TRUE), 150, 200)
  res <- assignment(cm, memory_mode = "implicit")
  expect_identical(res$certificate$arithmetic, "exact")
  again <- verify_assignment(res, cm, arithmetic = "exact",
                             duals = list(u = res$certificate$exact_u,
                                          v = res$certificate$exact_v))
  expect_true(again$certified_optimal)
})

test_that("verify_flow() reaches an exact conclusion and refuses a worse flow", {
  prob <- list(n_nodes = 4, supply = c(2, 1, -2, -1),
               arcs = data.frame(tail = c(1, 1, 2, 2), head = c(3, 4, 3, 4),
                                 lower = 0, upper = 2,
                                 cost = c(0.1, 0.7, 0.2, 0.3)))
  solved <- couplr:::.flow_solve(prob)
  cert <- verify_flow(solved)
  expect_true(cert$certified_optimal)
  expect_identical(cert$arithmetic, "exact")

  worse <- verify_flow(c(1, 1, 1, 0), prob, arithmetic = "exact")
  expect_false(worse$certified_optimal)
})

test_that("full_match() certifies exactly, dense and implicit", {
  set.seed(8)
  left <- data.frame(id = 1:40, x = rnorm(40), y = rnorm(40))
  right <- data.frame(id = 1:90, x = rnorm(90), y = rnorm(90))
  dense <- full_match(left, right, vars = c("x", "y"))
  implicit <- full_match(left, right, vars = c("x", "y"), memory_mode = "implicit")
  expect_identical(dense$certificate$arithmetic, "exact")
  expect_identical(implicit$certificate$arithmetic, "exact")
  expect_true(implicit$certificate$certified_optimal)
  expect_equal(implicit$certificate$primal_objective,
               dense$certificate$primal_objective, tolerance = 1e-12)
})
