# match_path(): one matching per value of one design choice, solved as one
# sequence.
#
# The claim every equality test makes is that a point of a path is the matching
# an independent call at that value finds. The path reaches it warm, from the
# previous point's flow and an arc set that has been growing since the first
# value, and an independent call reaches it cold from an empty candidate set;
# the two arrive at the same optimum through different arc sets, so the
# assertion is on the total and the matched count rather than on which optimum
# is named.

PATH_COST_TOL <- 1e-9

path_data <- function(nr, nc, p = 3, seed = 1) {
  set.seed(seed)
  left <- data.frame(id = seq_len(nr))
  right <- data.frame(id = seq_len(nc))
  for (k in seq_len(p)) {
    left[[paste0("x", k)]] <- stats::rnorm(nr)
    right[[paste0("x", k)]] <- stats::rnorm(nc)
  }
  list(left = left, right = right, vars = paste0("x", seq_len(p)))
}

test_that("a point of the path is the matching a solve at that value finds", {
  d <- path_data(20, 90, seed = 5101)
  values <- c(0.8, 1.2, 1.6, 2.5, Inf)

  path <- match_path(d$left, d$right, vars = d$vars, values = values)
  expect_s3_class(path, "couplr_path")
  expect_equal(nrow(path$path), length(values))
  expect_equal(path$path$max_distance, values)

  for (k in seq_along(values)) {
    one <- suppressWarnings(
      match_couples(d$left, d$right, vars = d$vars,
                    max_distance = values[k], memory_mode = "implicit")
    )
    expect_equal(path$path$n_matched[k], nrow(one$pairs))
    if (identical(path$path$status[k], "optimal")) {
      expect_equal(path$path$total_distance[k], sum(one$pairs$distance),
                   tolerance = PATH_COST_TOL)
    }
  }
})

test_that("every point that found a matching carries a certificate", {
  d <- path_data(15, 80, seed = 5102)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(1.5, 2, Inf))

  for (k in seq_len(nrow(path$path))) {
    if (!identical(path$path$status[k], "optimal")) next
    cert <- path$certificate[[k]]
    expect_s3_class(cert, "assignment_certificate")
    expect_true(cert$certified_optimal)
    expect_true(path$path$certified[k])
  }
})

test_that("certify = FALSE leaves the answer and drops the proof", {
  d <- path_data(12, 60, seed = 5103)
  values <- c(1.5, 2.5, Inf)

  proved <- match_path(d$left, d$right, vars = d$vars, values = values)
  bare <- match_path(d$left, d$right, vars = d$vars, values = values,
                     certify = FALSE)

  expect_equal(bare$path$total_distance, proved$path$total_distance,
               tolerance = PATH_COST_TOL)
  expect_equal(bare$path$n_matched, proved$path$n_matched)
  expect_true(all(vapply(bare$certificate, is.null, logical(1))))
  expect_false(any(bare$path$certified))
})

test_that("each point's match vector is a matching over the left units", {
  d <- path_data(18, 70, seed = 5104)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(1.5, 2, Inf))

  for (k in seq_len(nrow(path$path))) {
    m <- path$match[[k]]
    expect_length(m, nrow(d$left))
    taken <- m[m > 0L]
    expect_equal(anyDuplicated(taken), 0L)
    expect_true(all(taken <= nrow(d$right)))
    expect_equal(length(taken), path$path$n_matched[k])
  }
})

test_that("the path reads back over the left units when the left side is longer", {
  d <- path_data(70, 25, seed = 5105)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(2, Inf))

  expect_true(path$transposed)
  for (k in seq_len(nrow(path$path))) {
    expect_length(path$match[[k]], nrow(d$left))
    taken <- path$match[[k]]
    expect_true(all(taken[taken > 0L] <= nrow(d$right)))
  }
  expect_equal(path$path$n_matched[nrow(path$path)], nrow(d$right))
})

test_that("a point too tight to match reports the witness rather than a total", {
  d <- path_data(30, 35, seed = 5106)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(0.05, Inf))

  expect_equal(path$path$status[1], "infeasible")
  expect_true(is.na(path$path$total_distance[1]))
  expect_false(is.null(path$witness[[1]]))
  expect_true(length(path$witness[[1]]$rows) > 0L)

  expect_equal(path$path$status[2], "optimal")
  expect_true(path$path$certified[2])
})

test_that("per-variable calipers hold across the path", {
  d <- path_data(20, 90, seed = 5107)
  cal <- list(x1 = 0.5)
  values <- c(1.5, Inf)

  path <- match_path(d$left, d$right, vars = d$vars, values = values,
                     calipers = cal)

  for (k in seq_along(values)) {
    one <- suppressWarnings(
      match_couples(d$left, d$right, vars = d$vars, calipers = cal,
                    max_distance = values[k], memory_mode = "implicit")
    )
    expect_equal(path$path$n_matched[k], nrow(one$pairs))
    if (identical(path$path$status[k], "optimal")) {
      expect_equal(path$path$total_distance[k], sum(one$pairs$distance),
                   tolerance = PATH_COST_TOL)
    }
  }
})

test_that("a Mahalanobis path agrees with the solves it replaces", {
  d <- path_data(16, 70, seed = 5108)
  values <- c(2, 3, Inf)

  path <- match_path(d$left, d$right, vars = d$vars, values = values,
                     distance = "mahalanobis")

  for (k in seq_along(values)) {
    one <- suppressWarnings(
      match_couples(d$left, d$right, vars = d$vars, distance = "mahalanobis",
                    max_distance = values[k], memory_mode = "implicit")
    )
    expect_equal(path$path$n_matched[k], nrow(one$pairs))
    if (identical(path$path$status[k], "optimal")) {
      expect_equal(path$path$total_distance[k], sum(one$pairs$distance),
                   tolerance = PATH_COST_TOL)
    }
  }
})

test_that("a one-point path is a solve", {
  d <- path_data(14, 60, seed = 5109)
  path <- match_path(d$left, d$right, vars = d$vars, values = Inf)
  one <- match_couples(d$left, d$right, vars = d$vars,
                       memory_mode = "implicit")

  expect_equal(nrow(path$path), 1L)
  expect_equal(path$path$n_matched, nrow(one$pairs))
  expect_equal(path$path$total_distance, sum(one$pairs$distance),
               tolerance = PATH_COST_TOL)
})

test_that("the search record counts what the whole path cost", {
  d <- path_data(20, 90, seed = 5110)
  path <- match_path(d$left, d$right, vars = d$vars,
                     values = c(1, 1.5, 2, Inf))

  expect_equal(path$search$possible_edges, 20 * 90)
  expect_equal(path$search$candidate_edges,
               path$path$candidate_edges[nrow(path$path)])
  expect_equal(path$search$edges_evaluated, sum(path$path$edges_evaluated))
  # The candidate set only ever grows, so a point's own additions are the
  # difference between its total and the one before it.
  expect_equal(cumsum(path$path$pairs_added), path$path$candidate_edges)
  expect_true(all(path$path$n_rounds >= 1L))
})

test_that("a descending sweep is refused and says why", {
  d <- path_data(10, 40, seed = 5111)
  expect_error(
    match_path(d$left, d$right, vars = d$vars, values = c(2, 1)),
    "must ascend"
  )
  expect_error(
    match_path(d$left, d$right, vars = d$vars, values = c(1, 1)),
    "must ascend"
  )
})

test_that("the values and the knob are checked before anything is solved", {
  d <- path_data(10, 40, seed = 5112)

  expect_error(match_path(d$left, d$right, vars = d$vars, values = numeric(0)),
               "at least one value")
  expect_error(match_path(d$left, d$right, vars = d$vars, values = c(1, NA)),
               "missing value")
  expect_error(match_path(d$left, d$right, vars = d$vars, values = c("a", "b")),
               "numeric vector")
  expect_error(match_path(d$left, d$right, vars = d$vars, values = c(-1, 1)),
               "must .*be positive")
  expect_error(match_path(d$left, d$right, vars = d$vars, values = c(0, 1)),
               "must .*be positive")
  expect_error(
    match_path(d$left, d$right, vars = d$vars, vary = "ratio", values = 1:3),
    "`vary` must be one of"
  )
})

# A point's balance is the claim that the sweep is read for: what the matching
# at this value did to the covariates. The assertion is that it is the package's
# own balance for that matching, reached the other way round -- through the ids
# and the merge `balance_diagnostics()` does -- rather than a second reading.

as_path_point_result <- function(m, left, right, vars) {
  rows <- which(m > 0L)
  structure(
    list(
      pairs = tibble::tibble(
        left_id  = left$id[rows],
        right_id = right$id[m[rows]],
        distance = NA_real_
      ),
      info = list(vars = vars, method = "path")
    ),
    class = c("matching_result", "couplr_result")
  )
}

test_that("a point's balance is the balance of the matching it found", {
  d <- path_data(20, 90, seed = 5113)
  values <- c(1.2, 2, Inf)
  path <- match_path(d$left, d$right, vars = d$vars, values = values)

  expect_equal(nrow(path$balance), length(values) * length(d$vars))
  expect_equal(path$balance$variable, rep(d$vars, times = length(values)))
  expect_equal(path$balance$max_distance, rep(values, each = length(d$vars)))

  for (k in seq_along(values)) {
    bal <- balance_diagnostics(
      as_path_point_result(path$match[[k]], d$left, d$right, d$vars),
      d$left, d$right, vars = d$vars
    )

    expect_equal(path$path$mean_abs_std_diff[k], bal$overall$mean_abs_std_diff)
    expect_equal(path$path$max_abs_std_diff[k], bal$overall$max_abs_std_diff)

    point <- path$balance[path$balance$max_distance == values[k], ]
    expect_equal(point$std_diff, bal$var_stats$std_diff)
    expect_equal(point$var_ratio, bal$var_stats$var_ratio)
    expect_equal(point$mean_diff, bal$var_stats$mean_diff)
    expect_equal(point$n_left, rep(path$path$n_matched[k], length(d$vars)))
  }
})

test_that("balance is read on the variables the caller passed", {
  d <- path_data(20, 90, seed = 5114)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(2, Inf),
                     weights = c(8, 1, 1))

  m <- path$match[[1]]
  rows <- which(m > 0L)
  first <- path$balance[path$balance$max_distance == 2, ]

  expect_equal(first$mean_left,
               vapply(d$vars, function(v) mean(d$left[[v]][rows]), numeric(1)),
               ignore_attr = TRUE)
  expect_equal(first$mean_right,
               vapply(d$vars, function(v) mean(d$right[[v]][m[rows]]),
                      numeric(1)),
               ignore_attr = TRUE)
})

test_that("a point too tight to match reports no balance", {
  d <- path_data(30, 35, seed = 5106)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(0.05, Inf))

  expect_equal(path$path$status[1], "infeasible")
  expect_equal(path$path$n_matched[1], 0L)
  expect_true(is.na(path$path$mean_abs_std_diff[1]))
  expect_true(is.na(path$path$max_abs_std_diff[1]))

  first <- path$balance[path$balance$max_distance == 0.05, ]
  expect_equal(nrow(first), length(d$vars))
  expect_true(all(is.na(first$std_diff)))

  expect_false(is.na(path$path$mean_abs_std_diff[2]))
})

test_that("balance holds its orientation when the left side is longer", {
  d <- path_data(70, 25, seed = 5115)
  path <- match_path(d$left, d$right, vars = d$vars, values = c(2, Inf))
  expect_true(path$transposed)

  for (k in seq_len(nrow(path$path))) {
    bal <- balance_diagnostics(
      as_path_point_result(path$match[[k]], d$left, d$right, d$vars),
      d$left, d$right, vars = d$vars
    )
    point <- path$balance[path$balance$max_distance == path$path$max_distance[k], ]
    expect_equal(point$mean_left, bal$var_stats$mean_left)
    expect_equal(point$mean_right, bal$var_stats$mean_right)
    expect_equal(point$std_diff, bal$var_stats$std_diff)
  }
})
