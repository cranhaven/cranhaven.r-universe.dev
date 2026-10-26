# Every solver checked against optima computed by independent code:
# clue::solve_LSAP (Hungarian, Hornik), lpSolve (simplex / branch and bound
# on the integer program), and optmatch (Hansen's network-flow matching).

lap_methods <- setdiff(eval(formals(assignment)$method), c("auto", "ssp"))
ref_seeds <- if (identical(Sys.getenv("NOT_CRAN"), "true")) 1:5 else 1L

# Instances each method accepts: hk01 is defined on 0/1 costs, ssap_bucket
# on costs with bounded fractional precision, bruteforce enumerates n!.
methods_for <- function(C) {
  finite <- C[is.finite(C)]
  keep <- lap_methods
  if (!all(finite %in% c(0, 1))) keep <- setdiff(keep, "hk01")
  if (any(abs(finite - round(finite)) > 0)) keep <- setdiff(keep, "ssap_bucket")
  if (nrow(C) > 8) keep <- setdiff(keep, "bruteforce")
  keep
}

# Minimum (or maximum) of sum C[i, j] x[i, j] over x binary with every row
# assigned once and every column at most once, NA/Inf cells excluded from
# the variable set. `cuts` holds earlier solutions as column vectors; each
# adds sum_i x[i, cut[i]] <= n - 1, so the program returns the best
# assignment distinct from all of them.
ref_lp <- function(C, maximize = FALSE, cuts = list()) {
  n <- nrow(C)
  m <- ncol(C)
  cells <- which(is.finite(C), arr.ind = TRUE)
  cells <- cells[order(cells[, 1], cells[, 2]), , drop = FALSE]
  nv <- nrow(cells)
  A_row <- t(vapply(seq_len(n), function(i) as.numeric(cells[, 1] == i), numeric(nv)))
  A_col <- t(vapply(seq_len(m), function(j) as.numeric(cells[, 2] == j), numeric(nv)))
  A_cut <- do.call(rbind, lapply(cuts, function(cut) {
    as.numeric(cut[cells[, 1]] == cells[, 2])
  }))
  A <- rbind(A_row, A_col, A_cut)
  dir <- c(rep("==", n), rep("<=", m), rep("<=", length(cuts)))
  rhs <- c(rep(1, n), rep(1, m), rep(n - 1, length(cuts)))
  sol <- lpSolve::lp(if (maximize) "max" else "min", C[cells], A, dir, rhs,
                     all.bin = TRUE)
  if (sol$status != 0) return(NULL)
  chosen <- cells[sol$solution > 0.5, , drop = FALSE]
  match <- integer(n)
  match[chosen[, 1]] <- chosen[, 2]
  list(value = sol$objval, match = match)
}

ref_clue <- function(C, maximize = FALSE) {
  if (nrow(C) > ncol(C)) return(ref_clue(t(C), maximize))
  shift <- if (maximize) 0 else min(C)
  perm <- clue::solve_LSAP(C - shift, maximum = maximize)
  sum(C[cbind(seq_len(nrow(C)), as.integer(perm))])
}

expect_valid_assignment <- function(res, C) {
  match <- res$match
  expect_length(match, nrow(C))
  expect_true(all(match >= 1 & match <= ncol(C)))
  expect_false(anyDuplicated(match) > 0)
  picked <- C[cbind(seq_len(nrow(C)), match)]
  expect_true(all(is.finite(picked)))
  expect_equal(res$total_cost, sum(picked), tolerance = 1e-10)
}

gen_instances <- function(seed, n = 8, m = n) {
  set.seed(seed)
  forb <- matrix(sample(1:50, n * m, TRUE), n, m)
  forb[sample(n * m, floor(0.4 * n * m))] <- NA
  forb[cbind(seq_len(n), sample(m, n))] <- sample(1:50, n, TRUE)
  list(
    real      = matrix(runif(n * m), n, m),
    integer   = matrix(sample(0:100, n * m, TRUE), n, m),
    ties      = matrix(sample(0:3, n * m, TRUE), n, m),
    negative  = matrix(sample(-50:50, n * m, TRUE), n, m),
    magnitude = matrix(runif(n * m, 1e6, 2e6), n, m),
    binary    = matrix(sample(0:1, n * m, TRUE), n, m),
    forbidden = forb
  )
}

test_that("every LAP method reaches the integer-program optimum on small instances", {
  skip_if_not_installed("lpSolve")
  for (seed in ref_seeds) {
    shapes <- list(c(8, 8), c(6, 9))
    for (shape in shapes) {
      insts <- gen_instances(seed, shape[1], shape[2])
      for (fam in names(insts)) {
        C <- insts[[fam]]
        ref <- ref_lp(C)
        for (method in methods_for(C)) {
          res <- assignment(C, method = method)
          label <- sprintf("seed %d, %dx%d %s, %s", seed, shape[1], shape[2], fam, method)
          expect_valid_assignment(res, C)
          expect_equal(res$total_cost, ref$value, tolerance = 1e-9, label = label)
        }
      }
    }
  }
})

test_that("every LAP method maximizes to the integer-program optimum", {
  skip_if_not_installed("lpSolve")
  for (seed in ref_seeds) {
    insts <- gen_instances(seed)[c("real", "integer", "ties", "negative", "forbidden")]
    for (fam in names(insts)) {
      C <- insts[[fam]]
      ref <- ref_lp(C, maximize = TRUE)
      for (method in methods_for(C)) {
        res <- assignment(C, maximize = TRUE, method = method)
        expect_valid_assignment(res, C)
        expect_equal(res$total_cost, ref$value, tolerance = 1e-9,
                     label = sprintf("seed %d %s max, %s", seed, fam, method))
      }
    }
  }
})

test_that("every LAP method matches clue::solve_LSAP at n = 120", {
  skip_on_cran()
  skip_if_not_installed("clue")
  set.seed(42)
  n <- 120
  insts <- list(
    real      = matrix(runif(n * n), n),
    integer   = matrix(sample(0:1000, n * n, TRUE), n),
    ties      = matrix(sample(0:5, n * n, TRUE), n),
    rectangle = matrix(runif(80 * n), 80, n),
    binary    = matrix(sample(0:1, n * n, TRUE), n)
  )
  for (fam in names(insts)) {
    C <- insts[[fam]]
    ref <- ref_clue(C)
    for (method in methods_for(C)) {
      res <- assignment(C, method = method)
      expect_valid_assignment(res, C)
      expect_equal(res$total_cost, ref, tolerance = 1e-9,
                   label = sprintf("n=120 %s, %s", fam, method))
    }
  }
})

test_that("more rows than columns is solved as the transposed problem", {
  skip_if_not_installed("clue")
  set.seed(7)
  C <- matrix(runif(9 * 6), 9, 6)
  expect_equal(get_total_cost(lap_solve(C)), ref_clue(C), tolerance = 1e-9)
})

test_that("k-best costs equal the integer program with no-good cuts", {
  skip_if_not_installed("lpSolve")
  for (seed in 1:4) {
    set.seed(seed)
    C <- if (seed %% 2) matrix(runif(36), 6) else matrix(sample(0:4, 36, TRUE), 6)
    k <- 8
    ref <- numeric(k)
    cuts <- list()
    for (r in seq_len(k)) {
      sol <- ref_lp(C, cuts = cuts)
      ref[r] <- sol$value
      cuts[[r]] <- sol$match
    }
    kb <- lap_solve_kbest(C, k = k)
    totals <- tapply(kb$total_cost, kb$rank, unique)
    expect_equal(unname(as.numeric(totals)), ref, tolerance = 1e-9,
                 label = sprintf("k-best seed %d", seed))
    sols <- split(kb$target[order(kb$rank, kb$source)], kb$rank[order(kb$rank, kb$source)])
    expect_equal(length(unique(sols)), k)
  }
})

test_that("bottleneck value equals the smallest feasible threshold found by clue", {
  skip_if_not_installed("clue")
  for (seed in ref_seeds) {
    set.seed(seed)
    C <- matrix(sample(1:40, 64, TRUE), 8)
    if (seed > 3) C[sample(64, 20)] <- NA
    feasible <- function(thr) {
      allowed <- is.finite(C) & C <= thr
      clue_cost <- ifelse(allowed, 0, 1)
      sum(clue_cost[cbind(1:8, as.integer(clue::solve_LSAP(clue_cost)))]) == 0
    }
    vals <- sort(unique(C[is.finite(C)]))
    ref <- vals[which(vapply(vals, feasible, logical(1)))[1]]
    res <- bottleneck_assignment(C)
    expect_equal(res$bottleneck, ref, label = sprintf("bottleneck seed %d", seed))
    expect_equal(max(C[cbind(1:8, res$match)]), ref)
  }
})

test_that("line-metric solver matches clue on |x - y| and (x - y)^2 costs", {
  skip_if_not_installed("clue")
  set.seed(3)
  for (m in c(12, 20)) {
    x <- runif(12, 0, 10)
    y <- runif(m, 0, 10)
    for (cost in c("L1", "sq")) {
      C <- if (cost == "L1") abs(outer(x, y, "-")) else outer(x, y, "-")^2
      res <- lap_solve_line_metric(x, y, cost = cost)
      expect_equal(res$total_cost, ref_clue(C), tolerance = 1e-9,
                   label = sprintf("line metric m=%d %s", m, cost))
    }
  }
})

test_that("sinkhorn cost is within log(nm)/lambda of the exact transport optimum", {
  skip_if_not_installed("lpSolve")
  set.seed(11)
  n <- 10
  m <- 14
  C <- matrix(runif(n * m), n, m)
  exact <- lpSolve::lp.transport(C, "min",
                                 rep("=", n), rep(1 / n, n),
                                 rep("=", m), rep(1 / m, m),
                                 integers = NULL)$objval
  for (lambda in c(20, 50, 100)) {
    sk <- sinkhorn(C, lambda = lambda, tol = 1e-12, max_iter = 1e5)
    expect_true(sk$converged)
    expect_equal(rowSums(sk$transport_plan), rep(1 / n, n), tolerance = 1e-6)
    expect_equal(colSums(sk$transport_plan), rep(1 / m, m), tolerance = 1e-6)
    expect_gte(sk$cost, exact - 1e-9)
    expect_lte(sk$cost - exact, log(n * m) / lambda)
  }
})

# Integer covariates give integer distances, which optmatch solves without
# rounding; continuous ones give strictly positive distances, which
# optmatch::fullmatch requires (Hansen and Klopfer 2006, sec. 4.1).
matching_frames <- function(seed, n_left, n_right, continuous = FALSE) {
  set.seed(seed)
  draw <- function(k) if (continuous) runif(k, 0, 30) else sample(0:30, k, TRUE)
  left <- data.frame(id = paste0("L", seq_len(n_left)), x1 = draw(n_left), x2 = draw(n_left))
  right <- data.frame(id = paste0("R", seq_len(n_right)), x1 = draw(n_right), x2 = draw(n_right))
  D <- abs(outer(left$x1, right$x1, "-")) + abs(outer(left$x2, right$x2, "-"))
  dimnames(D) <- list(left$id, right$id)
  list(left = left, right = right, D = D)
}

unit_frame <- function(D) data.frame(row.names = c(rownames(D), colnames(D)))

optmatch_total <- function(fm, D) {
  sets <- split(names(fm), as.character(fm))
  sum(vapply(sets, function(u) {
    l <- intersect(u, rownames(D))
    r <- intersect(u, colnames(D))
    sum(D[l, r, drop = FALSE])
  }, numeric(1)))
}

test_that("pair and ratio matching reach optmatch::pairmatch's total distance", {
  skip_on_cran()
  skip_if_not_installed("optmatch")
  suppressPackageStartupMessages(library(optmatch))
  for (seed in 1:3) {
    for (ratio in 1:2) {
      fr <- matching_frames(seed, 25, 70)
      ours <- match_couples(fr$left, fr$right, vars = c("x1", "x2"),
                            distance = "manhattan", ratio = ratio)
      theirs <- optmatch::pairmatch(fr$D, controls = ratio, data = unit_frame(fr$D))
      expect_equal(nrow(ours$pairs), 25 * ratio)
      expect_equal(sum(ours$pairs$distance), optmatch_total(theirs, fr$D),
                   label = sprintf("pairmatch seed %d ratio %d", seed, ratio))
    }
  }
})

test_that("full matching reaches optmatch::fullmatch's total distance", {
  skip_on_cran()
  skip_if_not_installed("optmatch")
  suppressPackageStartupMessages(library(optmatch))
  for (seed in 1:3) {
    for (mx in c(Inf, 3)) {
      fr <- matching_frames(seed, 20, 45, continuous = TRUE)
      ours <- full_match(fr$left, fr$right, vars = c("x1", "x2"),
                         distance = "manhattan", max_controls = mx)
      expect_equal(ours$status, "optimal")
      grp <- split(ours$groups$id, ours$groups$group_id)
      our_total <- sum(vapply(grp, function(u) {
        sum(fr$D[intersect(u, rownames(fr$D)), intersect(u, colnames(fr$D)), drop = FALSE])
      }, numeric(1)))
      theirs <- optmatch::fullmatch(fr$D, min.controls = 1 / mx, max.controls = mx,
                                    data = unit_frame(fr$D),
                                    tol = 1e-9)
      expect_equal(our_total, optmatch_total(theirs, fr$D), tolerance = 1e-7,
                   label = sprintf("fullmatch seed %d max %s", seed, mx))
    }
  }
})
