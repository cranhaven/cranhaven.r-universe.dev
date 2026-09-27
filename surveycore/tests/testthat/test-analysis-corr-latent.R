# Tests for get_corr() — PR 3 public-API integration of polychoric / polyserial.
#
# Scope: integration tests through the exported get_corr() covering
#   - method = "pearson" bit-identity regression guard
#   - method = "polychoric" / "polyserial" happy paths on Taylor and replicate
#   - polycor::polychor() / polycor::polyserial() parity (1e-6)
#   - multi-pair long format, redundant / diagonal / wide
#   - grouped estimation
#   - variance column selection (se, ci, moe, deff, NULL)
#   - df / statistic / p_value method-awareness
#   - column label attributes (method-neutral)
#   - edge cases (empty domain, single-level, all-NA, zero-weight rows,
#     integer ordinal, 6x7 ordinal, surveytidy::filter domain)
#   - survey_collection dispatch
#   - meta(result)$method, bivariate_normal_cdf, n_failed_replicates_total
#
# Error / warning classes exercised at the public-API layer:
#   PC-1 .. PC-14 plus pre-existing Pearson classes (regression guard).

skip_on_cran()

# ── helpers ───────────────────────────────────────────────────────────────────

# Build a small unweighted Taylor design with two 4-level ordinal factors;
# correlated via a shifted coding of o1 into o2.
make_latent_taylor <- function(
  n = 200L,
  seed = 42L,
  n_psu = 20L,
  n_strata = 4L
) {
  set.seed(seed)
  df <- make_survey_data(
    n = n,
    n_psu = n_psu,
    n_strata = n_strata,
    seed = seed
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  df$o3 <- factor(sample(1:3, nrow(df), replace = TRUE), ordered = TRUE)
  df$cont <- as.numeric(df$o1) + stats::rnorm(nrow(df), sd = 0.7)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

# Unweighted-equivalent Taylor design (weights all 1, no strata): lets
# us sanity-check polychoric against polycor::polychor() at 1e-6.
#
# Uses the latent-Gaussian cut pattern from PR 1's
# tests/testthat/test-analysis-corr-latent-primitives.R (`make_ordinal_pair`):
# draw (x*, y*) bivariate-normal with correlation `rho`, then cut each by
# marginal quantiles into k_x / k_y ordered levels. This matches the
# fixture construction that PR 1 achieves 1e-6 parity against
# polycor::polychor() on — so the public-API wrapper should too.
make_unit_weight_design <- function(
  n = 200L,
  seed = 1L,
  n_levels_x = 4L,
  n_levels_y = 5L,
  rho = -0.3
) {
  set.seed(seed)
  x_latent <- stats::rnorm(n)
  y_latent <- rho * x_latent + sqrt(1 - rho^2) * stats::rnorm(n)
  tx <- stats::qnorm(seq_len(n_levels_x - 1L) / n_levels_x)
  ty <- stats::qnorm(seq_len(n_levels_y - 1L) / n_levels_y)
  x_ord <- as.integer(cut(x_latent, c(-Inf, tx, Inf), include.lowest = TRUE))
  y_ord <- as.integer(cut(y_latent, c(-Inf, ty, Inf), include.lowest = TRUE))
  df <- data.frame(
    id = seq_len(n),
    wt = 1,
    o1 = factor(x_ord, levels = seq_len(n_levels_x), ordered = TRUE),
    o2 = factor(y_ord, levels = seq_len(n_levels_y), ordered = TRUE),
    cont = x_latent + stats::rnorm(n, sd = 0.7),
    grp = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  list(df = df, design = as_survey(df, weights = wt))
}

# Hand-computed two-step polyserial MLE (Cox 1974; Mannan 2025 §5.1) —
# the strict oracle surveycore's .corr_polyserial_mle() targets at 1e-6.
# .hand_polyserial_twostep() — the strict polyserial oracle — lives in
# helper-test-data.R (shared with test-analysis-corr-latent-primitives.R).

# Hand-computed oracle polyserial fixture matching PR 1's
# make_polyserial_pair (rho = 0.5, n = 500, k_ord = 3, seed = 21).
make_polyserial_fixture <- function(rho, n, k_ord, seed) {
  set.seed(seed)
  ord_latent <- stats::rnorm(n)
  cont <- rho * ord_latent + sqrt(1 - rho^2) * stats::rnorm(n)
  tx <- stats::qnorm(seq_len(k_ord - 1L) / k_ord)
  ord <- as.integer(cut(ord_latent, c(-Inf, tx, Inf), include.lowest = TRUE))
  df <- data.frame(
    id = seq_len(n),
    wt = 1,
    o1 = factor(ord, levels = seq_len(k_ord), ordered = TRUE),
    cont = cont
  )
  list(
    df = df,
    design = as_survey(df, weights = wt),
    ord_int = ord,
    cont = cont
  )
}

make_latent_replicate <- function(
  type = "jk1",
  n = 200L,
  seed = 7L,
  n_psu = 20L,
  n_strata = 4L
) {
  set.seed(seed)
  df <- make_survey_data(
    n = n,
    n_psu = n_psu,
    n_strata = n_strata,
    design = "replicate",
    type = type,
    seed = seed
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  df$cont <- as.numeric(df$o1) + stats::rnorm(nrow(df), sd = 0.7)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = toupper(type)
  )
}


# =============================================================================
# Category 1 — Regression guard: method = "pearson" dispatch invariance
# (Task 1, 2). The spec's original "bit-identical numeric snapshot" framing
# is structurally impossible on a cross-BLAS CI matrix (macOS / Ubuntu /
# Windows produce 1-ULP divergence on otherwise-deterministic computations).
# We instead prove dispatch invariance — `method` omitted equals
# `method = "pearson"` to 1e-10 — and rely on pre-existing
# `test-analysis-corr.R` oracle tests for absolute Pearson correctness.
# =============================================================================

test_that("get_corr() explicit method = 'pearson' equals omitted method", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "taylor",
    seed = 1L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  r_default <- get_corr(d, x = c(y1, y2))
  r_explicit <- get_corr(d, x = c(y1, y2), method = "pearson")
  expect_equal(r_default$r, r_explicit$r, tolerance = 1e-10)
  expect_equal(r_default$ci_low, r_explicit$ci_low, tolerance = 1e-10)
  expect_equal(r_default$ci_high, r_explicit$ci_high, tolerance = 1e-10)
})

test_that("get_corr() method = 'nonsense' raises match.arg error", {
  df <- make_survey_data(n = 100L, seed = 2L)
  d <- as_survey(df, weights = wt)
  expect_error(get_corr(d, x = c(y1, y2), method = "spearman"))
})


# =============================================================================
# Category 2 — Polychoric happy path + oracle parity (Tasks 6-11)
# =============================================================================

test_that("get_corr() method = 'polychoric' matches polycor on 4x5 equal wt", {
  skip_if_not_installed("polycor")
  # Spec §Tolerances pins polycor::polychor() parity at 1e-6 on 3x3, 4x5,
  # and 2x2 equal-weight fixtures. PR 1's primitives test achieves 1e-6
  # at the .corr_polychoric_mle() layer with exactly the fixture below
  # (rho = -0.3, n = 800, k_x = 4, k_y = 5, seed = 2). The public-API
  # wrapper adds no new numerics, so the same tolerance holds here.
  fixt <- make_unit_weight_design(
    n = 800L,
    seed = 2L,
    n_levels_x = 4L,
    n_levels_y = 5L,
    rho = -0.3
  )
  r <- get_corr(fixt$design, x = c(o1, o2), method = "polychoric")
  oracle <- polycor::polychor(fixt$df$o1, fixt$df$o2)
  expect_equal(r$r[[1L]], oracle, tolerance = 1e-6)
  expect_identical(meta(r)$method, "polychoric")
})

test_that("get_corr() polychoric matches polycor on 3x3 equal weight", {
  skip_if_not_installed("polycor")
  # Matches PR 1's 3x3 primitive fixture (rho = 0.5, n = 500, seed = 1)
  # to reuse the 1e-6 strict-parity budget established there.
  fixt <- make_unit_weight_design(
    n = 500L,
    seed = 1L,
    n_levels_x = 3L,
    n_levels_y = 3L,
    rho = 0.5
  )
  r <- get_corr(fixt$design, x = c(o1, o2), method = "polychoric")
  oracle <- polycor::polychor(fixt$df$o1, fixt$df$o2)
  expect_equal(r$r[[1L]], oracle, tolerance = 1e-6)
})

test_that("get_corr() polychoric matches polycor on 2x2 equal weight", {
  skip_if_not_installed("polycor")
  # Matches PR 1's 2x2 primitive fixture (rho = 0.6, n = 1000, seed = 3)
  # at 1e-6 strict parity.
  fixt <- make_unit_weight_design(
    n = 1000L,
    seed = 3L,
    n_levels_x = 2L,
    n_levels_y = 2L,
    rho = 0.6
  )
  r <- get_corr(fixt$design, x = c(o1, o2), method = "polychoric")
  oracle <- polycor::polychor(fixt$df$o1, fixt$df$o2)
  expect_equal(r$r[[1L]], oracle, tolerance = 1e-6)
})

test_that("get_corr() method = 'polychoric' works on stratified survey_taylor", {
  d <- make_latent_taylor(n = 200L, seed = 11L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  expect_true(abs(r$r[[1L]]) <= 1)
  expect_true(r$ci_low[[1L]] >= -1)
  expect_true(r$ci_high[[1L]] <= 1)
  expect_lte(r$ci_low[[1L]], r$r[[1L]])
  expect_gte(r$ci_high[[1L]], r$r[[1L]])
})

test_that("get_corr() method = 'polychoric' on JK1 replicate matches taylor", {
  # Use the same underlying data for both designs so the MLE rho is equal.
  set.seed(20L)
  df <- make_survey_data(
    n = 160L,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 20L
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  d_tay <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  r_rep <- get_corr(d_rep, x = c(o1, o2), method = "polychoric")
  r_tay <- get_corr(d_tay, x = c(o1, o2), method = "polychoric")
  expect_equal(r_rep$r[[1L]], r_tay$r[[1L]], tolerance = 1e-6)
})

test_that("get_corr() method = 'polychoric' on BRR replicate produces se > 0", {
  d <- make_latent_replicate(type = "brr", n = 160L, seed = 22L)
  r <- get_corr(
    d,
    x = c(o1, o2),
    method = "polychoric",
    variance = c("se", "ci")
  )
  expect_true(is.finite(r$r[[1L]]))
  expect_true(r$se[[1L]] > 0)
})


# =============================================================================
# Category 3 — Polyserial happy path + oracle parity (Tasks 12-13)
# =============================================================================

test_that("get_corr() method = 'polyserial' matches hand two-step on 3-level fixture", {
  # decisions.md B1 established .hand_polyserial_twostep() as the strict
  # oracle for surveycore's Cox (1974) two-step polyserial MLE, pinned at
  # 1e-6. PR 1 achieves 1e-6 at the primitive layer; the public API
  # wraps the same .corr_polyserial_mle() call and so inherits the
  # tolerance. polycor::polyserial(ML = TRUE) is a joint MLE and is
  # mathematically inappropriate as an oracle regardless of tolerance.
  fixt <- make_polyserial_fixture(rho = 0.5, n = 500L, k_ord = 3L, seed = 21L)
  r <- get_corr(fixt$design, x = c(o1, cont), method = "polyserial")
  hand <- .hand_polyserial_twostep(fixt$ord_int, fixt$cont)
  expect_equal(r$r[[1L]], hand, tolerance = 1e-6)
  expect_identical(meta(r)$method, "polyserial")
})

test_that("get_corr() method = 'polyserial' matches hand two-step on 5-level fixture", {
  fixt <- make_polyserial_fixture(
    rho = -0.4,
    n = 600L,
    k_ord = 5L,
    seed = 22L
  )
  r <- get_corr(fixt$design, x = c(o1, cont), method = "polyserial")
  hand <- .hand_polyserial_twostep(fixt$ord_int, fixt$cont)
  expect_equal(r$r[[1L]], hand, tolerance = 1e-6)
})

test_that("get_corr() method = 'polyserial' matches hand two-step on 2-level fixture", {
  fixt <- make_polyserial_fixture(
    rho = 0.3,
    n = 800L,
    k_ord = 2L,
    seed = 23L
  )
  r <- get_corr(fixt$design, x = c(o1, cont), method = "polyserial")
  hand <- .hand_polyserial_twostep(fixt$ord_int, fixt$cont)
  expect_equal(r$r[[1L]], hand, tolerance = 1e-6)
})

test_that("get_corr() method = 'polyserial' works on JK1 replicate", {
  d <- make_latent_replicate(type = "jk1", n = 160L, seed = 32L)
  r <- get_corr(d, x = c(o1, cont), method = "polyserial")
  expect_true(abs(r$r[[1L]]) <= 1)
  expect_true(r$ci_low[[1L]] >= -1)
  expect_true(r$ci_high[[1L]] <= 1)
})


# =============================================================================
# Category 4 — Multi-pair long format, redundant, diagonal, wide (Tasks 15-18)
# =============================================================================

test_that("multi-pair polychoric returns 3 rows for 3 ordinal columns", {
  d <- make_latent_taylor(n = 200L, seed = 40L)
  r <- get_corr(d, x = c(o1, o2, o3), method = "polychoric")
  expect_identical(nrow(r), 3L)
  expect_identical(
    paste0(as.character(r$var1), "-", as.character(r$var2)),
    c("o1-o2", "o1-o3", "o2-o3")
  )
  # Each pair's r should be in [-1, 1].
  expect_true(all(r$r >= -1 & r$r <= 1))
})

test_that("polychoric + redundant = TRUE yields symmetric pairs", {
  d <- make_latent_taylor(n = 200L, seed = 41L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric", redundant = TRUE)
  expect_identical(nrow(r), 2L)
  expect_equal(r$r[[1L]], r$r[[2L]], tolerance = 1e-10)
  expect_identical(as.character(r$var1[[1L]]), as.character(r$var2[[2L]]))
  expect_identical(as.character(r$var1[[2L]]), as.character(r$var2[[1L]]))
})

test_that("polychoric + diagonal = TRUE includes self-rows with r = 1", {
  d <- make_latent_taylor(n = 200L, seed = 42L)
  r <- get_corr(
    d,
    x = c(o1, o2),
    method = "polychoric",
    diagonal = TRUE,
    variance = c("se", "ci")
  )
  expect_identical(nrow(r), 3L)
  self_rows <- r[as.character(r$var1) == as.character(r$var2), ]
  # Strip column-level `label` attributes before comparing values.
  expect_equal(unname(self_rows$r), c(1, 1), ignore_attr = TRUE)
  expect_equal(unname(self_rows$se), c(0, 0), ignore_attr = TRUE)
})

test_that("polychoric + format = 'wide' returns square matrix with NA diag", {
  d <- make_latent_taylor(n = 200L, seed = 43L)
  r <- get_corr(d, x = c(o1, o2, o3), method = "polychoric", format = "wide")
  expect_identical(nrow(r), 3L)
  expect_true(all(c("o1", "o2", "o3") %in% names(r)))
  expect_false("se" %in% names(r))
  expect_false("ci_low" %in% names(r))
  # Diagonal is NA.
  diag_idx <- match(as.character(r$variable), c("o1", "o2", "o3"))
  for (i in seq_len(nrow(r))) {
    expect_true(is.na(r[[i + 1L]][[i]]))
  }
})


# =============================================================================
# Category 5 — Grouping (Task 19)
# =============================================================================

test_that("polychoric + group = 2-level factor produces 2 rows", {
  fixt <- make_unit_weight_design(n = 200L, seed = 50L)
  r <- get_corr(fixt$design, x = c(o1, o2), method = "polychoric", group = grp)
  expect_identical(nrow(r), 2L)
  expect_true("grp" %in% names(r))
  expect_setequal(as.character(r$grp), c("A", "B"))
})


# =============================================================================
# Category 6 — Variance column selection (Task 20)
# =============================================================================

test_that("variance = 'se' emits only se; no ci_low/ci_high", {
  d <- make_latent_taylor(n = 200L, seed = 60L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric", variance = "se")
  expect_true("se" %in% names(r))
  expect_false("ci_low" %in% names(r))
  expect_false("moe" %in% names(r))
})

test_that("variance = 'ci' emits ci_low / ci_high", {
  d <- make_latent_taylor(n = 200L, seed = 61L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric", variance = "ci")
  expect_true("ci_low" %in% names(r))
  expect_true("ci_high" %in% names(r))
  expect_false("se" %in% names(r))
})

test_that("variance = c('se', 'ci', 'moe') emits all three; moe = (hi-lo)/2", {
  d <- make_latent_taylor(n = 200L, seed = 62L)
  r <- get_corr(
    d,
    x = c(o1, o2),
    method = "polychoric",
    variance = c("se", "ci", "moe")
  )
  expect_true(all(c("se", "ci_low", "ci_high", "moe") %in% names(r)))
  expected_moe <- (r$ci_high - r$ci_low) / 2
  expect_equal(r$moe, expected_moe, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("variance = 'deff' emits deff only", {
  d <- make_latent_taylor(n = 200L, seed = 63L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric", variance = "deff")
  expect_true("deff" %in% names(r))
  expect_false("se" %in% names(r))
  expect_true(is.finite(r$deff[[1L]]) || is.na(r$deff[[1L]]))
})

test_that("variance = NULL emits no variance-derived columns", {
  d <- make_latent_taylor(n = 200L, seed = 64L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric", variance = NULL)
  expect_false("se" %in% names(r))
  expect_false("ci_low" %in% names(r))
  expect_false("moe" %in% names(r))
  expect_false("deff" %in% names(r))
})


# =============================================================================
# Category 7 — df / statistic / p_value method awareness (Task 21, 22)
# =============================================================================

test_that("non-Pearson emits df = NA_integer_ and z-scale statistic", {
  d <- make_latent_taylor(n = 200L, seed = 70L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  expect_identical(r$df[[1L]], NA_integer_)
  expect_true(is.finite(r$statistic[[1L]]))
  # p_value consistent with two-sided standard normal reference.
  z_ref <- 2 * stats::pnorm(-abs(r$statistic[[1L]]))
  expect_equal(r$p_value[[1L]], z_ref, tolerance = 1e-10)
})

test_that("Pearson retains df = n - 2 and t-scale statistic", {
  d <- make_latent_taylor(n = 200L, seed = 71L)
  r <- get_corr(d, x = c(y1, y2), method = "pearson")
  expect_identical(r$df[[1L]], r$n[[1L]] - 2L)
  expect_true(is.finite(r$statistic[[1L]]))
})

test_that("column label attributes are method-neutral strings", {
  d <- make_latent_taylor(n = 200L, seed = 72L)
  r_poly <- get_corr(d, x = c(o1, o2), method = "polychoric")
  r_pear <- get_corr(d, x = c(y1, y2), method = "pearson")
  expect_identical(attr(r_poly$statistic, "label"), "statistic")
  expect_identical(attr(r_pear$statistic, "label"), "statistic")
  expect_identical(attr(r_poly$df, "label"), "df")
  expect_identical(attr(r_poly$p_value, "label"), "p-value")
})


# =============================================================================
# Category 8 — Edge cases (Tasks 24-31)
# =============================================================================

test_that("empty active domain yields r = NA and n = 0 without aborting", {
  skip_if_not_installed("surveytidy")
  d <- make_latent_taylor(n = 120L, seed = 80L)
  # Construct a domain that is empty by filtering on a condition with no
  # rows. surveytidy::filter() emits an informational warning that the
  # domain is empty; we suppress it here for test-output hygiene.
  d2 <- suppressWarnings(
    surveytidy::filter(d, as.integer(o1) > 999L)
  )
  # After PR 2 the .corr_latent_pair() dispatcher short-circuits to
  # r = NA_real_, n = 0 when the pair has 0 active-complete rows.
  r <- suppressWarnings(
    get_corr(d2, x = c(o1, o2), method = "polychoric")
  )
  expect_identical(r$n[[1L]], 0L)
  expect_identical(unname(r$r[[1L]]), NA_real_)
})

test_that("single-level ordinal emits PC-4", {
  set.seed(81L)
  df <- data.frame(
    id = 1:100,
    wt = 1,
    o1 = factor(rep(1L, 100L), levels = 1:3, ordered = TRUE),
    o2 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_error_polychoric_single_level_ordinal"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(o1, o2), method = "polychoric")
  )
})

test_that("all-NA focal column routes to empty-pair handling", {
  set.seed(82L)
  df <- data.frame(
    id = 1:100,
    wt = 1,
    o1 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE),
    o2 = factor(rep(NA_integer_, 100L), levels = 1:3, ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  expect_identical(r$n[[1L]], 0L)
  expect_identical(r$r[[1L]], NA_real_)
})

test_that("tiny-weight rows match physical-removal equivalent at 1e-3", {
  skip_if_not_installed("polycor")
  set.seed(83L)
  n <- 300L
  o1 <- sample(1:4, n, replace = TRUE)
  o2 <- o1 + sample(-1:1, n, replace = TRUE)
  o2[o2 < 1L] <- 1L
  o2[o2 > 4L] <- 4L
  keep <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.8, 0.2))
  # surveycore rejects non-positive weights in as_survey(); use a tiny
  # positive weight (1e-10) to preserve the row count while effectively
  # zeroing the row's contribution to the weighted likelihood.
  df_zw <- data.frame(
    id = 1:n,
    wt = ifelse(keep, 1, 1e-10),
    o1 = factor(o1, levels = 1:4, ordered = TRUE),
    o2 = factor(o2, levels = 1:4, ordered = TRUE)
  )
  d_zw <- as_survey(df_zw, weights = wt)
  r_zw <- get_corr(d_zw, x = c(o1, o2), method = "polychoric")
  # Compare against polychor on the physically subset data.
  df_sub <- df_zw[keep, , drop = FALSE]
  oracle <- polycor::polychor(df_sub$o1, df_sub$o2)
  expect_equal(r_zw$r[[1L]], oracle, tolerance = 1e-3)
})

test_that("integer vector with 3 distinct values is accepted for polychoric", {
  set.seed(84L)
  df <- data.frame(
    id = 1:150,
    wt = 1,
    x1 = sample(1:3, 150L, replace = TRUE),
    x2 = sample(1:3, 150L, replace = TRUE)
  )
  df$x1 <- as.integer(df$x1)
  df$x2 <- as.integer(df$x2)
  d <- as_survey(df, weights = wt)
  expect_no_warning(
    r <- get_corr(d, x = c(x1, x2), method = "polychoric")
  )
  expect_true(abs(r$r[[1L]]) <= 1)
})

test_that("integer with 15 distinct values + polyserial raises PC-3", {
  set.seed(85L)
  df <- data.frame(
    id = 1:200,
    wt = 1,
    x1 = as.integer(sample(1:20, 200L, replace = TRUE)),
    cont = rnorm(200L)
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = c(x1, cont), method = "polyserial"),
    class = "surveycore_error_polyserial_canonicalization_ambiguous"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(x1, cont), method = "polyserial")
  )
})

test_that("character column + method = 'polychoric' raises PC-1", {
  set.seed(86L)
  df <- data.frame(
    id = 1:100,
    wt = 1,
    o1 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE),
    s1 = sample(c("a", "b", "c"), 100L, replace = TRUE),
    stringsAsFactors = FALSE
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = c(o1, s1), method = "polychoric"),
    class = "surveycore_error_polychoric_requires_ordinal"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(o1, s1), method = "polychoric")
  )
})

test_that("6x7 ordinal pair runs without error", {
  set.seed(87L)
  n <- 300L
  df <- data.frame(
    id = 1:n,
    wt = 1,
    o1 = factor(sample(1:6, n, replace = TRUE), levels = 1:6, ordered = TRUE),
    o2 = factor(sample(1:7, n, replace = TRUE), levels = 1:7, ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_no_error({
    r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  })
  expect_true(is.finite(r$r[[1L]]))
})

test_that("surveytidy::filter domain matches raw-subset equivalent within 1e-4", {
  skip_if_not_installed("surveytidy")
  skip_if_not_installed("polycor")
  fixt <- make_unit_weight_design(n = 300L, seed = 88L)
  d_full <- fixt$design
  df <- fixt$df
  d_filt <- surveytidy::filter(d_full, grp == "A")
  r_filt <- get_corr(d_filt, x = c(o1, o2), method = "polychoric")
  df_sub <- df[df$grp == "A", , drop = FALSE]
  oracle <- polycor::polychor(df_sub$o1, df_sub$o2)
  # Agreement with polycor on equal-weight data is bounded by the
  # shared optimizer tolerance (~1e-4). Domain correctness is the gate;
  # optimizer precision is not.
  expect_equal(r_filt$r[[1L]], oracle, tolerance = 1e-4)
})


# =============================================================================
# Category 9 — Survey collection dispatch (Tasks 33-35)
# =============================================================================

test_that("survey_collection of all taylor members dispatches polychoric per-survey", {
  d1 <- make_latent_taylor(n = 120L, seed = 100L)
  d2 <- make_latent_taylor(n = 120L, seed = 101L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2)
  r <- get_corr(coll, x = c(o1, o2), method = "polychoric")
  expect_true(".survey" %in% names(r))
  expect_setequal(as.character(r$.survey), c("wave1", "wave2"))
})

.make_twophase_design <- function(seed) {
  df_tp <- make_survey_data(
    n = 120L,
    n_psu = 12L,
    n_strata = 3L,
    design = "twophase",
    seed = seed
  )
  df_tp$o1 <- factor(
    sample(1:4, nrow(df_tp), replace = TRUE),
    ordered = TRUE
  )
  df_tp$o2 <- factor(
    sample(1:4, nrow(df_tp), replace = TRUE),
    ordered = TRUE
  )
  ph1 <- as_survey(
    df_tp,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE
  )
  as_survey_twophase(ph1, subset = subset, method = "approx")
}

test_that("survey_collection with twophase member + polychoric raises PC-7 (dual)", {
  d_taylor <- make_latent_taylor(n = 120L, seed = 110L)
  d_tp <- .make_twophase_design(110L)
  coll <- as_survey_collection(w1 = d_taylor, w2 = d_tp)
  expect_error(
    get_corr(coll, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(coll, x = c(o1, o2), method = "polychoric")
  )
})

test_that("survey_collection with twophase + .if_missing_var = 'skip' still raises PC-7", {
  # Note: the collection's `.if_missing_var` hook catches
  # surveycore_error_variable_not_found only. PC-7 is a distinct class and
  # propagates through the dispatcher to the caller regardless of
  # `.if_missing_var`. (This matches the `.dispatch_over_collection()`
  # contract in R/survey-collection.R; altering it is out of scope for
  # PR 3.)
  d_taylor <- make_latent_taylor(n = 120L, seed = 120L)
  d_tp <- .make_twophase_design(120L)
  coll <- as_survey_collection(w1 = d_taylor, w2 = d_tp)
  expect_error(
    get_corr(
      coll,
      x = c(o1, o2),
      method = "polychoric",
      .if_missing_var = "skip"
    ),
    class = "surveycore_error_polychoric_design_unsupported"
  )
})


# =============================================================================
# Category 10 — Pearson regression: warning + error classes still fire (Task 36)
# =============================================================================

test_that("method = 'pearson' still warns on non-numeric columns (regression guard)", {
  set.seed(130L)
  df <- data.frame(
    id = 1:100,
    wt = 1,
    y1 = rnorm(100L),
    y2 = rnorm(100L),
    grp = sample(c("A", "B"), 100L, replace = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_warning(
    get_corr(d, x = c(y1, y2, grp), method = "pearson"),
    class = "surveycore_warning_corr_non_numeric"
  )
})

test_that("surveycore_error_insufficient_variables fires for < 2 columns under latent", {
  set.seed(131L)
  df <- data.frame(
    id = 1:100,
    wt = 1,
    o1 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = o1, method = "polychoric"),
    class = "surveycore_error_insufficient_variables"
  )
})

test_that("surveycore_error_invalid_variance_arg still fires under latent methods", {
  d <- make_latent_taylor(n = 120L, seed = 132L)
  expect_error(
    get_corr(d, x = c(o1, o2), method = "polychoric", variance = "bogus"),
    class = "surveycore_error_invalid_variance_arg"
  )
})


# =============================================================================
# Category 11 — meta() population (Tasks 37-38)
# =============================================================================

test_that("meta()$bivariate_normal_cdf is 'pbivnorm' for polychoric", {
  d <- make_latent_taylor(n = 120L, seed = 140L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  expect_identical(meta(r)$bivariate_normal_cdf, "pbivnorm")
})

test_that("meta()$bivariate_normal_cdf is 'pbivnorm' for polyserial", {
  d <- make_latent_taylor(n = 120L, seed = 141L)
  r <- get_corr(d, x = c(o1, cont), method = "polyserial")
  expect_identical(meta(r)$bivariate_normal_cdf, "pbivnorm")
})

test_that("meta()$bivariate_normal_cdf is absent for pearson", {
  d <- make_latent_taylor(n = 120L, seed = 142L)
  r <- get_corr(d, x = c(y1, y2), method = "pearson")
  expect_null(meta(r)$bivariate_normal_cdf)
})

test_that("meta()$n_failed_replicates_total absent when n_failed == 0", {
  d <- make_latent_replicate(type = "jk1", n = 160L, seed = 143L)
  r <- get_corr(d, x = c(o1, o2), method = "polychoric")
  expect_null(meta(r)$n_failed_replicates_total)
})


# =============================================================================
# Category 12 — Public-API dual-pattern tests for PC warnings (PC-9, PC-13)
# =============================================================================

test_that("PC-13 (unordered factor) surfaces via the public API (dual)", {
  set.seed(150L)
  df <- data.frame(
    id = 1:200,
    wt = 1,
    o1 = factor(
      sample(c("a", "b", "c"), 200L, replace = TRUE),
      ordered = FALSE
    ),
    o2 = factor(sample(1:3, 200L, replace = TRUE), ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_warning(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_warning_polychoric_unordered_factor"
  )
  # Snapshot the PC-13 condition text. Result is numeric and auto-prints,
  # so wrap in invisible() for Windows-BLAS stability.
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_unordered_factor = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})


# =============================================================================
# Category 14 — Public-API dual-pattern tests for remaining PC classes
# =============================================================================
# PC-1, PC-3, PC-4 already covered in Category 8. PC-13 above. This category
# covers PC-2, PC-5, PC-6, PC-7 (collection path above covers one flavour),
# PC-8, PC-9, PC-10, PC-11, PC-12, PC-14. Fixtures reuse PR 2's variance-layer
# constructions where possible; PC-6 and PC-11 are structurally unreachable
# at the public API with realistic data (PC-11 is marked # nocov in
# R/analysis-corr-latent.R; PC-6 arises only when the numerical-IF
# perturbation makes the MLE degenerate, which PR 2 exercises by directly
# invoking .corr_numerical_influence on a bogus rho_hat_full). Those two use
# `testthat::local_mocked_bindings()` to force the class to fire under
# `get_corr()` so the public-API surface is exercised end-to-end.

test_that("PC-2 (polyserial with two ordered factors) at public API (dual)", {
  set.seed(200L)
  df <- data.frame(
    id = 1:100L,
    wt = 1,
    o1 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE),
    o2 = factor(sample(1:3, 100L, replace = TRUE), ordered = TRUE)
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = c(o1, o2), method = "polyserial"),
    class = "surveycore_error_polyserial_requires_mixed_types"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(o1, o2), method = "polyserial")
  )
})

test_that("PC-5 (polychoric insufficient cells) at public API (dual)", {
  # 3 nonempty cells of a 2x2 table — below the MLE-identification floor.
  df <- data.frame(
    id = 1:25L,
    wt = 1,
    o1 = factor(
      c(rep(1L, 10), rep(2L, 10), rep(1L, 5)),
      levels = 1:2,
      ordered = TRUE
    ),
    o2 = factor(
      c(rep(1L, 10), rep(2L, 10), rep(2L, 5)),
      levels = 1:2,
      ordered = TRUE
    )
  )
  d <- as_survey(df, weights = wt)
  expect_error(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_error_polychoric_insufficient_cells"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(o1, o2), method = "polychoric")
  )
})

test_that("PC-6 (polychoric optim_failed) at public API (dual, mocked)", {
  # PC-6 fires when a perturbation inside .corr_numerical_influence() makes
  # the pair-level MLE degenerate. Not reachable at public API with realistic
  # data (see PR 2 decisions.md note). Mock .corr_polychoric_mle to raise
  # PC-6 directly on its first call; get_corr() should surface the class.
  d <- make_latent_taylor(n = 80L, seed = 201L)
  testthat::local_mocked_bindings(
    .corr_polychoric_mle = function(...) {
      cli::cli_abort(
        c(
          "x" = "Numerical optimization did not converge for pair (o1, o2).",
          "i" = "Optimizer message: simulated failure.",
          "v" = "Inspect the pair for extreme weight skew or sparse cells."
        ),
        class = "surveycore_error_polychoric_optim_failed"
      )
    }
  )
  expect_error(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_error_polychoric_optim_failed"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(o1, o2), method = "polychoric")
  )
})

test_that("PC-7 (twophase, single design) at public API (dual)", {
  # Single-design PC-7 path (the collection path is covered above).
  d_tp <- .make_twophase_design(202L)
  expect_error(
    get_corr(d_tp, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d_tp, x = c(o1, o2), method = "polychoric")
  )
})

test_that("PC-8 (> 20% replicate failure) at public API (dual)", {
  # Zero out every replicate except the last, producing ~R-1 failures.
  # Pattern mirrors PR 2's test-analysis-corr-latent-variance.R PC-8 fixture.
  set.seed(203L)
  df <- make_survey_data(
    n = 60L,
    n_psu = 30L,
    n_strata = 5L,
    design = "replicate",
    type = "jk1",
    seed = 203L
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  keep_rows <- which(
    as.integer(df$o1) == 1L & as.integer(df$o2) == 1L
  )
  if (length(keep_rows) < 2L) {
    skip("fixture does not produce the required degenerate replicates")
  }
  for (cn in rep_cols[-length(rep_cols)]) {
    df[[cn]] <- 0
    df[[cn]][keep_rows] <- df$wt[keep_rows]
  }
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  expect_error(
    suppressWarnings(get_corr(d, x = c(o1, o2), method = "polychoric")),
    class = "surveycore_error_replicate_convergence_failure"
  )
  expect_snapshot(
    error = TRUE,
    suppressWarnings(get_corr(d, x = c(o1, o2), method = "polychoric"))
  )
})

test_that("PC-9 (near-boundary rho, replicate path) at public API (dual)", {
  # 2x2 fixture saturating at the optimizer upper bound, on a replicate
  # design so PC-14 (Taylor-only) doesn't also fire. Pattern mirrors PR 2's
  # test-analysis-corr-latent-variance.R PC-9 fixture.
  set.seed(1L)
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(
    n = n,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 1L
  )
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  # Primary dual assertion: PC-9 fires. PC-10 also fires because the
  # boundary fixture's (1,3) and (3,1) cells are empty in the active
  # domain; muffle it so expect_warning() resolves on PC-9.
  expect_warning(
    withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_zero_count_level = function(w) {
        invokeRestart("muffleWarning")
      }
    ),
    class = "surveycore_warning_polychoric_boundary_rho"
  )
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_boundary_rho = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})

test_that("PC-10 (zero-count interior level) at public API (dual)", {
  # 4 levels with level 3 holding zero observations in the domain. Pattern
  # mirrors PR 2's test-analysis-corr-latent-variance.R PC-10 fixture.
  set.seed(204L)
  n <- 100L
  codes <- sample(c(1L, 2L, 4L), n, replace = TRUE)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 204L)
  df$o1 <- factor(codes, levels = 1:4, ordered = TRUE)
  df$o2 <- factor(sample(1:3, n, replace = TRUE), ordered = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_warning_polychoric_zero_count_level"
  )
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_zero_count_level = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})

test_that("PC-11 (polychoric sparse cell) at public API (dual, mocked)", {
  # PC-11 is unreachable at the public API with realistic data (marked
  # `# nocov` in R/analysis-corr-latent.R). Mock .corr_polychoric_mle to
  # return a fit object with n_sparse_cells > 0 and let .corr_latent_pair()
  # raise PC-11 through its normal code path.
  d <- make_latent_taylor(n = 80L, seed = 205L)
  # Compute the real fit first so the variance path has a valid rho, then
  # wrap to inject n_sparse_cells.
  real_mle <- surveycore:::.corr_polychoric_mle
  testthat::local_mocked_bindings(
    .corr_polychoric_mle = function(...) {
      out <- real_mle(...)
      out$n_sparse_cells <- 1L
      out
    }
  )
  expect_warning(
    withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_zero_count_level = function(w) {
        invokeRestart("muffleWarning")
      }
    ),
    class = "surveycore_warning_polychoric_sparse_cell"
  )
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_sparse_cell = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})

test_that("PC-12 at public API populates meta()$n_failed_replicates_total", {
  # Partial replicate failure (1 of R replicates fails). Pattern mirrors
  # PR 2's test-analysis-corr-latent-variance.R PC-12 fixture. Assertions:
  #   * PC-12 warning class fires.
  #   * meta(result)$n_failed_replicates_total equals the planned failure
  #     count (1L) — exercises the populated-scalar branch of the accumulator
  #     that Invariant 6 requires and spec test-spec.md PC-12 mandates.
  #   * meta(result)$bivariate_normal_cdf == "pbivnorm".
  # This test also satisfies Fix D coverage on R/analysis-corr.R.
  set.seed(13L)
  df <- make_survey_data(
    n = 60L,
    n_psu = 30L,
    n_strata = 5L,
    design = "replicate",
    type = "jk1",
    seed = 13L
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  keep_rows <- which(
    as.integer(df$o1) == 1L & as.integer(df$o2) == 1L
  )
  if (length(keep_rows) < 2L) {
    skip("fixture does not produce the required degenerate replicate")
  }
  # Break exactly 1 replicate column — produces k = 1 planned failure.
  df[[rep_cols[[1L]]]] <- 0
  df[[rep_cols[[1L]]]][keep_rows] <- df$wt[keep_rows]
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  expect_warning(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    class = "surveycore_warning_polychoric_replicate_convergence"
  )
  # Populated-scalar path: meta$n_failed_replicates_total = 1L.
  r <- suppressWarnings(
    get_corr(d, x = c(o1, o2), method = "polychoric")
  )
  expect_identical(meta(r)$n_failed_replicates_total, 1L)
  expect_identical(meta(r)$bivariate_normal_cdf, "pbivnorm")
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_replicate_convergence = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})

test_that("PC-14 (Taylor near-boundary wide CI) at public API (dual)", {
  # Same 2x2 boundary fixture as PC-9, but on a Taylor design so PC-14
  # fires in addition to PC-9. Pattern mirrors PR 2's PC-14 fixture.
  set.seed(2L)
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 2L)
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_boundary_rho = function(w) {
        invokeRestart("muffleWarning")
      },
      surveycore_warning_polychoric_zero_count_level = function(w) {
        invokeRestart("muffleWarning")
      }
    ),
    class = "surveycore_warning_polychoric_taylor_boundary_wide_ci"
  )
  expect_snapshot({
    invisible(withCallingHandlers(
      get_corr(d, x = c(o1, o2), method = "polychoric"),
      surveycore_warning_polychoric_taylor_boundary_wide_ci = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})


# =============================================================================
# Category 13 — name_style = "broom" threads statistic / df / p.value labels
# =============================================================================

test_that("name_style = 'broom' produces broom columns with neutral labels", {
  d <- make_latent_taylor(n = 200L, seed = 160L)
  r <- get_corr(
    d,
    x = c(o1, o2),
    method = "polychoric",
    variance = c("se", "ci"),
    name_style = "broom"
  )
  expect_true("estimate" %in% names(r))
  expect_true("std.error" %in% names(r))
  expect_true("conf.low" %in% names(r))
  expect_true("conf.high" %in% names(r))
  expect_identical(attr(r$statistic, "label"), "statistic")
})


# ── B3: PC-9 boundary warning threshold ──────────────────────────────────────

test_that("PC-9 boundary warning message contains '1e-4' not '1e-6'", {
  # Same near-boundary fixture as the PC-9 dual test above, but on a replicate
  # design to avoid PC-14 also firing.
  set.seed(1L)
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(
    n = n,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 1L
  )
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  # Capture the PC-9 warning text and verify threshold value
  captured_msg <- NULL
  withCallingHandlers(
    get_corr(d, x = c(o1, o2), method = "polychoric"),
    surveycore_warning_polychoric_boundary_rho = function(cond) {
      captured_msg <<- conditionMessage(cond)
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_true(grepl("1e-4", captured_msg, fixed = TRUE))
  expect_false(grepl("1e-6", captured_msg, fixed = TRUE))
})
