# Parity tests for the vendored saddlepoint CDF helpers
# (.pchisqsum_sad(), .pFsum_sad(), and the underlying .saddle() root-finder).
#
# These are byte-for-byte parity tests against survey 4.4-8 at absolute
# tolerance 1e-10. See spec §V.5 and §VII (parity table). The wrapper-level
# Satterthwaite fallback is out of scope here; the NA-regime parity test
# targets the root-finder directly per the impl plan PR B task 3.

skip_on_cran()

# ---------------------------------------------------------------------------
# .pchisqsum_sad() vs survey::pchisqsum(..., method = "saddlepoint")
# ---------------------------------------------------------------------------

test_that(".pchisqsum_sad() matches survey::pchisqsum() upper-tail regime", {
  skip_if_not_installed("survey")
  xs <- stats::qchisq(c(1 - 1e-6, 1 - 1e-3, 1 - 1e-2), df = 3)
  for (a in list(c(1, 1, 1), c(3, 1, 0.5))) {
    for (x in xs) {
      ours <- .pchisqsum_sad(x, df = c(1, 1, 1), a = a, lower.tail = FALSE)
      theirs <- survey::pchisqsum(
        x,
        df = c(1, 1, 1),
        a = a,
        method = "saddlepoint",
        lower.tail = FALSE
      )
      expect_equal(ours, theirs, tolerance = 1e-10)
    }
  }
})

test_that(".pchisqsum_sad() matches survey::pchisqsum() mid-range regime", {
  skip_if_not_installed("survey")
  xs <- stats::qchisq(c(0.25, 0.5, 0.75), df = 3)
  for (a in list(c(1, 1, 1), c(3, 1, 0.5))) {
    for (x in xs) {
      ours <- .pchisqsum_sad(x, df = c(1, 1, 1), a = a, lower.tail = FALSE)
      theirs <- survey::pchisqsum(
        x,
        df = c(1, 1, 1),
        a = a,
        method = "saddlepoint",
        lower.tail = FALSE
      )
      expect_equal(ours, theirs, tolerance = 1e-10)
    }
  }
})

test_that(".pchisqsum_sad() matches survey::pchisqsum() near-zero regime", {
  skip_if_not_installed("survey")
  a <- c(1, 1, 1)
  x <- 0.01 * sum(a)
  ours <- .pchisqsum_sad(x, df = c(1, 1, 1), a = a, lower.tail = FALSE)
  theirs <- survey::pchisqsum(
    x,
    df = c(1, 1, 1),
    a = a,
    method = "saddlepoint",
    lower.tail = FALSE
  )
  expect_equal(ours, theirs, tolerance = 1e-10)
})

test_that(".pchisqsum_sad() matches survey::pchisqsum() with scalar a", {
  skip_if_not_installed("survey")
  x <- stats::qchisq(0.95, df = 1) * 2
  ours <- .pchisqsum_sad(x, df = 1, a = 2, lower.tail = FALSE)
  theirs <- survey::pchisqsum(
    x,
    df = 1,
    a = 2,
    method = "saddlepoint",
    lower.tail = FALSE
  )
  expect_equal(ours, theirs, tolerance = 1e-10)
})

test_that(".pchisqsum_sad() matches survey::pchisqsum() large df regime", {
  skip_if_not_installed("survey")
  xs <- stats::qchisq(c(0.5, 0.99), df = 20)
  df <- rep(1L, 20)
  a <- rep(1, 20)
  for (x in xs) {
    ours <- .pchisqsum_sad(x, df = df, a = a, lower.tail = FALSE)
    theirs <- survey::pchisqsum(
      x,
      df = df,
      a = a,
      method = "saddlepoint",
      lower.tail = FALSE
    )
    expect_equal(ours, theirs, tolerance = 1e-10)
  }
})

# ---------------------------------------------------------------------------
# .pFsum_sad() vs survey::pFsum(..., method = "saddlepoint")
#
# Under the default mean.deff = mean(a), .pFsum_sad() is equivalent to upstream
# when called with x pre-divided by mean(a). See §V.5 (Pass 3 Issue 65).
# ---------------------------------------------------------------------------

test_that(".pFsum_sad() matches survey::pFsum() upper-tail regime", {
  skip_if_not_installed("survey")
  ddf <- 30
  xs <- stats::qchisq(c(1 - 1e-6, 1 - 1e-3, 1 - 1e-2), df = 3)
  for (a in list(c(1, 1, 1), c(3, 1, 0.5))) {
    for (x in xs) {
      ours <- .pFsum_sad(
        x / mean(a),
        df = c(1, 1, 1),
        a = a,
        ddf = ddf,
        lower.tail = FALSE
      )
      theirs <- survey::pFsum(
        x,
        df = c(1, 1, 1),
        a = a,
        ddf = ddf,
        method = "saddlepoint",
        lower.tail = FALSE
      )
      expect_equal(ours, theirs, tolerance = 1e-10)
    }
  }
})

test_that(".pFsum_sad() matches survey::pFsum() mid-range regime", {
  skip_if_not_installed("survey")
  ddf <- 30
  xs <- stats::qchisq(c(0.25, 0.5, 0.75), df = 3)
  for (a in list(c(1, 1, 1), c(3, 1, 0.5))) {
    for (x in xs) {
      ours <- .pFsum_sad(
        x / mean(a),
        df = c(1, 1, 1),
        a = a,
        ddf = ddf,
        lower.tail = FALSE
      )
      theirs <- survey::pFsum(
        x,
        df = c(1, 1, 1),
        a = a,
        ddf = ddf,
        method = "saddlepoint",
        lower.tail = FALSE
      )
      expect_equal(ours, theirs, tolerance = 1e-10)
    }
  }
})

test_that(".pFsum_sad() matches survey::pFsum() near-zero regime", {
  skip_if_not_installed("survey")
  ddf <- 30
  a <- c(1, 1, 1)
  x <- 0.01 * sum(a)
  ours <- .pFsum_sad(
    x / mean(a),
    df = c(1, 1, 1),
    a = a,
    ddf = ddf,
    lower.tail = FALSE
  )
  theirs <- survey::pFsum(
    x,
    df = c(1, 1, 1),
    a = a,
    ddf = ddf,
    method = "saddlepoint",
    lower.tail = FALSE
  )
  expect_equal(ours, theirs, tolerance = 1e-10)
})

test_that(".pFsum_sad() matches survey::pFsum() with scalar a", {
  skip_if_not_installed("survey")
  ddf <- 30
  a <- 2
  x <- stats::qchisq(0.95, df = 1) * 2
  ours <- .pFsum_sad(
    x / mean(a),
    df = 1,
    a = a,
    ddf = ddf,
    lower.tail = FALSE
  )
  theirs <- survey::pFsum(
    x,
    df = 1,
    a = a,
    ddf = ddf,
    method = "saddlepoint",
    lower.tail = FALSE
  )
  expect_equal(ours, theirs, tolerance = 1e-10)
})

test_that(".pFsum_sad() matches survey::pFsum() large df regime", {
  skip_if_not_installed("survey")
  ddf <- 30
  df <- rep(1L, 20)
  a <- rep(1, 20)
  xs <- stats::qchisq(c(0.5, 0.99), df = 20)
  for (x in xs) {
    ours <- .pFsum_sad(
      x / mean(a),
      df = df,
      a = a,
      ddf = ddf,
      lower.tail = FALSE
    )
    theirs <- survey::pFsum(
      x,
      df = df,
      a = a,
      ddf = ddf,
      method = "saddlepoint",
      lower.tail = FALSE
    )
    expect_equal(ours, theirs, tolerance = 1e-10)
  }
})

# ---------------------------------------------------------------------------
# NA-regime parity at the root-finder level.
#
# The root-finder NA regime is where uniroot() fails to converge or the
# abs(hatzeta) < 1e-4 guard fires — both yield NA. Assert .saddle() returns
# NA on the same inputs as survey:::saddle().
# ---------------------------------------------------------------------------

test_that(".saddle() root-finder returns NA on the same inputs as survey:::saddle()", {
  skip_if_not_installed("survey")
  upstream_saddle <- utils::getFromNamespace("saddle", "survey")

  # Trigger the abs(hatzeta) < 1e-4 guard: x at the mean sum(a) of the
  # weighted chi-square sum reduces hatzeta to (near) zero.
  a1 <- c(1, 1, 1)
  x1 <- sum(a1) # mean of the distribution: hatzeta ≈ 0
  ours1 <- .saddle(x1, a1)
  theirs1 <- upstream_saddle(x1, a1)
  expect_identical(is.na(ours1), is.na(theirs1))

  # Extreme eigenvalue heterogeneity + small x to stress the root-finder.
  # Both implementations produce a `NaNs produced` warning from sqrt() of a
  # negative value in the saddle-point algebra; we assert parity of the
  # NA regime, not warning suppression.
  a2 <- c(1e-6, 1, 1e6)
  x2 <- sum(a2)
  ours2 <- suppressWarnings(.saddle(x2, a2))
  theirs2 <- suppressWarnings(upstream_saddle(x2, a2))
  expect_identical(is.na(ours2), is.na(theirs2))
})
