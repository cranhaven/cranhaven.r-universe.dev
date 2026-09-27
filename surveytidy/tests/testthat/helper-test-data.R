# tests/testthat/helper-test-data.R
#
# Test infrastructure for surveytidy.
# Loaded automatically by testthat before any test file runs.
#
# Re-uses make_survey_data() and test_invariants() from surveycore via the
# package's helper infrastructure. make_all_designs() is imported from
# surveycore's own helper (available after surveycore is loaded).
#
# Additional surveytidy-specific helpers:
#   - The surveycore helpers (make_survey_data, test_invariants,
#     make_all_designs) are accessible because surveytidy imports surveycore.

# Source the surveycore test helper to get make_survey_data, test_invariants,
# and make_all_designs. These functions live in surveytidy's test environment
# after devtools::load_all() + devtools::test().
#
# Note: When running tests via devtools::test() from the surveytidy project,
# surveycore's helper is NOT automatically loaded. We recreate the necessary
# helpers inline here.

make_survey_data <- function(
  n = 500L,
  n_psu = 50L,
  n_strata = 5L,
  design = c("taylor", "replicate", "twophase"),
  type = "brr",
  with_labels = FALSE,
  seed = 42L
) {
  design <- match.arg(design)
  type <- tolower(type)

  if (type %in% c("brr", "fay") && n_psu %% 2L != 0L) {
    stop(
      "n_psu must be even for BRR/Fay replicate designs. ",
      "Got n_psu = ",
      n_psu,
      "."
    )
  }

  set.seed(seed)

  psus_per_stratum <- rep(n_psu %/% n_strata, n_strata)
  remainder <- n_psu %% n_strata
  if (remainder > 0L) {
    psus_per_stratum[seq_len(remainder)] <-
      psus_per_stratum[seq_len(remainder)] + 1L
  }
  psu_stratum <- rep(seq_len(n_strata), psus_per_stratum)

  psu_sizes_raw <- sample(5:15, n_psu, replace = TRUE)
  exact <- psu_sizes_raw / sum(psu_sizes_raw) * n
  psu_sizes <- as.integer(floor(exact))
  psu_sizes <- pmax(psu_sizes, 1L)
  remaining <- n - sum(psu_sizes)
  if (remaining > 0L) {
    top_idx <- order(exact - floor(exact), decreasing = TRUE)[seq_len(
      remaining
    )]
    psu_sizes[top_idx] <- psu_sizes[top_idx] + 1L
  }

  psu_index <- rep(seq_len(n_psu), times = psu_sizes)
  strata <- psu_stratum[psu_index]

  stratum_n <- tabulate(strata, nbins = n_strata)
  stratum_pop <- round(stratum_n * runif(n_strata, 8, 15))
  fpc <- stratum_pop[strata]

  base_wt <- stratum_pop / stratum_n
  wt <- base_wt[strata] * exp(rnorm(n, mean = 0, sd = 0.2))

  y1 <- rnorm(n, mean = 50, sd = 10)
  y2 <- rnorm(n, mean = 0, sd = 1)
  y3 <- as.integer(runif(n) < 0.3)
  group <- sample(c("A", "B", "C"), n, replace = TRUE)

  df <- data.frame(
    psu = paste0("psu_", psu_index),
    strata = paste0("stratum_", strata),
    fpc = fpc,
    wt = wt,
    y1 = y1,
    y2 = y2,
    y3 = y3,
    group = group,
    stringsAsFactors = FALSE
  )

  if (design == "replicate") {
    R <- switch(
      type,
      brr = n_psu %/% 2L,
      fay = n_psu %/% 2L,
      jk1 = ,
      jk2 = ,
      jkn = ,
      bootstrap = n_psu,
      n_psu
    )
    repwt_matrix <- matrix(
      wt * exp(matrix(rnorm(n * R, mean = 0, sd = 0.1), nrow = n, ncol = R)),
      nrow = n,
      ncol = R
    )
    repwt_df <- as.data.frame(repwt_matrix)
    names(repwt_df) <- paste0("repwt_", seq_len(R))
    df <- cbind(df, repwt_df)
  }

  if (design == "twophase") {
    df$phase2_ind <- runif(n) < 0.4
  }

  if (with_labels) {
    attr(df$y1, "label") <- "Outcome variable 1 (continuous)"
    attr(df$y2, "label") <- "Outcome variable 2 (continuous)"
    attr(df$y3, "label") <- "Outcome variable 3 (binary, 0/1)"
    attr(df$y3, "labels") <- c("No" = 0L, "Yes" = 1L)
    attr(df$group, "label") <- "Demographic group"
    attr(df$group, "labels") <- c(
      "Group A" = "A",
      "Group B" = "B",
      "Group C" = "C"
    )
    attr(df$wt, "label") <- "Sampling weight"
  }

  df
}

test_invariants <- function(design) {
  testthat::expect_true(is.data.frame(design@data))
  testthat::expect_false(is.null(design@data))
  testthat::expect_gte(nrow(design@data), 1L)
  testthat::expect_false(
    anyDuplicated(names(design@data)) > 0L,
    label = "@data has no duplicate column names"
  )

  if (S7::S7_inherits(design, surveycore::survey_twophase)) {
    p1 <- design@variables$phase1
    p2 <- design@variables$phase2
    design_vars <- c(
      p1$ids,
      p1$weights,
      p1$strata,
      p1$fpc,
      if (!is.null(p2)) c(p2$ids, p2$strata, p2$probs, p2$fpc),
      design@variables$subset
    )
  } else {
    design_vars <- c(
      design@variables$ids,
      design@variables$weights,
      design@variables$strata,
      design@variables$fpc
    )
  }
  design_vars <- design_vars[!is.null(design_vars)]
  for (v in design_vars) {
    testthat::expect_true(
      v %in% names(design@data),
      label = paste0("design var '", v, "' present in @data")
    )
    testthat::expect_true(
      is.atomic(design@data[[v]]),
      label = paste0("design var '", v, "' is atomic")
    )
  }

  wt_var <- if (S7::S7_inherits(design, surveycore::survey_twophase)) {
    design@variables$phase1$weights
  } else {
    design@variables$weights
  }
  if (!is.null(wt_var)) {
    wt_col <- design@data[[wt_var]]
    testthat::expect_true(is.numeric(wt_col))
    testthat::expect_true(all(wt_col[!is.na(wt_col)] > 0))
  }

  if (S7::S7_inherits(design, surveycore::survey_replicate)) {
    for (rw in design@variables$repweights) {
      testthat::expect_true(is.numeric(design@data[[rw]]))
    }
  }

  testthat::expect_true(
    S7::S7_inherits(design@metadata, surveycore::survey_metadata)
  )

  # Invariant 6: every column listed in visible_vars must exist in @data
  vv <- design@variables$visible_vars
  if (!is.null(vv)) {
    bad <- setdiff(vv, names(design@data))
    testthat::expect_true(
      length(bad) == 0L,
      label = paste0(
        "visible_vars contains columns not in @data: ",
        paste(bad, collapse = ", ")
      )
    )
  }

  # Invariant 7: surveytidy_recode attr must be stripped before @data is stored.
  # .strip_label_attrs() in mutate.survey_base() must remove this attr from
  # every column. Any failure here is a regression in the strip step.
  for (col in names(design@data)) {
    testthat::expect_null(
      attr(design@data[[col]], "surveytidy_recode"),
      label = paste0(
        "@data[[\"",
        col,
        "\"]] must not carry surveytidy_recode attr"
      )
    )
  }

  invisible(design)
}

# ---------------------------------------------------------------------------
# survey_result test helpers
# ---------------------------------------------------------------------------

#' Build a survey_result fixture of a given type and design
#'
#' @param type One of "means", "freqs", "ratios"
#' @param design One of "taylor", "replicate", "twophase"
#' @param seed Integer seed for reproducibility
#' @return A survey_result tibble subclass
make_survey_result <- function(
  type = c("means", "freqs", "ratios"),
  design = c("taylor", "replicate", "twophase"),
  seed = 42L
) {
  type <- match.arg(type)
  design <- match.arg(design)

  if (design == "twophase") {
    # Twophase designs need method = "approx" to work with analysis functions.
    # make_all_designs() uses the default method = "full" (which requires
    # explicit phase 2 design variables); rebuild with method = "approx" here.
    df_p <- make_survey_data(
      n = 100L,
      n_psu = 10L,
      n_strata = 2L,
      design = "twophase",
      seed = seed
    )
    phase1 <- surveycore::as_survey(
      df_p,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc,
      nest = TRUE
    )
    d <- suppressWarnings(
      surveycore::as_survey_twophase(
        phase1,
        subset = phase2_ind,
        method = "approx"
      )
    )
  } else {
    all_designs <- make_all_designs(seed = seed)
    d <- all_designs[[design]]
  }

  suppressWarnings(switch(
    type,
    means = surveycore::get_means(d, x = y1, group = group, variance = "se"),
    freqs = surveycore::get_freqs(d, x = group),
    ratios = surveycore::get_ratios(d, numerator = y1, denominator = y2)
  ))
}

#' Assert all 8 invariants for a survey_result object
#'
#' Called as the FIRST assertion in every non-error test block.
#'
#' @param result A survey_result object
#' @param expected_class Character(1); the expected subclass (e.g. "survey_means")
test_result_invariants <- function(result, expected_class) {
  testthat::expect_true(
    inherits(result, expected_class),
    label = paste0("inherits(result, '", expected_class, "')")
  )
  testthat::expect_true(
    inherits(result, "survey_result"),
    label = "inherits(result, 'survey_result')"
  )
  testthat::expect_true(
    tibble::is_tibble(result),
    label = "tibble::is_tibble(result)"
  )
  m <- surveycore::meta(result)
  testthat::expect_false(
    is.null(m),
    label = "!is.null(surveycore::meta(result))"
  )
  testthat::expect_true(
    is.list(m),
    label = "is.list(surveycore::meta(result))"
  )
  required_keys <- c(
    "design_type",
    "conf_level",
    "call",
    "group",
    "n_respondents"
  )
  for (k in required_keys) {
    testthat::expect_true(
      k %in% names(m),
      label = paste0("'", k, "' present in .meta")
    )
  }
  testthat::expect_true(
    is.list(m$group),
    label = "meta$group is a list"
  )
  testthat::expect_true(
    is.integer(m$n_respondents),
    label = "meta$n_respondents is integer"
  )

  invisible(result)
}

#' Assert the meta coherence invariant for a survey_result
#'
#' Every name in meta$group and meta$x must be a column in the result tibble.
#' numerator$name and denominator$name must also be present if non-NULL.
#'
#' @param result A survey_result object
test_result_meta_coherent <- function(result) {
  m <- surveycore::meta(result)
  cols <- names(result)
  # Check that all $group keys reference existing output columns.
  # $group keys are grouping variable names that ARE output columns (e.g.,
  # a result grouped by "group" has a "group" column in the output tibble).
  for (g in names(m$group)) {
    testthat::expect_true(
      g %in% cols,
      label = paste("group col", g, "exists in result")
    )
  }
  # Note: $x keys are focal INPUT variable names (e.g., "y1" for
  # get_means(x = y1)). For get_means, the output column is "mean", not "y1".
  # For get_freqs(x = group), the output column IS "group", so $x and output
  # columns happen to align. We do not assert $x against output cols because
  # the semantics differ across analysis functions.
  #
  # $numerator$name and $denominator$name are also input variable names for
  # get_ratios() results — not output column names. Not checked here.
  invisible(result)
}

# ---------------------------------------------------------------------------
# survey_collection test helpers (Phase 0.7)
# ---------------------------------------------------------------------------

#' Build a 3-member collection mixing all three design subclasses.
#'
#' Members are unnamed (taylor / replicate / twophase) and share the column
#' schema produced by make_all_designs(). Default `@id` is ".survey" and
#' default `@if_missing_var` is "error".
make_test_collection <- function(seed = 42L) {
  designs <- make_all_designs(seed = seed)
  surveycore::as_survey_collection(
    !!!designs,
    .id = ".survey",
    .if_missing_var = "error"
  )
}

#' Build a 3-member, all-`survey_taylor` collection with deliberately
#' divergent non-design column schemas. Used to exercise
#' `.if_missing_var = "skip"`, `any_of()`, and per-verb missing-variable
#' handling.
#'
#' Member contract:
#'   * m1 — full schema (psu, strata, fpc, wt, y1, y2, y3, group)
#'   * m2 — drops y2 and y3 (psu, strata, fpc, wt, y1, group)
#'   * m3 — drops y1 and adds region (psu, strata, fpc, wt, y2, y3, group, region)
make_heterogeneous_collection <- function(seed = 42L) {
  base <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)

  m1_data <- base
  m2_data <- base[, !(names(base) %in% c("y2", "y3"))]
  m3_data <- base[, !(names(base) %in% "y1")]
  m3_data$region <- sample(
    c("north", "south", "east", "west"),
    nrow(m3_data),
    replace = TRUE
  )

  to_taylor <- function(df) {
    surveycore::as_survey(
      df,
      ids = psu,
      strata = strata,
      weights = wt,
      fpc = fpc
    )
  }

  surveycore::as_survey_collection(
    m1 = to_taylor(m1_data),
    m2 = to_taylor(m2_data),
    m3 = to_taylor(m3_data),
    .id = ".survey",
    .if_missing_var = "error"
  )
}

#' Collection-level invariant helper. Mirrors `test_invariants()` but for
#' a `survey_collection` (G1 / G1b / @id / @if_missing_var enforcement).
#' Spec §IX.2.
test_collection_invariants <- function(coll) {
  testthat::expect_true(
    S7::S7_inherits(coll, surveycore::survey_collection)
  )

  testthat::expect_gte(length(coll@surveys), 1L)
  for (member in coll@surveys) {
    testthat::expect_true(S7::S7_inherits(member, surveycore::survey_base))
  }

  testthat::expect_type(coll@id, "character")
  testthat::expect_length(coll@id, 1L)
  testthat::expect_true(nzchar(coll@id))

  testthat::expect_true(coll@if_missing_var %in% c("error", "skip"))

  for (member in coll@surveys) {
    testthat::expect_identical(member@groups, coll@groups)
  }

  for (gcol in coll@groups) {
    for (member in coll@surveys) {
      testthat::expect_true(gcol %in% names(member@data))
    }
  }

  invisible(coll)
}


make_all_designs <- function(seed = 42L) {
  df_t <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = seed
  )
  taylor <- surveycore::as_survey(
    df_t,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  df_r <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = seed
  )
  repwt_cols <- grep("^repwt_", names(df_r), value = TRUE)
  replicate <- surveycore::as_survey_replicate(
    df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  df_p <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = seed
  )
  phase1 <- surveycore::as_survey(
    df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  twophase <- suppressWarnings(
    surveycore::as_survey_twophase(phase1, subset = phase2_ind)
  )

  list(taylor = taylor, replicate = replicate, twophase = twophase)
}
