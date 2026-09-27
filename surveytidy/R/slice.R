# R/slice.R
#
# slice_*() family for survey design objects.
#
# slice_*() functions physically remove rows. They always issue
# surveycore_warning_physical_subset and error on 0-row results. A factory
# function is used to avoid repetition across the six slice variants.
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   slice.survey_base()            — row selection by position
#   slice.survey_result()          — class/meta preservation for survey_result
#   slice_head.survey_base()       — first n rows
#   slice_head.survey_result()     — class/meta preservation for survey_result
#   slice_tail.survey_base()       — last n rows
#   slice_tail.survey_result()     — class/meta preservation for survey_result
#   slice_min.survey_base()        — n rows with lowest values of a variable
#   slice_min.survey_result()      — class/meta preservation for survey_result
#   slice_max.survey_base()        — n rows with highest values of a variable
#   slice_max.survey_result()      — class/meta preservation for survey_result
#   slice_sample.survey_base()     — random sample of rows
#   slice_sample.survey_result()   — class/meta preservation for survey_result

# `check_fn` is a free variable in functions created by .make_slice_method().
# R CMD check cannot determine from static analysis that it is always defined
# in the enclosing environment via the factory — suppress the note here.
utils::globalVariables("check_fn")


# ── collection helpers (PR 2d) ────────────────────────────────────────────────

# Verb-specific pre-flight: catch slice arguments that would empty every
# member BEFORE any member is touched. Without this, the per-member dispatch
# turns each member into a 0-row data frame and the surveycore class
# validator surfaces a misleading "single member is invalid" message.
# Spec §IV.6.
#
# Per-variant logic:
#   slice         — eval each `...` quosure in an empty environment via
#                   tryCatch(eval_tidy(..., data = NULL)). Raise iff the
#                   result is integer(0). Eval failure (NSE references a
#                   column or `n()`) silently skips the pre-flight; the
#                   per-member call is the source of truth.
#   slice_head /
#   slice_tail /
#   slice_sample /
#   slice_min  /
#   slice_max     — raise iff `n == 0L` or `prop == 0`. (`slice_min` /
#                   `slice_max` deliberately do NOT pre-evaluate their
#                   `order_by` expression; only n/prop fully determine
#                   emptiness.)
.check_slice_zero <- function(verb_name, dots = NULL, n = NULL, prop = NULL) {
  empties <- FALSE
  if (identical(verb_name, "slice")) {
    for (quo in dots) {
      val <- tryCatch(
        rlang::eval_tidy(quo, data = NULL),
        error = function(cnd) NULL
      )
      if (is.integer(val) && length(val) == 0L) {
        empties <- TRUE
        break
      }
    }
  } else {
    if (!is.null(n) && length(n) == 1L && !is.na(n) && n == 0L) {
      empties <- TRUE
    }
    if (!is.null(prop) && length(prop) == 1L && !is.na(prop) && prop == 0) {
      empties <- TRUE
    }
  }

  if (!empties) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    c(
      "x" = "{.fn {verb_name}} arguments would produce 0 rows on every member of the collection.",
      "i" = "Survey objects require at least 1 row, so the operation cannot proceed.",
      "v" = "Pass a non-zero {.arg n} or {.arg prop}, or use {.fn filter} for domain estimation (keeps all rows)."
    ),
    class = "surveytidy_error_collection_slice_zero"
  )
}

# Derive a stable per-survey integer seed from a survey name and a
# user-provided seed. Spec §II.3.3.
#
# Used only by `slice_sample.survey_collection` when `seed` is non-NULL.
# Returns an integer in [0, 2^28).
.derive_member_seed <- function(survey_name, seed) {
  hex <- rlang::hash(paste0(survey_name, "::", seed))
  strtoi(substr(hex, 1, 7), 16L)
}


# ── slice_*() factory ─────────────────────────────────────────────────────────

# Inline helper: warn when slice_sample() is called with weight_by =
# The weight_by column is independent of the survey design weights — it samples
# proportional to arbitrary values, not the probability-weighted design.
# Uses ...names() to detect the weight_by argument without evaluating it
# (weight_by is a tidy-select/NSE argument and evaluating it here would fail).
.check_slice_sample_weight_by <- function(...) {
  # Body runs only via the .make_slice_method() closure (passed as `check_fn`);
  # covr cannot trace closure execution back to source lines. Verified covered
  # by test-arrange.R "slice_sample() with weight_by = issues additional warning".
  # nocov start
  if ("weight_by" %in% ...names()) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "{.fn slice_sample} was called with {.arg weight_by} on a survey ",
          "object."
        ),
        "i" = paste0(
          "The {.arg weight_by} column samples rows proportional to its ",
          "values, independently of the survey design weights."
        ),
        "v" = paste0(
          "If you intend probability-proportional sampling, use the survey ",
          "design weights instead."
        )
      ),
      class = "surveytidy_warning_slice_sample_weight_by"
    )
  }
  # nocov end
}

# Factory that produces a slice_*.survey_base function.
#   fn_name:  character; shown in warnings and errors (e.g. "slice_head")
#   dplyr_fn: the corresponding dplyr function (e.g. dplyr::slice_head)
#   check_fn: optional extra check run before slicing (NULL for most variants)
.make_slice_method <- function(fn_name, dplyr_fn, check_fn = NULL) {
  # force() the closure variables so R CMD check understands they are
  # intentional free variables in the returned function, not typos.
  # The factory body and its returned closure execute under indirection
  # (assigned at top-level, then invoked via dplyr's S3 dispatch); covr
  # cannot trace that execution back to source lines. Verified covered by
  # every slice_*() test in test-arrange.R (including the n=0 error path
  # exercised at lines 191–217).
  # nocov start
  force(fn_name)
  force(dplyr_fn)
  force(check_fn)
  function(.data, ...) {
    .warn_physical_subset(fn_name)
    if (!is.null(check_fn)) {
      check_fn(...)
    }
    new_data <- dplyr_fn(.data@data, ...)
    if (nrow(new_data) == 0L) {
      cli::cli_abort(
        c(
          "x" = "{.fn {fn_name}} produced 0 rows.",
          "i" = "Survey objects require at least 1 row.",
          "v" = "Use {.fn filter} for domain estimation (keeps all rows)."
        ),
        class = "surveytidy_error_subset_empty_result"
      )
    }
    .data@data <- new_data
    .data
  }
  # nocov end
}

#' Physically select rows of a survey design object
#'
#' @description
#' `slice()`, `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()`,
#' and `slice_sample()` **physically remove rows** from a survey design
#' object. For subpopulation analyses, use [filter()] instead — it marks
#' rows as out-of-domain without removing them, preserving valid variance
#' estimation.
#'
#' All slice functions always issue `surveycore_warning_physical_subset`
#' and error if the result would have 0 rows.
#'
#' @details
#' ## Physical subsetting
#' Unlike [filter()], slice functions actually remove rows. This changes
#' the survey design — unless the design was explicitly built for the
#' subset population, variance estimates may be incorrect.
#'
#' ## `slice_sample()` and survey weights
#' `slice_sample(weight_by = )` samples rows proportional to a column's
#' values, independently of the survey design weights. A
#' `surveytidy_warning_slice_sample_weight_by` warning is issued as a
#' reminder. If you intend probability-proportional sampling, use the
#' design weights directly.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, a
#'   `survey_result` object returned by a surveycore estimation function, or
#'   a [`survey_collection`][surveycore::survey_collection].
#' @param ... Passed to the corresponding `dplyr::slice_*()` function. For
#'   `slice()` only, the `...` accepts a vector of row indices.
#' @param n Number of rows to keep. See [`dplyr::slice_head()`].
#' @param prop Fraction of rows to keep (between 0 and 1). See
#'   [`dplyr::slice_head()`].
#' @param order_by <[`data-masking`][rlang::args_data_masking]> Variable to
#'   order by, used by `slice_min()` and `slice_max()`. See
#'   [`dplyr::slice_min()`].
#' @param with_ties Should ties be kept together? Used by `slice_min()` and
#'   `slice_max()`. See [`dplyr::slice_min()`].
#' @param na_rm Should missing values in `order_by` be removed before
#'   slicing? Used by `slice_min()` and `slice_max()`. See
#'   [`dplyr::slice_min()`].
#' @param weight_by <[`data-masking`][rlang::args_data_masking]> Sampling
#'   weights for `slice_sample()`. See [`dplyr::slice_sample()`]. Independent
#'   of the survey design weights — issues
#'   `surveytidy_warning_slice_sample_weight_by` as a reminder.
#' @param replace Should sampling be performed with replacement? Used by
#'   `slice_sample()`. See [`dplyr::slice_sample()`].
#' @param seed Used by `slice_sample.survey_collection` only. `NULL` (the
#'   default) leaves the ambient RNG state alone; an integer seed makes
#'   per-survey samples deterministic and order-independent (see
#'   "Survey collections" below).
#' @param by Per-call grouping override accepted by `slice_min()`,
#'   `slice_max()`, and `slice_sample()`. Not supported on
#'   `survey_collection` — passing a non-NULL value raises
#'   `surveytidy_error_collection_by_unsupported`. Use [group_by()] on the
#'   collection (or set `coll@groups`) instead.
#' @param .by Accepted for interface compatibility; not used by survey
#'   methods.
#' @param .preserve Accepted for interface compatibility; not used by survey
#'   methods.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * A subset of rows is retained; unselected rows are permanently removed.
#' * Columns and survey design attributes are unchanged.
#' * Always issues `surveycore_warning_physical_subset`.
#'
#' @examples
#' # create a survey object from the bundled NPORS dataset
#' d <- surveycore::as_survey(
#'   surveycore::pew_npors_2025,
#'   weights = weight,
#'   strata = stratum
#' )
#'
#' # first 10 rows (issues a physical subset warning)
#' slice_head(d, n = 10)
#'
#' # rows with the 5 lowest survey weights
#' slice_min(d, order_by = weight, n = 5)
#'
#' # random sample of 50 rows
#' slice_sample(d, n = 50)
#'
#' @family row operations
#' @seealso [filter()] for domain-aware row marking (preferred for
#'   subpopulation analyses), [arrange()] for row sorting
#' @name slice
NULL

#' @rdname slice
#' @method slice survey_base
slice.survey_base <- .make_slice_method(
  "slice",
  dplyr::slice
)
#' @rdname slice
#' @method slice_head survey_base
slice_head.survey_base <- .make_slice_method(
  "slice_head",
  dplyr::slice_head
)
#' @rdname slice
#' @method slice_tail survey_base
slice_tail.survey_base <- .make_slice_method(
  "slice_tail",
  dplyr::slice_tail
)
#' @rdname slice
#' @method slice_min survey_base
slice_min.survey_base <- .make_slice_method(
  "slice_min",
  dplyr::slice_min
)
#' @rdname slice
#' @method slice_max survey_base
slice_max.survey_base <- .make_slice_method(
  "slice_max",
  dplyr::slice_max
)
#' @rdname slice
#' @method slice_sample survey_base
slice_sample.survey_base <- .make_slice_method(
  "slice_sample",
  dplyr::slice_sample,
  check_fn = .check_slice_sample_weight_by
)

# ── survey_result passthrough variants ────────────────────────────────────────

#' @rdname slice
#' @method slice survey_result
slice.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname slice
#' @method slice_head survey_result
slice_head.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname slice
#' @method slice_tail survey_result
slice_tail.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname slice
#' @method slice_min survey_result
slice_min.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname slice
#' @method slice_max survey_result
slice_max.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname slice
#' @method slice_sample survey_result
slice_sample.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}


# ── survey_collection methods (PR 2d) ─────────────────────────────────────────

#' @rdname slice
#' @method slice survey_collection
#'
#' @section Survey collections:
#' Slice variants are dispatched to each member independently. Each member's
#' `slice_*.survey_base` call emits `surveycore_warning_physical_subset` — an
#' N-member collection therefore surfaces N warnings.
#'
#' Before dispatching, a verb-specific pre-flight raises
#' `surveytidy_error_collection_slice_zero` when the supplied arguments would
#' produce a 0-row result on every member (e.g., `n = 0`, literal
#' `slice(integer(0))`). This stops dispatch before any member is touched, so
#' users see a slice-specific message instead of a misleading per-member
#' validator failure.
#'
#' `slice`, `slice_head`, `slice_tail`, and `slice_sample` (when `weight_by =
#' NULL`) reference no user columns — their signatures omit `.if_missing_var`.
#' `slice_min`, `slice_max`, and `slice_sample` with a non-NULL `weight_by`
#' do reference user columns; their signatures include `.if_missing_var`.
#'
#' `slice_min`, `slice_max`, and `slice_sample` reject the per-call `by`
#' argument with `surveytidy_error_collection_by_unsupported`; use
#' [group_by()] on the collection (or `coll@groups`) instead.
slice.survey_collection <- function(.data, ...) {
  dots <- rlang::enquos(...)
  .check_slice_zero("slice", dots = dots)
  .dispatch_verb_over_collection(
    fn = dplyr::slice,
    verb_name = "slice",
    collection = .data,
    ...,
    .detect_missing = "none",
    .may_change_groups = FALSE
  )
}

#' @rdname slice
#' @method slice_head survey_collection
slice_head.survey_collection <- function(.data, ..., n = NULL, prop = NULL) {
  .check_slice_zero("slice_head", n = n, prop = prop)
  .dispatch_verb_over_collection(
    fn = dplyr::slice_head,
    verb_name = "slice_head",
    collection = .data,
    ...,
    .scalar_args = list(n = n, prop = prop),
    .detect_missing = "none",
    .may_change_groups = FALSE
  )
}

#' @rdname slice
#' @method slice_tail survey_collection
slice_tail.survey_collection <- function(.data, ..., n = NULL, prop = NULL) {
  .check_slice_zero("slice_tail", n = n, prop = prop)
  .dispatch_verb_over_collection(
    fn = dplyr::slice_tail,
    verb_name = "slice_tail",
    collection = .data,
    ...,
    .scalar_args = list(n = n, prop = prop),
    .detect_missing = "none",
    .may_change_groups = FALSE
  )
}

#' @rdname slice
#' @method slice_min survey_collection
#' @inheritParams survey_collection_args
slice_min.survey_collection <- function(
  .data,
  order_by,
  ...,
  n = NULL,
  prop = NULL,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .if_missing_var = NULL
) {
  .reject_collection_by(rlang::enquo(by), "slice_min")
  .check_slice_zero("slice_min", n = n, prop = prop)
  order_by_quo <- rlang::enquo(order_by)
  rlang::inject(
    .dispatch_verb_over_collection(
      fn = dplyr::slice_min,
      verb_name = "slice_min",
      collection = .data,
      order_by = !!order_by_quo,
      ...,
      .scalar_args = list(
        n = n,
        prop = prop,
        with_ties = with_ties,
        na_rm = na_rm
      ),
      .if_missing_var = .if_missing_var,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  )
}

#' @rdname slice
#' @method slice_max survey_collection
#' @inheritParams survey_collection_args
slice_max.survey_collection <- function(
  .data,
  order_by,
  ...,
  n = NULL,
  prop = NULL,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .if_missing_var = NULL
) {
  .reject_collection_by(rlang::enquo(by), "slice_max")
  .check_slice_zero("slice_max", n = n, prop = prop)
  order_by_quo <- rlang::enquo(order_by)
  rlang::inject(
    .dispatch_verb_over_collection(
      fn = dplyr::slice_max,
      verb_name = "slice_max",
      collection = .data,
      order_by = !!order_by_quo,
      ...,
      .scalar_args = list(
        n = n,
        prop = prop,
        with_ties = with_ties,
        na_rm = na_rm
      ),
      .if_missing_var = .if_missing_var,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  )
}

#' @rdname slice
#' @method slice_sample survey_collection
#' @inheritParams survey_collection_args
#'
#' @section `slice_sample.survey_collection` reproducibility:
#' `slice_sample.survey_collection` adds a `seed = NULL` argument absent from
#' `slice_sample.survey_base`.
#'
#' * `seed = NULL` (default): no seed manipulation. Per-survey
#'   `slice_sample()` calls draw from the ambient RNG state in iteration
#'   order. Reproducibility requires a single upstream `set.seed()` AND a
#'   stable collection size and member order — adding or removing a survey
#'   changes the samples drawn from every subsequent survey.
#' * `seed = <integer>`: each per-survey call is wrapped with a deterministic
#'   per-survey seed derived as
#'   `strtoi(substr(rlang::hash(paste0(survey_name, "::", seed)), 1, 7), 16L)`.
#'   Per-survey samples are stable regardless of collection order, additions,
#'   or removals. The ambient `.Random.seed` is restored on exit.
#'
#' For any analysis intended to be reproducible, pass an explicit integer
#' `seed`.
slice_sample.survey_collection <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  by = NULL,
  weight_by = NULL,
  replace = FALSE,
  seed = NULL,
  .if_missing_var = NULL
) {
  .reject_collection_by(rlang::enquo(by), "slice_sample")
  .check_slice_zero("slice_sample", n = n, prop = prop)

  weight_by_quo <- rlang::enquo(weight_by)
  has_weight_by <- !rlang::quo_is_null(weight_by_quo)
  detect <- if (has_weight_by) "pre_check" else "none"
  # Build the optional weight_by arg list outside `inject()`. Nesting `!!q`
  # inside `if (...) list(...) else list()` does NOT get substituted because
  # `inject()` does not recurse through `if/else` expressions; the `!!`
  # would then evaluate as base R's double-negation (`Ops.quosure`) and
  # fail. Splicing a pre-built list of quosures via `!!!` works because
  # dplyr accepts quosures as data-mask arguments.
  weight_by_args <- if (has_weight_by) {
    list(weight_by = weight_by_quo)
  } else {
    list()
  }

  if (is.null(seed)) {
    rlang::inject(
      .dispatch_verb_over_collection(
        fn = dplyr::slice_sample,
        verb_name = "slice_sample",
        collection = .data,
        ...,
        !!!weight_by_args,
        .scalar_args = list(n = n, prop = prop, replace = replace),
        .if_missing_var = .if_missing_var,
        .detect_missing = detect,
        .may_change_groups = FALSE
      )
    )
  } else {
    .slice_sample_seeded(
      collection = .data,
      ...,
      n = n,
      prop = prop,
      weight_by_quo = weight_by_quo,
      replace = replace,
      seed = seed,
      resolved_if_missing_var = .if_missing_var %||% .data@if_missing_var,
      id_from_stored = is.null(.if_missing_var),
      detect = detect
    )
  }
}


# ── slice_sample seeded path (PR 2d) ──────────────────────────────────────────

# Shared inline helper: reject the per-call `by` argument on a collection-layer
# slice verb. Mirrors the inlined block in filter.survey_collection /
# mutate.survey_collection. Lives in slice.R because it's used only here
# (slice_min, slice_max, slice_sample). Takes `by_quo` (a quosure captured by
# the verb method via `rlang::enquo(by)`) so a non-NULL `by = <bare_name>`
# expression doesn't force evaluation of the bare name (which may not exist
# in any environment) before the rejection fires.
.reject_collection_by <- function(by_quo, verb_name) {
  if (!rlang::quo_is_null(by_quo)) {
    cli::cli_abort(
      c(
        "x" = "{.arg by} is not supported on {.cls survey_collection}.",
        "i" = "Per-call grouping overrides do not compose cleanly with {.code coll@groups}.",
        "v" = "Use {.fn group_by} on the collection (or set {.code coll@groups}) instead."
      ),
      class = "surveytidy_error_collection_by_unsupported"
    )
  }
}

# Manual per-survey loop for slice_sample with seed != NULL. Mirrors
# `.dispatch_verb_over_collection()` because the dispatcher does not expose
# a per-survey hook needed to derive a per-survey seed via
# `.derive_member_seed()`. Restores ambient `.Random.seed` on exit so the
# call is side-effect-free for the caller's RNG state.
.slice_sample_seeded <- function(
  collection,
  ...,
  n,
  prop,
  weight_by_quo,
  replace,
  seed,
  resolved_if_missing_var,
  id_from_stored,
  detect
) {
  dots <- rlang::enquos(...)
  dots <- .unwrap_scalar_dots(dots)

  results <- vector("list", length(collection@surveys))
  names(results) <- names(collection@surveys)
  skipped <- character(0L)

  for (nm in names(collection@surveys)) {
    survey <- collection@surveys[[nm]]

    # Detection: pre_check inspects weight_by + any passed dots for missing
    # column refs.
    if (identical(detect, "pre_check")) {
      check_dots <- c(dots, list(weight_by = weight_by_quo))
      sentinel <- .pre_check_missing_vars(check_dots, survey, nm)
      if (!is.null(sentinel)) {
        if (identical(resolved_if_missing_var, "skip")) {
          skipped <- c(skipped, nm)
          next
        }
        cli::cli_abort(
          c(
            "x" = "{.fn slice_sample} failed on survey {.val {nm}}: missing referenced variable{?s} {.field {sentinel$missing_vars}}."
          ),
          parent = sentinel,
          class = "surveytidy_error_collection_verb_failed"
        )
      }
    }

    scalar_args <- list(n = n, prop = prop, replace = replace)
    scalar_args <- scalar_args[
      !vapply(scalar_args, is.null, logical(1L))
    ]
    weight_by_args <- if (!rlang::quo_is_null(weight_by_quo)) {
      list(weight_by = weight_by_quo)
    } else {
      list()
    }
    r <- withr::with_seed(
      .derive_member_seed(nm, seed),
      rlang::inject(dplyr::slice_sample(
        survey,
        !!!dots,
        !!!weight_by_args,
        !!!scalar_args
      ))
    )
    results[[nm]] <- r
  }

  results <- results[!vapply(results, is.null, logical(1L))]

  if (length(skipped) > 0L) {
    cli::cli_inform(
      c(
        "i" = "{.fn slice_sample} skipped survey{?s} {.val {skipped}} (missing referenced variable{?s})."
      ),
      class = "surveytidy_message_collection_skipped_surveys"
    )
  }

  if (length(results) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.fn slice_sample} produced an empty {.cls survey_collection}.",
        "i" = if (identical(resolved_if_missing_var, "skip")) {
          "All surveys were skipped because they were missing referenced variables."
        } else {
          # Unreachable in slice_sample: results are emptied only when every
          # survey is skipped (skip mode). In error mode, a missing weight_by
          # raises before this branch; in the absence of weight_by, slice_sample
          # with n > 0 / prop > 0 (slice-zero is pre-checked) never returns
          # empty results from non-empty data. Branch kept for symmetry with
          # the dispatcher's empty-result message.
          "All surveys produced empty results." # nocov
        },
        "i" = if (id_from_stored) {
          "{.code .if_missing_var} resolved to {.val {resolved_if_missing_var}} from the collection's stored property."
        } else {
          "{.code .if_missing_var = {.val {resolved_if_missing_var}}} was passed to this call."
        },
        "v" = "Inspect {.fn names} on the input collection and verify each member has the referenced columns."
      ),
      class = "surveytidy_error_collection_verb_emptied"
    )
  }

  rlang::inject(surveycore::as_survey_collection(
    !!!results,
    .id = collection@id,
    .if_missing_var = collection@if_missing_var
  ))
}
