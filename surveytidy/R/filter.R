# R/filter.R
#
# Domain-aware filter() for survey design objects.
#
# filter() MARKS rows as in-domain rather than removing them.
# This is essential for correct variance estimation of subpopulation
# statistics. Removing rows changes the design; marking them does not.
#
# Domain state:
#   - Stored as a logical column "..surveycore_domain.." in @data
#   - Also stored as an accumulated quosure list in @variables$domain
#   - Chained filters AND the domain columns together; conditions accumulate
#   - "domain" key is ABSENT from @variables until first filter() call
#     (formal exception to "all keys always present" rule — see decisions log)
#
# Dispatch wiring: these functions use plain names (filter.survey_base,
# dplyr_reconstruct.survey_base) and are registered in .onLoad() via
# registerS3method() with the namespaced class string
# "surveycore::survey_base". S3 dispatch finds them because the class
# vector for S7 objects includes "surveycore::survey_base".
# See R/zzz.R for the registration.
#
# Functions defined here:
#   filter.survey_base()    — domain estimation (marks rows in-domain)
#   filter.survey_result()  — class/meta preservation for survey_result
#   filter_out()            — domain exclusion (complement of filter)
#   subset.survey_base()    — physical row removal (with warning)
#
# Note: dplyr_reconstruct.survey_base() was moved to R/utils.R on
# feature/select so it is co-located with the other multi-verb helpers.

# ── filter() ─────────────────────────────────────────────────────────────────

#' Keep or drop rows using domain estimation
#'
#' @description
#' `filter()` and `filter_out()` mark rows as in or out of the survey domain
#' without removing them. Unlike a standard data frame filter, all rows are
#' always retained — only their domain status changes. Estimation functions
#' restrict analysis to in-domain rows while using the full design for
#' variance estimation.
#'
#' `filter()` marks rows **matching** the condition as in-domain.
#' `filter_out()` marks rows **matching** the condition as out-of-domain — it
#' is the complement of `filter()`, and reads more naturally when the intent
#' is exclusion.
#'
#' @details
#' ## Chaining
#' Multiple calls accumulate via AND: a row must satisfy every condition to
#' remain in-domain. These are equivalent:
#'
#' ```r
#' filter(d, ridageyr >= 18, riagendr == 2)
#' filter(d, ridageyr >= 18) |> filter(riagendr == 2)
#' ```
#'
#' ## Missing values
#' Unlike base `[`, both functions treat `NA` as `FALSE`: rows where the
#' condition evaluates to `NA` are treated as out-of-domain.
#'
#' ## Useful filter functions
#' * Comparisons: `==`, `>`, `>=`, `<`, `<=`, `!=`
#' * Logical: `&`, `|`, `!`, [xor()]
#' * Missing values: [is.na()]
#' * Range: [dplyr::between()], [dplyr::near()]
#' * Multi-column: [dplyr::if_any()], [dplyr::if_all()]
#'
#' ## Inspecting the domain
#' The domain status of each row is stored in the `..surveycore_domain..`
#' column of `@data`. `TRUE` means in-domain; `FALSE` means out-of-domain.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Logical conditions
#'   evaluated against the survey data. Multiple conditions are combined with
#'   `&`. `NA` results are treated as `FALSE`.
#' @param .by Not supported for survey objects. Use [group_by()] to add
#'   grouping.
#' @param .preserve Ignored. Included for compatibility with the dplyr generic.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * All rows appear in the output.
#' * Domain status of each row may be updated.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # keep adults 50 and older
#' filter(d, agecat >= 3)
#'
#' # multiple conditions are AND-ed together
#' filter(d, agecat >= 3, gender == 2)
#'
#' # filter_out() excludes matching rows — complement of filter()
#' filter_out(d, agecat == 1)
#'
#' # chained calls accumulate (these are equivalent)
#' filter(d, agecat >= 3, gender == 2)
#' d |>
#'   filter(agecat >= 3) |>
#'   filter(gender == 2)
#'
#' # multi-column conditions with if_any() and if_all()
#' filter(d, dplyr::if_any(c(smuse_fb, smuse_yt), ~ !is.na(.x)))
#' filter(d, dplyr::if_all(c(smuse_fb, smuse_yt), ~ !is.na(.x)))
#'
#' @family filtering
#' @seealso [filter_out()] for excluding rows, [subset()] for physical row
#'   removal
#' @name filter
NULL

#' @rdname filter
#' @method filter survey_base
filter.survey_base <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  if (!is.null(.by)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .by} is not supported for survey design objects.",
        "i" = "Use {.fn group_by} to add grouping to a survey design."
      ),
      class = "surveytidy_error_filter_by_unsupported"
    )
  }

  # Capture quosures for accumulation in @variables$domain (done at the end).
  conditions <- rlang::quos(...)

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  n <- nrow(.data@data)

  if (length(conditions) == 0L) {
    # No conditions — trivial domain (all rows in-domain)
    domain_mask <- rep(TRUE, n)
  } else {
    # Evaluate conditions via dplyr::filter() so that dplyr helpers like
    # if_any() and if_all() work. These functions require dplyr's internal
    # data-masking context, which rlang::eval_tidy() alone does not provide.
    #
    # Strategy: attach a sentinel row-ID column, run dplyr::filter(), then
    # use set membership to determine which rows are in-domain.
    row_id_col <- "..surveytidy_filter_id.."
    while (row_id_col %in% names(.data@data)) {
      row_id_col <- paste0(".", row_id_col, ".")
    }
    tmp <- .data@data
    tmp[[row_id_col]] <- seq_len(n)
    kept <- dplyr::filter(tmp, ...)
    domain_mask <- seq_len(n) %in% kept[[row_id_col]]
  }

  # AND with existing domain column if present (chained filters)
  if (domain_col %in% names(.data@data)) {
    domain_mask <- .data@data[[domain_col]] & domain_mask
  }

  # Warn on empty domain
  if (!any(domain_mask)) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "filter() produced an empty domain \u2014 no rows match ",
          "the condition."
        ),
        "i" = "Variance estimation on this domain will fail."
      ),
      class = "surveycore_warning_empty_domain"
    )
  }

  # Store domain column in @data; accumulate condition quosures in @variables
  .data@data[[domain_col]] <- domain_mask
  .data@variables$domain <- c(.data@variables$domain, conditions)
  .data
}

#' @rdname filter
#' @method filter survey_result
filter.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname filter
#' @method filter survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `filter()` is dispatched to each
#' member independently. Each member's domain column is updated per
#' `filter.survey_base`'s contract; the per-member empty-domain warning
#' (`surveycore_warning_empty_domain`) fires N times on an N-member collection
#' if every member's filter is empty. The output `survey_collection` preserves
#' the input's `@id`, `@if_missing_var`, and `@groups`. Use `.if_missing_var`
#' to override the collection's stored missing-variable behavior for this call.
#'
#' `.by` is rejected at the collection layer with
#' `surveytidy_error_collection_by_unsupported`. Set grouping with
#' [group_by()] on the collection instead.
filter.survey_collection <- function(
  .data,
  ...,
  .by = NULL,
  .preserve = FALSE,
  .if_missing_var = NULL
) {
  if (!is.null(.by)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .by} is not supported on {.cls survey_collection}.",
        "i" = "Per-call grouping overrides do not compose cleanly with {.code coll@groups}.",
        "v" = "Use {.fn group_by} on the collection (or set {.code coll@groups}) instead."
      ),
      class = "surveytidy_error_collection_by_unsupported"
    )
  }
  .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = .data,
    ...,
    .preserve = .preserve,
    .if_missing_var = .if_missing_var,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
}


# ── filter_out() ─────────────────────────────────────────────────────────────

#' @rdname filter
#' @method filter_out survey_base
filter_out.survey_base <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  if (!is.null(.by)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .by} is not supported for survey design objects.",
        "i" = "Use {.fn group_by} to add grouping to a survey design."
      ),
      class = "surveytidy_error_filter_by_unsupported"
    )
  }

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  n <- nrow(.data@data)

  if (...length() == 0L) {
    # No conditions — exclude nothing; all rows remain in-domain
    domain_mask <- rep(TRUE, n)
  } else {
    # Evaluate conditions via dplyr::filter() to support if_any()/if_all().
    # Rows that survive the filter MATCH the exclusion condition, so those
    # are the rows to mark out-of-domain.
    row_id_col <- "..surveytidy_filter_id.."
    while (row_id_col %in% names(.data@data)) {
      row_id_col <- paste0(".", row_id_col, ".")
    }
    tmp <- .data@data
    tmp[[row_id_col]] <- seq_len(n)
    kept <- dplyr::filter(tmp, ...)
    # In-domain = rows NOT matching the exclusion condition
    # (dplyr treats NA conditions as FALSE → those rows are NOT in kept →
    #  they stay in-domain, matching the documented behaviour)
    domain_mask <- !(seq_len(n) %in% kept[[row_id_col]])
  }

  # AND with existing domain column if present (chained calls)
  if (domain_col %in% names(.data@data)) {
    domain_mask <- .data@data[[domain_col]] & domain_mask
  }

  # Warn on empty domain
  if (!any(domain_mask)) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "filter_out() produced an empty domain \u2014 all rows were excluded."
        ),
        "i" = "Variance estimation on this domain will fail."
      ),
      class = "surveycore_warning_empty_domain"
    )
  }

  .data@data[[domain_col]] <- domain_mask
  .data
}

#' @rdname filter
#' @method filter_out survey_collection
#' @inheritParams survey_collection_args
filter_out.survey_collection <- function(
  .data,
  ...,
  .by = NULL,
  .preserve = FALSE,
  .if_missing_var = NULL
) {
  if (!is.null(.by)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .by} is not supported on {.cls survey_collection}.",
        "i" = "Per-call grouping overrides do not compose cleanly with {.code coll@groups}.",
        "v" = "Use {.fn group_by} on the collection (or set {.code coll@groups}) instead."
      ),
      class = "surveytidy_error_collection_by_unsupported"
    )
  }
  .dispatch_verb_over_collection(
    fn = filter_out,
    verb_name = "filter_out",
    collection = .data,
    ...,
    .preserve = .preserve,
    .if_missing_var = .if_missing_var,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
}


# ── subset() ─────────────────────────────────────────────────────────────────

#' Physically remove rows from a survey design object
#'
#' @description
#' `subset()` physically removes rows from a
#' [`survey_base`][surveycore::survey_base] object where `condition` evaluates
#' to `FALSE`. **This changes the survey design.** Unless the design was
#' explicitly built for the subset population, variance estimates will be
#' incorrect.
#'
#' For subpopulation analyses, use [filter()] instead. `filter()` marks rows
#' as in or out of the domain without removing them, leaving the full design
#' intact for variance estimation.
#'
#' `subset()` always emits a `surveycore_warning_physical_subset` warning as a
#' reminder of the statistical implications.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param condition A logical expression evaluated against the survey data.
#'   Rows where `condition` is `FALSE` or `NA` are removed.
#' @param ... Ignored. Included for compatibility with the base [subset()]
#'   generic.
#'
#' @return
#' An object of the same type as `x` with only matching rows retained. Always
#' issues `surveycore_warning_physical_subset`.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # physical row removal — always issues a warning
#' subset(d, agecat >= 3)
#'
#' @seealso [filter()] for domain-aware row marking (preferred for
#'   subpopulation analyses)
#' @export
subset.survey_base <- function(x, condition, ...) {
  .warn_physical_subset("subset")

  cond_quo <- rlang::enquo(condition)
  keep_mask <- rlang::eval_tidy(cond_quo, data = x@data)
  keep_mask[is.na(keep_mask)] <- FALSE

  if (!any(keep_mask)) {
    cli::cli_abort(
      c(
        "x" = "subset() condition matched 0 rows.",
        "i" = "Survey objects require at least 1 row.",
        "v" = "Use {.fn filter} for domain estimation (keeps all rows)."
      ),
      class = "surveytidy_error_subset_empty_result"
    )
  }

  x@data <- x@data[keep_mask, , drop = FALSE]
  x
}
