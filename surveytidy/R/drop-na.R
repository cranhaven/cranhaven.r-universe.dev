# R/drop-na.R
#
# drop_na() for survey design objects.
#
# drop_na() marks rows where the specified columns contain NA as out-of-domain,
# without removing them. This is equivalent to filter(!is.na(col1), !is.na(col2), ...)
# and gives correct variance estimates for downstream regression analyses.
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   drop_na.survey_base()   — domain-aware NA marking
#   drop_na.survey_result() — class/meta preservation for survey_result

# ── drop_na() ─────────────────────────────────────────────────────────────────

#' Mark rows with missing values as out-of-domain
#'
#' @description
#' `drop_na()` marks rows where specified columns contain `NA` as
#' out-of-domain, without removing them. If no columns are specified, any
#' `NA` in any column marks the row out-of-domain.
#'
#' This is the domain-aware equivalent of tidyr's `drop_na()`: rather than
#' physically dropping rows, it applies [filter()] with `!is.na()` conditions,
#' preserving all rows for correct variance estimation.
#'
#' @details
#' ## Chaining
#' Successive `drop_na()` calls AND their conditions together, and they
#' accumulate with [filter()] calls too. These are equivalent:
#'
#' ```r
#' drop_na(d, bpxsy1) |> filter(ridageyr >= 18)
#' filter(d, !is.na(bpxsy1), ridageyr >= 18)
#' ```
#'
#' @param data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`tidy-select`][tidyselect::language]> Columns to inspect for
#'   `NA`. If empty, all columns are checked.
#'
#' @return
#' An object of the same type as `data` with the following properties:
#'
#' * Rows are not added or removed.
#' * Rows where selected columns contain `NA` are marked out-of-domain.
#' * Columns and survey design attributes are unchanged.
#'
#' @examples
#' library(tidyr)
#'
#' # create a survey object from the bundled NPORS dataset
#' d <- surveycore::as_survey(
#'   surveycore::pew_npors_2025,
#'   weights = weight,
#'   strata = stratum
#' )
#'
#' # mark rows with NA in votegen_post as out-of-domain
#' drop_na(d, votegen_post)
#'
#' # mark rows with NA in either social media column
#' drop_na(d, smuse_fb, smuse_yt)
#'
#' # no columns specified — any NA in any column marks the row out-of-domain
#' drop_na(d)
#'
#' @family row operations
#' @seealso [filter()] for domain-aware row marking
#' @name drop_na
NULL

#' @rdname drop_na
#' @method drop_na survey_base
drop_na.survey_base <- function(data, ...) {
  # Resolve which columns to check for NA
  if (...length() == 0L) {
    target_cols <- names(data@data)
  } else {
    pos <- tidyselect::eval_select(rlang::expr(c(...)), data@data)
    target_cols <- names(pos)
  }

  # Build !is.na() mask for selected columns, ANDed together
  domain_mask <- !rowSums(is.na(data@data[, target_cols, drop = FALSE])) > 0

  # Chain with existing domain column (same logic as filter())
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  if (domain_col %in% names(data@data)) {
    domain_mask <- data@data[[domain_col]] & domain_mask
  }

  # Warn if empty domain (mirrors filter() behavior — no error)
  if (!any(domain_mask)) {
    cli::cli_warn(
      c(
        "!" = "{.fn drop_na} resulted in an empty domain (0 in-domain rows).",
        "i" = "All rows have {.code NA} in at least one of the selected columns.",
        "v" = paste0(
          "Check the column selection or inspect the data for ",
          "pervasive missingness."
        )
      ),
      class = "surveycore_warning_empty_domain"
    )
  }

  # Store constructed !is.na() quosures in @variables$domain (for introspection)
  na_quos <- lapply(target_cols, function(col) {
    rlang::quo(!is.na(!!rlang::sym(col)))
  })
  data@variables$domain <- c(data@variables$domain, na_quos)

  data@data[[domain_col]] <- domain_mask
  data
}

#' @rdname drop_na
#' @method drop_na survey_result
drop_na.survey_result <- function(data, ...) {
  old_class <- class(data)
  old_meta <- attr(data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}


# ── drop_na.survey_collection (PR 2b) ────────────────────────────────────────

#' @rdname drop_na
#' @method drop_na survey_collection
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `drop_na()` is dispatched to each
#' member independently with the same `...`. Per-member empty-domain warnings
#' fire as usual. The collection's stored `@if_missing_var` controls behavior
#' when a tidyselect-named column is absent from one or more members;
#' detection mode is class-catch (the tidyselect error is caught at dispatch
#' time).
#'
#' Unlike other collection verbs, `drop_na()` does not accept a per-call
#' `.if_missing_var` argument: tidyr's `drop_na()` generic calls
#' `rlang::check_dots_unnamed()` before S3 dispatch, which rejects any named
#' `...` argument. Use [surveycore::set_collection_if_missing_var()] to change
#' the collection's stored behavior instead.
drop_na.survey_collection <- function(data, ...) {
  .dispatch_verb_over_collection(
    fn = tidyr::drop_na,
    verb_name = "drop_na",
    collection = data,
    ...,
    .if_missing_var = NULL,
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
}
