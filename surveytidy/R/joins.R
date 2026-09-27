# R/joins.R
#
# dplyr join functions for survey design objects.
#
# All join functions are designed to preserve survey design integrity:
# - Functions that could add rows (right_join, full_join) error unconditionally.
# - Functions that could remove rows (inner_join) are domain-aware by default.
# - Functions that would corrupt design variables (any join where y has columns
#   matching design variable names) warn and drop those columns from y.
# - bind_rows errors unconditionally when x is a survey object.
#
# Dispatch wiring: registered in .onLoad() via registerS3method() with the
# namespaced class string "surveycore::survey_base". See R/zzz.R.
#
# Functions defined here:
#   left_join.survey_base()   — adds lookup columns from a data frame
#   semi_join.survey_base()   — domain-aware: keep rows matching y
#   anti_join.survey_base()   — domain-aware: keep rows NOT matching y
#   bind_cols.survey_base()   — append columns by position
#   inner_join.survey_base()  — domain-aware (default) or physical subset
#   right_join.survey_base()  — always errors
#   full_join.survey_base()   — always errors
#   bind_rows.survey_base()   — always errors

# ── Shared internal helpers ───────────────────────────────────────────────────

# Check that y is not a survey design object.
# Errors with surveytidy_error_join_survey_to_survey if it is.
# Returns invisible(TRUE) on success.
.check_join_y_type <- function(y) {
  if (S7::S7_inherits(y, surveycore::survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.arg y} is a survey design object, not a data frame.",
        "i" = paste0(
          "Joining two survey objects requires manual reconciliation of ",
          "design specifications."
        ),
        "v" = paste0(
          "Extract {.code y@data} to join the underlying data, then ",
          "re-specify the design with {.fn surveycore::as_survey}."
        )
      ),
      class = "surveytidy_error_join_survey_to_survey"
    )
  }
  invisible(TRUE)
}

# Check that y does not have columns that match design variable names in x.
# Also protects the domain column (SURVEYCORE_DOMAIN_COL).
# Columns listed in `by` are excluded from the check (they are join keys).
#
# Arguments:
#   x  : survey_base object
#   y  : data.frame
#   by : character vector of join key names (from the x side), or character(0)
#
# Returns: cleaned y (with conflicting columns dropped). May emit warning.
.check_join_col_conflict <- function(x, y, by) {
  # Build the set of protected column names
  protected <- c(
    .survey_design_var_names(x),
    surveycore::SURVEYCORE_DOMAIN_COL
  )

  # Parse by argument to get the x-side key names
  by_x_names <- if (is.null(by)) {
    character(0)
  } else if (is.character(by)) {
    unname(by)
  } else {
    by$x # dplyr::join_by() object
  }

  # Conflicting: protected columns that appear in y, excluding join keys
  conflicting <- intersect(names(y), setdiff(protected, by_x_names))

  if (length(conflicting) > 0L) {
    n_conflict <- length(conflicting)
    cli::cli_warn(
      c(
        "!" = paste0(
          "{.arg y} has {n_conflict} column{?s} with the same name{?s} as ",
          "design variable{?s}: {.field {conflicting}}."
        ),
        "i" = paste0(
          "{n_conflict} conflicting column{?s} dropped from {.arg y} before ",
          "joining to protect the survey design."
        ),
        "i" = "Use {.fn dplyr::rename} on {.arg y} to resolve before joining."
      ),
      class = "surveytidy_warning_join_col_conflict"
    )
    y <- y[, setdiff(names(y), conflicting), drop = FALSE]
  }

  y
}

# Check that the row count did not expand after a join.
# Errors with surveytidy_error_join_row_expansion if new_nrow > original_nrow.
#
# Arguments:
#   original_nrow : integer - row count before the join
#   new_nrow      : integer - row count after the join
#   by_label      : character(1) or NULL - join key info for the error message
#
# Returns invisible(TRUE) on success.
.check_join_row_expansion <- function(
  original_nrow,
  new_nrow,
  by_label = NULL
) {
  if (new_nrow > original_nrow) {
    by_msg <- if (!is.null(by_label)) {
      paste0(" on key{?s} {.field {by_label}}")
    } else {
      ""
    }
    cli::cli_abort(
      c(
        "x" = paste0(
          "The join would expand {.arg x} from {original_nrow} to ",
          "{new_nrow} row{?s} because {.arg y} has duplicate keys",
          by_msg,
          "."
        ),
        "i" = "Duplicate respondent rows corrupt variance estimation.",
        "v" = paste0(
          "Use {.fn dplyr::distinct} to deduplicate {.arg y} before joining."
        )
      ),
      class = "surveytidy_error_join_row_expansion"
    )
  }
  invisible(TRUE)
}

# Construct a typed S3 sentinel for @variables$domain.
# Phase 1 consumers use inherits(entry, "surveytidy_join_domain") to
# distinguish sentinels from quosures.
#
# Arguments:
#   type : character(1) - "semi_join", "anti_join", or "inner_join"
#   keys : character    - vector of key column names
.new_join_domain_sentinel <- function(type, keys) {
  structure(list(type = type, keys = keys), class = "surveytidy_join_domain")
}

# Detect and repair suffix renames in @metadata and @variables$visible_vars.
# When a left_join or inner_join suffixes an x-side column (because y has a
# non-design column with the same name), the @metadata@variable_labels key and
# visible_vars entry for the original column name must be updated to the new
# suffixed name.
#
# Arguments:
#   x          : survey_base — after x@data has been updated with join result
#   old_x_cols : character   — names(x@data) captured BEFORE the join
#   suffix     : character(2) — the suffix argument passed to the join
#
# Returns: modified x with updated @metadata and @variables$visible_vars.
.repair_suffix_renames <- function(x, old_x_cols, suffix) {
  current_cols <- names(x@data)

  # Find columns that were in old_x_cols but are no longer in current_cols
  # (they were renamed with a suffix)
  renamed_old <- setdiff(old_x_cols, current_cols)
  if (length(renamed_old) == 0L) {
    return(x)
  }

  # For each renamed column, check if the suffixed version exists
  suffix_x <- suffix[[1L]]
  rename_map <- character(0)
  for (old in renamed_old) {
    new <- paste0(old, suffix_x)
    if (new %in% current_cols) {
      rename_map[[old]] <- new
    }
  }

  if (length(rename_map) == 0L) {
    return(x)
  }

  # Update @metadata@variable_labels keys
  vl <- x@metadata@variable_labels
  for (old in names(rename_map)) {
    if (!is.null(vl[[old]])) {
      vl[[rename_map[[old]]]] <- vl[[old]]
      vl[[old]] <- NULL
    }
  }
  x@metadata@variable_labels <- vl

  # Update @variables$visible_vars entries
  vv <- x@variables$visible_vars
  if (!is.null(vv)) {
    x@variables$visible_vars <- ifelse(
      vv %in% names(rename_map),
      rename_map[vv],
      vv
    )
  }

  x
}

# Helper: resolve the by argument to a character vector of x-side key names.
# Used by semi_join, anti_join, and inner_join for the domain sentinel.
.resolve_by_to_x_names <- function(by, x_data, y) {
  if (is.null(by)) {
    intersect(names(x_data), names(y))
  } else if (is.character(by)) {
    # by can be c("key") or c(x_col = "y_col"); take the names if named, else
    # the values
    if (!is.null(names(by)) && any(nchar(names(by)) > 0L)) {
      # Named: c(x_key = "y_key") — x side is the names
      x_keys <- names(by)[nchar(names(by)) > 0L]
      y_keys <- unname(by[nchar(names(by)) > 0L])
      # Unnamed entries: x and y keys are the same
      unnamed <- by[nchar(names(by)) == 0L]
      c(x_keys, unname(unnamed))
    } else {
      unname(by)
    }
  } else {
    by$x # dplyr::join_by() object
  }
}


# ── left_join ─────────────────────────────────────────────────────────────────

#' Add columns from a data frame to a survey design
#'
#' @description
#' `left_join()` adds columns from a plain data frame `y` to a survey design
#' object `x`, matching on keys defined by `by`. All rows of `x` are preserved
#' (left join semantics). Rows with no match in `y` receive `NA` for the new
#' columns.
#'
#' @details
#' ## Design integrity
#' `y` must be a plain data frame, not a survey object. If `y` has column names
#' that match any design variable in `x` (weights, strata, PSU, FPC,
#' replicate weights, or the domain column), those columns are dropped from `y`
#' with a warning before joining. Join keys in `by` are excluded from this
#' check.
#'
#' ## Row count
#' `left_join()` errors if `y` has duplicate keys that would expand `x` beyond
#' its original row count. Duplicate respondent rows corrupt variance
#' estimation. Deduplicate `y` with `dplyr::distinct()` before joining.
#'
#' ## Metadata
#' New columns from `y` receive no variable labels in `@metadata`. If a column
#' in `x@data` is suffix-renamed because `y` has a non-design column with the
#' same name, the corresponding `@metadata@variable_labels` key is updated to
#' the new suffixed name.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param y A plain data frame with lookup columns. Must not be a survey
#'   object. Must not have column names matching any design variable in `x`
#'   (those are dropped with a warning).
#' @param by A character vector of column names or a [dplyr::join_by()]
#'   specification. `NULL` uses all common column names.
#' @param copy Forwarded to [dplyr::left_join()].
#' @param suffix A character vector of length 2 appended to deduplicate column
#'   names shared between `x` and `y`. Forwarded to [dplyr::left_join()].
#' @param ... Additional arguments forwarded to [dplyr::left_join()].
#' @param keep Forwarded to [dplyr::left_join()].
#'
#' @return
#' A survey design object of the same type as `x` with new columns from `y`
#' appended to `@data`. `visible_vars` is updated if it was set.
#'
#' @examples
#' # create a small survey object
#' df <- data.frame(
#'   psu = paste0("psu_", 1:5),
#'   strata = "s1",
#'   fpc = 100,
#'   wt = 1,
#'   y1 = 1:5
#' )
#' d <- surveycore::as_survey(
#'   df,
#'   ids = psu,
#'   weights = wt,
#'   strata = strata,
#'   fpc = fpc,
#'   nest = TRUE
#' )
#'
#' # add a lookup column from a plain data frame
#' lookup <- data.frame(y1 = 1:5, label = letters[1:5])
#' left_join(d, lookup, by = "y1")
#'
#' @family joins
#' @name left_join
NULL

#' @rdname left_join
#' @method left_join survey_base
#' @noRd
left_join.survey_base <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  # Step 1: Guard — y must not be a survey object
  .check_join_y_type(y)

  # Step 2: Guard — y must not have design-variable columns
  y <- .check_join_col_conflict(x, y, by)

  # Step 3: Capture old column names before join
  old_x_cols <- names(x@data)
  original_nrow <- nrow(x@data)

  # Step 4: Run join and guard row expansion
  result <- dplyr::left_join(
    x@data,
    y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )
  .check_join_row_expansion(original_nrow, nrow(result))

  # Assign new data
  x@data <- result

  # Step 4b: Detect and repair suffix renames in @metadata and visible_vars
  x <- .repair_suffix_renames(x, old_x_cols, suffix)

  # Step 5: Update visible_vars if set (append new columns from y)
  if (!is.null(x@variables$visible_vars)) {
    # New columns: in result but not in old_x_cols, and not suffix-renamed
    # versions of old columns (those were already handled by .repair_suffix_renames)
    new_cols <- setdiff(names(x@data), old_x_cols)
    if (length(new_cols) > 0L) {
      x@variables$visible_vars <- c(x@variables$visible_vars, new_cols)
    }
  }

  # Step 6: Domain column — unchanged (left join preserves all rows)
  x
}


# ── semi_join ─────────────────────────────────────────────────────────────────

#' Domain-aware semi- and anti-join for survey designs
#'
#' @description
#' `semi_join()` marks rows as in-domain when they have a match in `y`.
#' `anti_join()` marks rows as in-domain when they do NOT have a match in `y`.
#' Neither function removes rows or adds new columns — they are implemented as
#' domain operations, exactly like [filter()].
#'
#' @details
#' ## Domain awareness
#' Unlike standard `dplyr::semi_join()` and `dplyr::anti_join()`, these
#' implementations never physically remove rows. Instead, unmatched (or matched,
#' for `anti_join`) rows are marked `FALSE` in the `..surveycore_domain..`
#' column of `@data`, exactly as [filter()] does. This preserves variance
#' estimation validity.
#'
#' ## Chaining
#' Multiple calls accumulate via AND: a row must satisfy every condition from
#' every `filter()`, `semi_join()`, and `anti_join()` call to remain in-domain.
#'
#' ## Duplicate keys in y
#' Duplicate keys in `y` collapse to a single `TRUE` (for `semi_join`) or a
#' single `FALSE` (for `anti_join`) per survey row. Row expansion is not
#' possible with these functions.
#'
#' ## @variables$domain sentinel
#' A typed S3 sentinel of class `"surveytidy_join_domain"` is appended to
#' `@variables$domain`. Phase 1 consumers can use
#' `inherits(entry, "surveytidy_join_domain")` to distinguish join sentinels
#' from quosures.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param y A plain data frame. Must not be a survey object.
#' @param by A character vector of column names or a [dplyr::join_by()]
#'   specification. `NULL` uses all common column names.
#' @param copy Forwarded to the underlying dplyr function.
#' @param ... Additional arguments forwarded to the underlying dplyr function.
#'
#' @return
#' A survey design object of the same type as `x` with the domain column
#' (`..surveycore_domain..`) updated. Row count unchanged. No new columns added.
#'
#' @examples
#' # create a small survey object
#' df <- data.frame(
#'   psu = paste0("psu_", 1:5),
#'   strata = "s1",
#'   fpc = 100,
#'   wt = 1,
#'   y1 = 1:5
#' )
#' d <- surveycore::as_survey(
#'   df,
#'   ids = psu,
#'   weights = wt,
#'   strata = strata,
#'   fpc = fpc,
#'   nest = TRUE
#' )
#' keepers <- data.frame(y1 = c(1, 3, 5))
#'
#' # semi_join: rows matching keepers stay in-domain
#' semi_join(d, keepers, by = "y1")
#'
#' # anti_join: rows matching keepers are marked out-of-domain
#' anti_join(d, keepers, by = "y1")
#'
#' @family joins
#' @name semi_join
NULL

#' @rdname semi_join
#' @method semi_join survey_base
#' @noRd
semi_join.survey_base <- function(x, y, by = NULL, copy = FALSE, ...) {
  # Step 1: Guard — y must not be a survey object
  .check_join_y_type(y)

  # Step 2: Guard — reserved column name check
  if ("..surveytidy_row_index.." %in% names(x@data)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x@data} contains a reserved internal column ",
          "{.field ..surveytidy_row_index..} that conflicts with masking logic."
        ),
        "i" = "This column name is reserved for internal use by surveytidy.",
        "v" = paste0(
          "Rename the column in your data before passing it to ",
          "{.fn semi_join} or {.fn anti_join}."
        )
      ),
      class = "surveytidy_error_reserved_col_name"
    )
  }

  # Compute match mask using row-index approach
  x_temp <- x@data
  x_temp[["..surveytidy_row_index.."]] <- seq_len(nrow(x@data))
  matched <- dplyr::semi_join(x_temp, y, by = by, copy = copy, ...)
  new_mask <- seq_len(nrow(x@data)) %in%
    matched[["..surveytidy_row_index.."]]

  # Step 3: AND with existing domain
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  existing <- x@data[[domain_col]]
  if (is.null(existing)) {
    existing <- rep(TRUE, nrow(x@data))
  }
  new_domain <- existing & new_mask
  x@data[[domain_col]] <- new_domain

  # Step 4: Empty domain check
  if (!any(new_domain)) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "semi_join() produced an empty domain \u2014 no rows match."
        ),
        "i" = "Variance estimation on this domain will fail."
      ),
      class = "surveycore_warning_empty_domain"
    )
  }

  # Step 6: Append domain sentinel
  resolved_by <- .resolve_by_to_x_names(by, x@data, y)
  sentinel <- .new_join_domain_sentinel("semi_join", resolved_by)
  x@variables$domain <- c(x@variables$domain, list(sentinel))

  x
}

#' @rdname semi_join
#' @name anti_join
NULL

#' @rdname semi_join
#' @method anti_join survey_base
#' @noRd
anti_join.survey_base <- function(x, y, by = NULL, copy = FALSE, ...) {
  # Step 1: Guard — y must not be a survey object
  .check_join_y_type(y)

  # Step 2: Guard — reserved column name check
  if ("..surveytidy_row_index.." %in% names(x@data)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x@data} contains a reserved internal column ",
          "{.field ..surveytidy_row_index..} that conflicts with masking logic."
        ),
        "i" = "This column name is reserved for internal use by surveytidy.",
        "v" = paste0(
          "Rename the column in your data before passing it to ",
          "{.fn semi_join} or {.fn anti_join}."
        )
      ),
      class = "surveytidy_error_reserved_col_name"
    )
  }

  # Compute match mask using row-index approach (same as semi_join)
  x_temp <- x@data
  x_temp[["..surveytidy_row_index.."]] <- seq_len(nrow(x@data))
  matched <- dplyr::semi_join(x_temp, y, by = by, copy = copy, ...)
  # new_mask is TRUE for matched rows; anti_join wants the complement
  new_mask <- seq_len(nrow(x@data)) %in%
    matched[["..surveytidy_row_index.."]]

  # Step 3: AND with existing domain (anti_join negates the mask)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  existing <- x@data[[domain_col]]
  if (is.null(existing)) {
    existing <- rep(TRUE, nrow(x@data))
  }
  new_domain <- existing & !new_mask
  x@data[[domain_col]] <- new_domain

  # Step 4: Empty domain check
  if (!any(new_domain)) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "anti_join() produced an empty domain \u2014 all rows were excluded."
        ),
        "i" = "Variance estimation on this domain will fail."
      ),
      class = "surveycore_warning_empty_domain"
    )
  }

  # Step 6: Append domain sentinel
  resolved_by <- .resolve_by_to_x_names(by, x@data, y)
  sentinel <- .new_join_domain_sentinel("anti_join", resolved_by)
  x@variables$domain <- c(x@variables$domain, list(sentinel))

  x
}


# ── bind_cols ─────────────────────────────────────────────────────────────────

#' Append columns to a survey design by position
#'
#' @description
#' `bind_cols()` appends columns from one or more plain data frames to a survey
#' design object, matching by row position. This is equivalent to an implicit
#' row-index `left_join()`. All rows are preserved; row count is unchanged.
#'
#' When `x` is not a survey object, this function delegates to
#' [dplyr::bind_cols()] transparently.
#'
#' @details
#' ## Design integrity
#' None of the objects in `...` may be a survey object. If any new column name
#' matches a design variable in `x`, that column is dropped with a warning.
#' All inputs in `...` must have exactly the same number of rows as `x`.
#'
#' ## Dispatch note
#' `dplyr::bind_cols()` uses `vctrs::vec_cbind()` internally and does not
#' dispatch via S3 on `x`. surveytidy provides its own `bind_cols()` that
#' intercepts survey objects before delegating to dplyr.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object, or any object
#'   accepted by [dplyr::bind_cols()].
#' @param ... One or more plain data frames or named lists. When `x` is a
#'   survey object, none of the objects may be survey objects.
#' @param .name_repair Forwarded to [dplyr::bind_cols()].
#'
#' @return
#' When `x` is a survey object: a survey design object of the same type as `x`
#' with new columns appended to `@data`. `visible_vars` is updated if it was
#' set. When `x` is not a survey object: the result of [dplyr::bind_cols()].
#'
#' @examples
#' library(surveytidy)
#'
#' # create a small survey object
#' df <- data.frame(
#'   psu = paste0("psu_", 1:5),
#'   strata = "s1",
#'   fpc = 100,
#'   wt = 1,
#'   y1 = 1:5
#' )
#' d <- surveycore::as_survey(
#'   df,
#'   ids = psu,
#'   weights = wt,
#'   strata = strata,
#'   fpc = fpc,
#'   nest = TRUE
#' )
#'
#' # append a new column by row position
#' extra <- data.frame(label = letters[1:5])
#' bind_cols(d, extra)
#'
#' @family joins
#' @export
bind_cols <- function(x, ..., .name_repair = "unique") {
  # When x is not a survey object, delegate to dplyr::bind_cols transparently
  if (!S7::S7_inherits(x, surveycore::survey_base)) {
    return(dplyr::bind_cols(x, ..., .name_repair = .name_repair))
  }
  bind_cols.survey_base(x, ..., .name_repair = .name_repair)
}

#' @noRd
bind_cols.survey_base <- function(x, ..., .name_repair = "unique") {
  # Step 1: Guard — none of ... may be a survey object
  dots <- list(...)
  for (obj in dots) {
    if (S7::S7_inherits(obj, surveycore::survey_base)) {
      cli::cli_abort(
        c(
          "x" = "Survey objects cannot be combined with {.fn bind_cols}.",
          "i" = "One or more objects in {.arg ...} is a survey design object.",
          "v" = paste0(
            "Extract {.code @data} from each survey object and bind the raw ",
            "data frames instead."
          )
        ),
        class = "surveytidy_error_join_survey_to_survey"
      )
    }
  }

  # Step 2: Guard — column conflict with design variables
  # First bind all ... together to check for conflicts
  combined_y <- dplyr::bind_cols(...)
  cleaned_y <- .check_join_col_conflict(x, combined_y, by = character(0))

  # Step 3: Guard — row count must match exactly
  if (nrow(cleaned_y) != nrow(x@data)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn bind_cols} requires all inputs to have the same number of rows."
        ),
        "i" = paste0(
          "{.arg x} has {nrow(x@data)} row{?s}; the new data has ",
          "{nrow(cleaned_y)} row{?s}."
        ),
        "v" = paste0(
          "Ensure the data frame you are binding has exactly ",
          "{nrow(x@data)} row{?s} before calling {.fn bind_cols}."
        )
      ),
      class = "surveytidy_error_bind_cols_row_mismatch"
    )
  }

  # Step 4: Bind and update visible_vars
  old_x_cols <- names(x@data)
  x@data <- dplyr::bind_cols(x@data, cleaned_y, .name_repair = .name_repair)

  # Update visible_vars if set (append new columns)
  if (!is.null(x@variables$visible_vars)) {
    new_cols <- setdiff(names(x@data), old_x_cols)
    if (length(new_cols) > 0L) {
      x@variables$visible_vars <- c(x@variables$visible_vars, new_cols)
    }
  }

  x
}


# ── inner_join ────────────────────────────────────────────────────────────────

#' Domain-aware inner join for survey designs
#'
#' @description
#' `inner_join()` has two modes controlled by `.domain_aware` (default `TRUE`):
#'
#' **Domain-aware mode (`.domain_aware = TRUE`, default):** Unmatched rows are
#' marked `FALSE` in the domain column (exactly like [filter()] or [semi_join()]),
#' and `y`'s columns are added to all rows (with `NA` for unmatched rows). All
#' rows remain in `@data`. Row count is unchanged. This is the survey-correct
#' default.
#'
#' **Physical mode (`.domain_aware = FALSE`):** Unmatched rows are physically
#' removed, exactly like base R `inner_join`. Emits
#' `surveycore_warning_physical_subset`. Errors for `survey_twophase` designs.
#'
#' @details
#' ## Choosing a mode
#' The domain-aware default preserves variance estimation validity. The
#' `nrow()` behaviour (count stays the same) is consistent with [filter()] and
#' [semi_join()] precedents in surveytidy.
#'
#' Physical mode (`.domain_aware = FALSE`) is appropriate only when you
#' explicitly want to reduce the design to a specific subpopulation. For
#' replicate designs (BRR, jackknife), physical row removal can corrupt
#' half-sample or pairing structure, producing numerically wrong variance
#' estimates. Domain-aware mode is recommended for replicate designs.
#'
#' ## Duplicate keys
#' Duplicate keys in `y` that would expand the row count are an error in both
#' modes. Deduplicate `y` with `dplyr::distinct()` before joining.
#'
#' @details
#' ## The `.domain_aware` argument (survey-specific extension)
#' The surveytidy method adds one argument not present in the dplyr generic:
#' `.domain_aware = TRUE` (default) performs domain-aware joining; set
#' `.domain_aware = FALSE` for physical row removal (emits
#' `surveycore_warning_physical_subset`; errors for `survey_twophase`).
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param y A plain data frame. Must not be a survey object.
#' @param by A character vector of column names or a [dplyr::join_by()]
#'   specification. `NULL` uses all common column names.
#' @param copy Forwarded to the underlying dplyr function.
#' @param suffix A character vector of length 2. Forwarded to the underlying
#'   dplyr function for handling shared column names.
#' @param ... Additional arguments forwarded to the underlying dplyr function.
#' @param keep Forwarded to the underlying dplyr function.
#'
#' @return
#' A survey design object of the same type as `x`.
#' - Domain-aware mode (`.domain_aware = TRUE`): row count unchanged;
#'   `..surveycore_domain..` updated; new columns from `y` appended.
#' - Physical mode (`.domain_aware = FALSE`): row count reduced to matched
#'   rows; new columns from `y` appended.
#'
#' @examples
#' # create a small survey object
#' df <- data.frame(
#'   psu = paste0("psu_", 1:5),
#'   strata = "s1",
#'   fpc = 100,
#'   wt = 1,
#'   y1 = 1:5
#' )
#' d <- surveycore::as_survey(
#'   df,
#'   ids = psu,
#'   weights = wt,
#'   strata = strata,
#'   fpc = fpc,
#'   nest = TRUE
#' )
#' lookup <- data.frame(y1 = 1:3, label = letters[1:3])
#'
#' # domain-aware: marks rows 4 and 5 as out-of-domain
#' inner_join(d, lookup, by = "y1")
#'
#' # physical: removes rows 4 and 5
#' inner_join(d, lookup, by = "y1", .domain_aware = FALSE)
#'
#' @family joins
#' @name inner_join
NULL

#' @rdname inner_join
#' @method inner_join survey_base
#' @noRd
inner_join.survey_base <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  .domain_aware = TRUE
) {
  if (.domain_aware) {
    # ── Domain-aware mode ──────────────────────────────────────────────────

    # Step 1: Guard — y must not be a survey object
    .check_join_y_type(y)

    # Step 2: Guard — column conflict
    y <- .check_join_col_conflict(x, y, by)

    # Step 3: Guard — reserved column name for row-index approach
    if ("..surveytidy_row_index.." %in% names(x@data)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg x@data} contains a reserved internal column ",
            "{.field ..surveytidy_row_index..} that conflicts with masking ",
            "logic."
          ),
          "i" = "This column name is reserved for internal use by surveytidy.",
          "v" = paste0(
            "Rename the column in your data before passing it to ",
            "{.fn inner_join}."
          )
        ),
        class = "surveytidy_error_reserved_col_name"
      )
    }

    # Compute match mask using row-index approach (same as semi_join)
    x_temp <- x@data
    x_temp[["..surveytidy_row_index.."]] <- seq_len(nrow(x@data))
    matched <- dplyr::semi_join(x_temp, y, by = by, copy = copy, ...)
    match_mask <- seq_len(nrow(x@data)) %in%
      matched[["..surveytidy_row_index.."]]

    # Step 4: AND with existing domain
    domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
    existing <- x@data[[domain_col]]
    if (is.null(existing)) {
      existing <- rep(TRUE, nrow(x@data))
    }
    new_domain <- existing & match_mask
    x@data[[domain_col]] <- new_domain

    # Step 5: Empty domain check
    if (!any(new_domain)) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "inner_join() produced an empty domain \u2014 no rows match."
          ),
          "i" = "Variance estimation on this domain will fail."
        ),
        class = "surveycore_warning_empty_domain"
      )
    }

    # Step 6: Left join for new columns
    old_x_cols <- names(x@data)
    original_nrow <- nrow(x@data)
    result <- dplyr::left_join(
      x@data,
      y,
      by = by,
      copy = copy,
      suffix = suffix,
      ...,
      keep = keep
    )
    .check_join_row_expansion(original_nrow, nrow(result))
    x@data <- result

    # Repair suffix renames and update visible_vars
    x <- .repair_suffix_renames(x, old_x_cols, suffix)
    if (!is.null(x@variables$visible_vars)) {
      new_cols <- setdiff(names(x@data), old_x_cols)
      if (length(new_cols) > 0L) {
        x@variables$visible_vars <- c(x@variables$visible_vars, new_cols)
      }
    }

    # Step 7: Append domain sentinel
    resolved_by <- .resolve_by_to_x_names(by, x@data, y)
    sentinel <- .new_join_domain_sentinel("inner_join", resolved_by)
    x@variables$domain <- c(x@variables$domain, list(sentinel))
  } else {
    # ── Physical mode ──────────────────────────────────────────────────────

    # Step 1: Guard — y must not be a survey object
    .check_join_y_type(y)

    # Step 2: Guard — twophase designs
    if (S7::S7_inherits(x, surveycore::survey_twophase)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.code inner_join(.domain_aware = FALSE)} cannot physically ",
            "remove rows from a two-phase design."
          ),
          "i" = paste0(
            "Removing rows from a two-phase design can orphan phase 2 rows ",
            "or corrupt the phase 1 sample frame."
          ),
          "v" = paste0(
            "Use {.code .domain_aware = TRUE} (the default) or ",
            "{.fn semi_join} for domain-aware filtering."
          )
        ),
        class = "surveytidy_error_join_twophase_row_removal"
      )
    }

    # Step 3: Guard — column conflict
    y <- .check_join_col_conflict(x, y, by)

    # Step 4: Guard — row count must not expand; also computes the join result
    original_nrow <- nrow(x@data)
    result <- dplyr::inner_join(
      x@data,
      y,
      by = by,
      copy = copy,
      suffix = suffix,
      ...,
      keep = keep
    )
    .check_join_row_expansion(original_nrow, nrow(result))

    # Step 5: Result already computed in Step 4

    # Step 6: Physical subset warning (inline — not .warn_physical_subset)
    removed_n <- original_nrow - nrow(result)
    cli::cli_warn(
      c(
        "!" = paste0(
          "{.fn inner_join} physically removed {removed_n} row{?s} from the ",
          "survey design."
        ),
        "i" = "Physical row removal can bias variance estimation.",
        "i" = paste0(
          "Use {.code .domain_aware = TRUE} (the default) to mark rows as ",
          "out-of-domain without removing them."
        )
      ),
      class = "surveycore_warning_physical_subset"
    )

    # Step 7: Empty result guard
    if (nrow(result) == 0L) {
      cli::cli_abort(
        c(
          "x" = "inner_join() condition matched 0 rows.",
          "i" = "Survey objects require at least 1 row.",
          "v" = "Use {.fn semi_join} for domain estimation (keeps all rows)."
        ),
        class = "surveytidy_error_subset_empty_result"
      )
    }

    x@data <- result
  }

  x
}


# ── right_join ────────────────────────────────────────────────────────────────

#' Unsupported joins for survey designs
#'
#' @description
#' `right_join()` and `full_join()` error unconditionally for survey design
#' objects because they can add rows from `y` that have no match in the survey.
#' Those new rows would have `NA` for all design variables (weights, strata,
#' PSU), producing an invalid design object.
#'
#' @details
#' Use [left_join()] to add lookup columns from `y`. Use [filter()] or
#' [semi_join()] to restrict the survey domain.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param y A data frame or survey object.
#' @param by Ignored — the function always errors.
#' @param copy Ignored — the function always errors.
#' @param suffix Ignored — the function always errors.
#' @param keep Ignored — the function always errors.
#' @param ... Additional arguments (ignored; the function always errors).
#'
#' @return Never returns — always throws an error.
#'
#' @examples
#' # create a tiny survey object and a lookup table with an extra row
#' d <- surveycore::as_survey(
#'   data.frame(wt = c(1, 1), y1 = c(1, 2)),
#'   weights = wt
#' )
#' lookup <- data.frame(y1 = c(1, 2, 3), label = c("a", "b", "c"))
#'
#' # right_join() and full_join() always error on a survey object — they would
#' # add rows with NA design variables, producing an invalid design
#' tryCatch(
#'   right_join(d, lookup, by = "y1"),
#'   error = function(e) message(conditionMessage(e))
#' )
#'
#' tryCatch(
#'   full_join(d, lookup, by = "y1"),
#'   error = function(e) message(conditionMessage(e))
#' )
#'
#' # the recommended alternative: use left_join() to add lookup columns
#' # without changing the row set
#' left_join(d, lookup, by = "y1")
#'
#' @family joins
#' @name right_join
NULL

#' @rdname right_join
#' @method right_join survey_base
#' @noRd
right_join.survey_base <- function(x, y, ...) {
  .check_join_y_type(y)
  fn_name <- "right_join"
  cli::cli_abort(
    c(
      "x" = paste0(
        "{.fn {fn_name}} would add rows from {.arg y} that have no match ",
        "in the survey."
      ),
      "i" = paste0(
        "New rows would have {.code NA} for all design variables (weights, ",
        "strata, PSU), producing an invalid design object."
      ),
      "v" = paste0(
        "Use {.fn left_join} to add lookup columns from {.arg y}, or ",
        "{.fn filter} / {.fn semi_join} to restrict the survey domain."
      )
    ),
    class = "surveytidy_error_join_adds_rows"
  )
}

#' @rdname right_join
#' @name full_join
NULL

#' @rdname right_join
#' @method full_join survey_base
#' @noRd
full_join.survey_base <- function(x, y, ...) {
  .check_join_y_type(y)
  fn_name <- "full_join"
  cli::cli_abort(
    c(
      "x" = paste0(
        "{.fn {fn_name}} would add rows from {.arg y} that have no match ",
        "in the survey."
      ),
      "i" = paste0(
        "New rows would have {.code NA} for all design variables (weights, ",
        "strata, PSU), producing an invalid design object."
      ),
      "v" = paste0(
        "Use {.fn left_join} to add lookup columns from {.arg y}, or ",
        "{.fn filter} / {.fn semi_join} to restrict the survey domain."
      )
    ),
    class = "surveytidy_error_join_adds_rows"
  )
}


# ── bind_rows ─────────────────────────────────────────────────────────────────

#' Stack surveys with bind_rows (errors unconditionally)
#'
#' @description
#' `bind_rows()` errors unconditionally when the first argument is a survey
#' design object. Stacking two surveys changes the design — the combined object
#' requires a new design specification (e.g., a new survey-wave stratum).
#'
#' When the first argument is not a survey object, this function delegates to
#' [dplyr::bind_rows()] transparently.
#'
#' @details
#' **Known limitation:** If the survey object is passed as a **non-first**
#' argument (e.g., `bind_rows(df, survey)`), this function delegates to
#' `dplyr::bind_rows(df, survey)` which will fail with a dplyr/vctrs error
#' rather than the survey-specific error. Always pass the survey object as the
#' first argument to ensure the correct error is triggered.
#'
#' ## Dispatch note
#' `dplyr::bind_rows()` uses `vctrs::vec_rbind()` internally for recent dplyr
#' versions and does not reliably dispatch via S3 on `x` for S7 objects.
#' surveytidy provides its own `bind_rows()` that intercepts survey objects
#' before delegating to dplyr (GAP-6 verified: S3 dispatch does not work;
#' standalone function approach used instead).
#'
#' @param x A [`survey_base`][surveycore::survey_base] object (always errors),
#'   or any object accepted by [dplyr::bind_rows()] (transparent delegation).
#' @param ... Additional arguments.
#' @param .id Forwarded to [dplyr::bind_rows()].
#'
#' @return Never returns when `x` is a survey object — always throws an error.
#'   When `x` is not a survey object, returns the result of [dplyr::bind_rows()].
#'
#' @examples
#' # NOTE: do not load dplyr here — its bind_rows() would mask surveytidy's
#' # bind_rows() and bypass the survey-object check shown below.
#'
#' # two raw data frames that together define a combined survey
#' df1 <- data.frame(wt = c(1, 1), y1 = c(1, 2))
#' df2 <- data.frame(wt = c(1, 1), y1 = c(3, 4))
#'
#' # bind_rows() on plain data frames delegates to dplyr::bind_rows()
#' bind_rows(df1, df2)
#'
#' # but bind_rows() on a survey object always errors — stacking two surveys
#' # would change the design, requiring a new design specification
#' d1 <- surveycore::as_survey(df1, weights = wt)
#'
#' tryCatch(
#'   bind_rows(d1, df2),
#'   error = function(e) message(conditionMessage(e))
#' )
#'
#' # the recommended workflow: extract raw data from each survey, bind, then
#' # re-specify the design on the combined data frame
#' combined <- bind_rows(
#'   surveycore::survey_data(d1),
#'   df2
#' )
#' surveycore::as_survey(combined, weights = wt)
#'
#' @family joins
#' @export
bind_rows <- function(x, ..., .id = NULL) {
  if (S7::S7_inherits(x, surveycore::survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.fn bind_rows} cannot stack survey design objects.",
        "i" = paste0(
          "Stacking two surveys changes the design \u2014 the combined ",
          "object requires a new design specification."
        ),
        "v" = paste0(
          "Extract {.code @data} from each survey object with ",
          "{.fn surveycore::survey_data}, bind the raw data frames with ",
          "{.fn dplyr::bind_rows}, then re-specify the combined design with ",
          "{.fn surveycore::as_survey}."
        )
      ),
      class = "surveytidy_error_bind_rows_survey"
    )
  }
  dplyr::bind_rows(x, ..., .id = .id)
}

#' @noRd
bind_rows.survey_base <- function(x, ..., .id = NULL) {
  bind_rows(x, ..., .id = .id)
}


# ── survey_collection: unsupported join verbs (PR 4) ──────────────────────────

# Shared internal helper for all 6 *_join.survey_collection error stubs.
# All join semantics on a survey_collection are unsupported — the contract for
# how a join should compose with a multi-survey container (apply per member?
# broadcast across all? error if y has rows that match no member?) is not yet
# resolved. Error early and direct users to apply the join inside a per-survey
# pipeline before constructing the collection.
.collection_join_unsupported <- function(verb_name) {
  cli::cli_abort(
    c(
      "x" = "{.fn {verb_name}} on a {.cls survey_collection} is not supported.",
      "i" = paste0(
        "The semantics (apply to each survey? broadcast across all?) are ",
        "still being designed."
      ),
      "v" = paste0(
        "Apply the join inside a per-survey pipeline before constructing ",
        "the collection."
      )
    ),
    class = "surveytidy_error_collection_verb_unsupported",
    call = rlang::caller_env()
  )
}

#' @rdname left_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `left_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method left_join survey_collection
left_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("left_join")
}

#' @rdname right_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `right_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method right_join survey_collection
right_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("right_join")
}

#' @rdname inner_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `inner_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method inner_join survey_collection
inner_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("inner_join")
}

#' @rdname right_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `full_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method full_join survey_collection
full_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("full_join")
}

#' @rdname semi_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `semi_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method semi_join survey_collection
semi_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("semi_join")
}

#' @rdname semi_join
#'
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When called on a [`surveycore::survey_collection`], `anti_join()` errors
#' unconditionally with class
#' `surveytidy_error_collection_verb_unsupported`. The semantics for joining
#' a plain data frame onto a multi-survey container are still being designed.
#' Apply the join inside a per-survey pipeline before constructing the
#' collection.
#'
#' @method anti_join survey_collection
anti_join.survey_collection <- function(x, y, ..., .if_missing_var = NULL) {
  .collection_join_unsupported("anti_join")
}
