# R/mutate.R
#
# mutate() for survey design objects.
#
# Delegates the actual column computation to dplyr::mutate() on @data, then
# re-attaches any protected columns that dplyr dropped (e.g., via .keep =
# "none"), updates visible_vars, and records new column transformations in
# @metadata.
#
# Design variable modification is detected by name: if a mutation's LHS name
# matches a protected column, a warning is issued. Note that across() calls
# that modify design variables will NOT trigger this warning — the limitation
# is documented and accepted for Phase 0.5.
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   mutate.survey_base()   — column computation with design-var preservation
#   mutate.survey_result() — class/meta preservation for survey_result

# ── mutate() ──────────────────────────────────────────────────────────────────

#' Create, modify, and delete columns of a survey design object
#'
#' @description
#' `mutate()` adds new columns or modifies existing ones while preserving the
#' survey design structure required for valid variance estimation. It delegates
#' column computation to `dplyr::mutate()` on the underlying data.
#'
#' Use `NULL` as a value to delete a column. Design variables (weights,
#' strata, PSUs) cannot be deleted this way — they are always preserved.
#'
#' @details
#' ## Design variable modification
#' If the left-hand side of a mutation names a design variable (e.g.,
#' `mutate(d, wt = wt * 2)`), a `surveytidy_warning_mutate_design_var`
#' warning is issued. Detection is name-based: `across()` calls that happen
#' to modify design variables will **not** trigger the warning.
#'
#' ## `.keep` and design variables
#' Design variables (weights, strata, PSUs, FPC, replicate weights, and the
#' domain column) are always preserved in the output, regardless of `.keep`.
#' This ensures variance estimation remains valid even when `.keep = "none"`.
#'
#' ## Grouped mutate
#' Grouping set by [group_by()] is respected automatically — leave `.by =
#' NULL` (the default) and mutate expressions will compute within groups.
#' The `.by` argument is not used directly.
#'
#' ## Useful mutate functions
#' * Arithmetic: `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`
#' * Rounding: [round()], [floor()], [ceiling()], [trunc()]
#' * Ranking: [dplyr::dense_rank()], [dplyr::min_rank()], [dplyr::row_number()]
#' * Cumulative: [cumsum()], [cummax()], [cummin()], [cummean()]
#' * Conditional: [dplyr::if_else()], [dplyr::case_when()], [dplyr::case_match()]
#' * Missing values: [dplyr::na_if()], [dplyr::coalesce()]
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the output column name; the value is an expression
#'   evaluated against the survey data. Use `NULL` to delete a non-design
#'   column.
#' @param .by Not used directly. Set grouping with [group_by()] instead.
#'   When `@groups` is non-empty and `.by` is `NULL` (the default), the
#'   active groups are applied automatically.
#' @param .keep Which columns to retain. One of `"all"` (default), `"used"`,
#'   `"unused"`, or `"none"`. Design variables are always re-attached
#'   regardless of this argument.
#' @param .before,.after <[`tidy-select`][tidyselect::language]> Optionally
#'   position new columns before or after an existing one.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * Rows are not added or removed.
#' * Columns are retained, modified, or removed per `...` and `.keep`.
#' * Design variables (weights, strata, PSUs) are always present.
#' * Groups and survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # add a new column
#' mutate(d, college_grad = educcat == 1)
#'
#' # conditional recoding
#' mutate(
#'   d,
#'   college = dplyr::if_else(educcat == 1, "college+", "non-college")
#' )
#'
#' # grouped mutate — within-group mean centring
#' d |>
#'   group_by(gender) |>
#'   mutate(econ_centred = econ1mod - mean(econ1mod, na.rm = TRUE))
#'
#' # .keep = "none" keeps only new columns plus design vars (always preserved)
#' mutate(
#'   d,
#'   college = dplyr::if_else(educcat == 1, "college+", "non-college"),
#'   .keep = "none"
#' )
#'
#' @family modification
#' @seealso [rename()] to rename columns, [select()] to drop columns
#' @name mutate
NULL

#' @rdname mutate
#' @method mutate survey_base
mutate.survey_base <- function(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
) {
  .keep <- match.arg(.keep)

  # Step 1a: Determine base_data and effective_by based on rowwise / grouped
  # mode. Rowwise mode takes precedence over grouping.
  rowwise_mode <- isTRUE(.data@variables$rowwise)
  id_cols <- .data@variables$rowwise_id_cols %||% character(0)
  group_names <- .data@groups

  # Step 1b: Detect design variable modification by name.
  # Split into two warning classes:
  #   - weight column:  surveytidy_warning_mutate_weight_col
  #   - structural vars (strata, PSU, FPC, repweights):
  #     surveytidy_warning_mutate_structural_var
  # Only explicitly-named LHS expressions are detected. across() and other
  # multi-output expressions are NOT detected — accepted limitation.
  mutations <- rlang::quos(...)
  mutated_names <- names(mutations)

  weight_var <- if (S7::S7_inherits(.data, surveycore::survey_twophase)) {
    .data@variables$phase1$weights
  } else {
    .data@variables$weights
  }
  structural_vars <- setdiff(.survey_design_var_names(.data), weight_var)
  structural_vars <- intersect(structural_vars, names(.data@data))

  changed_weight <- intersect(mutated_names, weight_var)
  changed_structural <- intersect(mutated_names, structural_vars)

  if (length(changed_weight) > 0L) {
    cli::cli_warn(
      c(
        "!" = "mutate() modified weight column {.field {changed_weight}}.",
        "i" = "Effective sample size may be affected.",
        "v" = "Use {.fn update_design} to intentionally change design variables."
      ),
      class = "surveytidy_warning_mutate_weight_col"
    )
  }
  if (length(changed_structural) > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "mutate() modified structural design variable(s): ",
          "{.field {changed_structural}}."
        ),
        "i" = "Structural recoding can invalidate variance estimates.",
        "i" = paste0(
          "Use {.fn subset} or {.fn filter} to restrict the domain; ",
          "do not recode design variables."
        )
      ),
      class = "surveytidy_warning_mutate_structural_var"
    )
  }

  # Step 2: Pre-attach label attrs from @metadata so recode functions can
  # read them via attr(x, "labels") / attr(x, "label").
  augmented_data <- .attach_metadata_attrs(.data@data, .data@metadata)

  if (rowwise_mode) {
    effective_by <- NULL
    base_data <- if (length(id_cols) > 0L) {
      dplyr::rowwise(augmented_data, dplyr::all_of(id_cols))
    } else {
      dplyr::rowwise(augmented_data)
    }
  } else if (is.null(.by) && length(group_names) > 0L) {
    base_data <- augmented_data
    effective_by <- group_names
  } else {
    base_data <- augmented_data
    effective_by <- .by
  }

  # Step 3: Run the mutation on augmented_data (was on @data in Phase 0.5).
  # Capture .before and .after as quosures so NSE column-name expressions
  # (e.g., .before = y2) are forwarded correctly via rlang::inject().
  # Also: dplyr 1.2.0 errors when .before AND .after are both passed explicitly
  # (even as NULL), and tidyselect warns when effective_by = NULL is passed.
  before_quo <- rlang::enquo(.before)
  after_quo <- rlang::enquo(.after)
  has_before <- !rlang::quo_is_null(before_quo)
  has_after <- !rlang::quo_is_null(after_quo)
  has_by <- !is.null(effective_by)

  new_data <- rlang::inject(
    dplyr::mutate(
      base_data,
      ...,
      !!!if (has_by) list(.by = effective_by) else list(),
      .keep = .keep,
      !!!if (has_before) list(.before = !!before_quo) else list(),
      !!!if (has_after) list(.after = !!after_quo) else list()
    )
  )

  # Strip rowwise_df class after rowwise mutation. dplyr::mutate() on a
  # rowwise_df returns a rowwise_df. If this class leaks into @data, every
  # subsequent mutate() call on the object will behave row-wise.
  if (rowwise_mode) {
    new_data <- dplyr::ungroup(new_data)
  }

  # Step 4: Post-detect labelled outputs and update @metadata.
  updated_metadata <- .extract_metadata_attrs(
    new_data,
    .data@metadata,
    mutated_names
  )

  # Step 5a: Capture surveytidy_recode attrs for transformation log before
  # the strip step removes them. Capture the full attr (not just description)
  # so we can distinguish recode calls (attr set) from non-recode calls
  # (attr NULL), even when .description was not supplied (description = NULL).
  recode_attrs <- lapply(mutated_names, function(col) {
    attr(new_data[[col]], "surveytidy_recode")
  })
  names(recode_attrs) <- mutated_names

  # Step 5b: Strip haven attrs and surveytidy_recode attr from @data.
  new_data <- .strip_metadata_attrs(new_data)

  # Step 6: Re-attach protected columns that .keep dropped
  protected_in_data <- intersect(.protected_cols(.data), names(.data@data))
  missing_protected <- setdiff(protected_in_data, names(new_data))
  if (length(missing_protected) > 0L) {
    new_data <- cbind(new_data, .data@data[, missing_protected, drop = FALSE])
  }

  # Step 7: Update visible_vars — add new columns, remove dropped columns
  new_cols <- setdiff(names(new_data), names(.data@data))
  if (!is.null(.data@variables$visible_vars)) {
    vv <- .data@variables$visible_vars
    vv <- intersect(vv, names(new_data))
    vv <- c(vv, new_cols)
    .data@variables$visible_vars <- if (length(vv) == 0L) NULL else vv
  }

  # Step 8: Record transformations in @metadata@transformations.
  # Recode calls (those that set the surveytidy_recode attr) get a structured
  # list record: fn, source_cols, expr, output_type, description.
  # When surveytidy_recode$fn and surveytidy_recode$var are set, those values
  # are used directly (immune to aliasing; var correct inside across()).
  # Non-recode new columns get plain text (Phase 0.5 behavior).
  for (col in mutated_names) {
    q <- mutations[[col]]
    recode_attr <- recode_attrs[[col]]
    if (!is.null(q) && !is.null(recode_attr)) {
      # Use fn and var from the recode attr when available; fall back to quosure.
      recode_fn <- recode_attr$fn
      recode_var <- recode_attr$var
      fn_name <- if (!is.null(recode_fn)) {
        recode_fn
      } else {
        as.character(rlang::call_name(rlang::quo_get_expr(q)))
      }
      source_cols <- if (!is.null(recode_var)) {
        recode_var
      } else {
        setdiff(all.vars(rlang::quo_squash(q)), col)
      }
      # For row_means() / row_sums(): (a) warn if .cols includes design vars,
      # (b) fall back to the column name as label when .label was not supplied
      # (cur_column() is unavailable in regular mutate() context).
      if (isTRUE(recode_fn %in% c("row_means", "row_sums"))) {
        # (a) design-variable overlap warning
        design_vars <- .survey_design_var_names(.data)
        overlap <- intersect(source_cols, design_vars)
        if (length(overlap) > 0L) {
          cli::cli_warn(
            c(
              "!" = paste0(
                ".cols includes {length(overlap)} design variable ",
                "column{?s}: {.field {overlap}}."
              ),
              "i" = paste0(
                "Row aggregation across design variables produces ",
                "methodologically meaningless results."
              ),
              "i" = paste0(
                "Use a targeted selector such as ",
                "{.code starts_with(\"y\")} to restrict to substantive columns."
              )
            ),
            class = "surveytidy_warning_rowstats_includes_design_var"
          )
        }
        # (b) label fallback: use column name when .label was not supplied
        if (is.null(updated_metadata@variable_labels[[col]])) {
          updated_metadata@variable_labels[[col]] <- col
        }
      }
      updated_metadata@transformations[[col]] <- list(
        fn = fn_name,
        source_cols = source_cols,
        expr = deparse(rlang::quo_squash(q)),
        output_type = if (is.factor(new_data[[col]])) "factor" else "vector",
        description = recode_attr$description
      )
    } else if (!is.null(q) && col %in% new_cols) {
      updated_metadata@transformations[[col]] <- rlang::quo_text(q)
    }
  }

  # Step 9: Assign updated @data and @metadata and return.
  .data@data <- new_data
  .data@metadata <- updated_metadata
  .data
}

#' @rdname mutate
#' @method mutate survey_result
mutate.survey_result <- function(.data, ...) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  result <- NextMethod() |> .restore_survey_result(old_class, old_meta)
  new_meta <- .prune_result_meta(attr(result, ".meta"), names(result))
  attr(result, ".meta") <- new_meta
  result
}

#' @rdname mutate
#' @method mutate survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `mutate()` is dispatched to each
#' member independently. Per-member warnings (e.g.,
#' `surveytidy_warning_mutate_weight_col` when modifying the weight column)
#' fire once per member in which they apply — an N-member collection that
#' all modify the weight column will surface N warnings.
#'
#' If members have non-uniform rowwise state (some are rowwise, some are not),
#' `mutate()` emits `surveytidy_warning_collection_rowwise_mixed` once before
#' dispatch as a soft-invariant diagnostic. Dispatch still proceeds; per-member
#' rowwise/non-rowwise semantics apply for the call. To resolve, call
#' [rowwise()] or [ungroup()] on the entire collection first.
#'
#' `.by` is rejected at the collection layer with
#' `surveytidy_error_collection_by_unsupported`. Set grouping with
#' [group_by()] on the collection instead.
mutate.survey_collection <- function(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL,
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

  rowwise_state <- vapply(.data@surveys, is_rowwise, logical(1L))
  if (any(rowwise_state) && !all(rowwise_state)) {
    rw_names <- names(.data@surveys)[rowwise_state]
    nrw_names <- names(.data@surveys)[!rowwise_state]
    cli::cli_warn(
      c(
        "!" = paste0(
          "{.fn mutate} called on a {.cls survey_collection} with mixed ",
          "rowwise state."
        ),
        "i" = paste0(
          "Rowwise: {.val {rw_names}}; non-rowwise: {.val {nrw_names}}. ",
          "Each member will be mutated under its own semantics, which may ",
          "give inconsistent results."
        ),
        "i" = paste0(
          "Call {.code rowwise(coll)} or {.code ungroup(coll)} on the ",
          "collection first to make rowwise state uniform."
        )
      ),
      class = "surveytidy_warning_collection_rowwise_mixed"
    )
  }

  .keep <- match.arg(.keep)
  .dispatch_verb_over_collection(
    fn = dplyr::mutate,
    verb_name = "mutate",
    collection = .data,
    ...,
    .keep = .keep,
    .before = .before,
    .after = .after,
    .if_missing_var = .if_missing_var,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
}
