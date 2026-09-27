# R/rename.R
#
# rename() and rename_with() for survey design objects.
#
# Both verbs share the atomic-update helper .apply_rename_map(), which:
#   - Warns on design variable renames (surveytidy_warning_rename_design_var)
#   - Silently blocks renaming the domain column (SURVEYCORE_DOMAIN_COL)
#   - Atomically updates @data, @variables, @metadata, visible_vars, @groups
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   rename.survey_base()        — column renaming with design-var preservation
#   rename.survey_result()      — column renaming with .meta key updates
#   rename_with.survey_base()   — function-applied renaming
#   rename_with.survey_result() — function-applied renaming for survey_result

# ── .apply_rename_map() ───────────────────────────────────────────────────────

# Shared atomic-update helper used by rename() and rename_with().
#
# Arguments:
#   .data      — survey_base object
#   rename_map — named character vector; names = old col names, values = new
#
# Returns the modified .data object. S7 validation is called once at the end.
#
# @keywords internal
# @noRd
.apply_rename_map <- function(.data, rename_map) {
  old_names <- names(rename_map)
  new_names <- unname(rename_map)

  # Step 1: silently block renaming the domain column.
  # SURVEYCORE_DOMAIN_COL has a fixed identity used by filter() and estimation.
  # Renaming it would silently break those verbs. Other design variables (strata,
  # PSU, weights) are warned about but still renamed — users may legitimately
  # rename their own columns.
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  is_domain <- old_names == domain_col
  if (any(is_domain)) {
    rename_map <- rename_map[!is_domain]
    old_names <- names(rename_map)
    new_names <- unname(rename_map)
  }

  # If nothing remains after removing the domain column, warn and return.
  if (length(rename_map) == 0L) {
    if (any(is_domain)) {
      # Only the domain col was in the map — issue the design var warning
      cli::cli_warn(
        c(
          "!" = "Renamed design variable{?s} {.field {domain_col}}.",
          "i" = "{cli::qty(1L)}The survey design has been updated to track the new name{?s}."
        ),
        class = "surveytidy_warning_rename_design_var"
      )
    }
    return(.data)
  }

  # Step 2: warn if any design variable is being renamed (including domain col
  # that was stripped — combine for a single warning).
  protected <- intersect(.protected_cols(.data), names(.data@data))
  is_design_var <- old_names %in% protected
  all_design_cols <- c(
    if (any(is_domain)) domain_col else character(0),
    old_names[is_design_var]
  )
  if (length(all_design_cols) > 0L) {
    n_design_cols <- length(all_design_cols)
    cli::cli_warn(
      c(
        "!" = "Renamed design variable{?s} {.field {all_design_cols}}.",
        "i" = "{cli::qty(n_design_cols)}The survey design has been updated to track the new name{?s}."
      ),
      class = "surveytidy_warning_rename_design_var"
    )
  }

  # Steps 3–6: atomically update @data, @variables, @metadata, visible_vars,
  # and @groups. Use attr() to bypass per-assignment S7 validation, then call
  # S7::validate() once on the fully consistent final state.

  # Step 3: rename columns in @data
  new_data <- .data@data
  name_positions <- match(old_names, names(new_data))
  names(new_data)[name_positions] <- new_names
  attr(.data, "data") <- new_data

  # Step 4: update @variables (design spec + visible_vars)
  new_variables <- .sc_update_design_var_names(.data@variables, rename_map)

  # .sc_update_design_var_names() does not handle twophase nested phase1/phase2
  # variables or the $subset column — update those here.
  .update_phase_vars <- function(phase) {
    if (is.null(phase)) {
      return(phase)
    }
    for (slot in c("ids", "weights", "strata", "fpc", "probs")) {
      if (!is.null(phase[[slot]])) {
        matched <- phase[[slot]] %in% names(rename_map)
        phase[[slot]][matched] <- rename_map[phase[[slot]][matched]]
      }
    }
    phase
  }
  new_variables$phase1 <- .update_phase_vars(new_variables$phase1)
  new_variables$phase2 <- .update_phase_vars(new_variables$phase2)

  if (
    !is.null(new_variables$subset) &&
      new_variables$subset %in% names(rename_map)
  ) {
    new_variables$subset <- rename_map[[new_variables$subset]]
  }

  # Update visible_vars within new_variables — replace old names with new names
  vv <- new_variables$visible_vars
  if (!is.null(vv)) {
    for (i in seq_along(old_names)) {
      vv[vv == old_names[[i]]] <- new_names[[i]]
    }
    new_variables$visible_vars <- vv
  }
  attr(.data, "variables") <- new_variables

  # Step 5: update @metadata keys across all slots
  new_metadata <- .sc_rename_metadata_keys(.data@metadata, rename_map)
  attr(.data, "metadata") <- new_metadata

  # Step 6 (NEW): update @groups — replace old column names with new names
  old_groups <- .data@groups
  if (length(old_groups) > 0L) {
    new_groups <- old_groups
    for (i in seq_along(old_names)) {
      new_groups[new_groups == old_names[[i]]] <- new_names[[i]]
    }
    attr(.data, "groups") <- new_groups
  }

  # Final validation on the fully consistent state
  S7::validate(.data)

  .data
}


# ── rename() ──────────────────────────────────────────────────────────────────

#' Rename columns of a survey design object
#'
#' @description
#' `rename()` and `rename_with()` change column names in the underlying data
#' and automatically keep the survey design in sync. Variable labels, value
#' labels, and other metadata follow the rename — no manual bookkeeping
#' required.
#'
#' Use `rename()` for `new_name = old_name` pairs; use `rename_with()` to
#' apply a function across a selection of column names.
#'
#' Renaming a design variable (weights, strata, PSUs) is fully supported:
#' the design specification updates automatically and a
#' `surveytidy_warning_rename_design_var` warning is issued to confirm the
#' change.
#'
#' @details
#' ## What gets updated
#' * **Column names in `@data`** — the rename takes effect immediately.
#' * **Design specification** — if a renamed column is a design variable
#'   (weights, strata, PSU, FPC, or replicate weights), `@variables` is
#'   updated to track the new name.
#' * **Metadata** — variable labels, value labels, question prefaces, notes,
#'   and transformation records in `@metadata` are re-keyed to the new name.
#' * **`visible_vars`** — any occurrence of the old name in
#'   `@variables$visible_vars` is replaced with the new name, so
#'   [select()] + `rename()` pipelines work correctly.
#' * **Groups** — if a renamed column is in the active grouping, `@groups`
#'   is updated to use the new name.
#'
#' ## Renaming design variables
#' Renaming a design variable (e.g., the weights column) is intentionally
#' allowed. A `surveytidy_warning_rename_design_var` warning is issued as a
#' reminder that the design specification has been updated — not to indicate
#' an error.
#'
#' ## rename_with() function forms
#' `.fn` can be any of:
#' * A bare function: `rename_with(d, toupper)`
#' * A formula: `rename_with(d, ~ toupper(.))`
#' * A lambda: `rename_with(d, \(x) paste0(x, "_v2"))`
#'
#' Extra arguments to `.fn` can be passed via `...`:
#' ```r
#' rename_with(d, stringr::str_replace, .cols = tidyselect::starts_with("y"),
#'             pattern = "y", replacement = "outcome")
#' ```
#'
#' `.cols` uses tidy-select syntax. The default `dplyr::everything()` applies
#' `.fn` to all columns including design variables — which will trigger a
#' `surveytidy_warning_rename_design_var` warning for each renamed design
#' variable.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`tidy-select`][tidyselect::language]> Use `new_name = old_name`
#'   pairs to rename columns. Any number of columns can be renamed in a
#'   single call.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * Rows are not added or removed.
#' * Column order is preserved.
#' * Renamed columns are updated in `@data`, `@variables`, `@metadata`, and
#'   `@groups`.
#' * Survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # rename() ----------------------------------------------------------------
#'
#' # rename an outcome column
#' rename(d, financial_situation = fin_sit)
#'
#' # rename multiple columns at once
#' rename(d, region = cregion, education = educcat)
#'
#' # rename a design variable — warns and updates the design specification
#' rename(d, survey_weight = weight)
#'
#' # rename_with() -----------------------------------------------------------
#'
#' # apply a function to all matching columns
#' rename_with(d, toupper, .cols = tidyselect::starts_with("econ"))
#'
#' # use a formula
#' rename_with(d, ~ paste0(., "_v2"), .cols = tidyselect::starts_with("econ"))
#'
#' @family modification
#' @seealso [mutate()] to add or modify column values, [select()] to drop
#'   columns
#' @name rename
NULL

#' @rdname rename
#' @method rename survey_base
rename.survey_base <- function(.data, ...) {
  # map: named integer vector; names = new names, values = column positions
  map <- tidyselect::eval_rename(rlang::expr(c(...)), .data@data)
  new_names <- names(map)
  old_names <- names(.data@data)[map]

  # rename_map: names = old column names, values = new column names
  rename_map <- stats::setNames(new_names, old_names)

  .apply_rename_map(.data, rename_map)
}

#' @rdname rename
#' @method rename survey_result
rename.survey_result <- function(.data, ...) {
  tbl <- tibble::as_tibble(.data)

  # Build rename map: eval_rename returns named integer (new_name → position)
  # Convert to c(old_name = "new_name") format for .apply_result_rename_map
  map <- tidyselect::eval_rename(rlang::expr(c(...)), tbl)
  rename_map <- stats::setNames(names(map), names(tbl)[map])

  .apply_result_rename_map(.data, rename_map)
}


# ── rename_with() ─────────────────────────────────────────────────────────────

#' @rdname rename
#' @method rename_with survey_base
#' @param .fn A function (or formula/lambda) applied to selected column names.
#'   Must return a character vector of the same length as its input, with no
#'   duplicates and no conflicts with existing non-renamed column names.
#' @param .cols <[`tidy-select`][tidyselect::language]> Columns whose names
#'   `.fn` will transform. Defaults to all columns.
rename_with.survey_base <- function(
  .data,
  .fn,
  .cols = dplyr::everything(),
  ...
) {
  # Step 1: resolve .cols to column names (NOT eval_rename — .cols is a
  # selection expression, not a new = old rename spec).
  # enquo() captures the caller's expression + environment so that tidyselect
  # helpers like tidyselect::starts_with() are found correctly.
  selected <- tidyselect::eval_select(rlang::enquo(.cols), .data@data)
  old_names <- names(selected)

  # Step 2: apply .fn to produce new column names
  .fn <- rlang::as_function(.fn)
  new_names <- rlang::exec(.fn, old_names, !!!rlang::list2(...))

  # Step 3: validate .fn output
  non_renamed <- setdiff(names(.data@data), old_names)

  if (!is.character(new_names)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .fn} must return a character vector.",
        "i" = "Got {.cls {class(new_names)[[1L]]}} of length {length(new_names)}.",
        "v" = "Check that {.arg .fn} returns a plain character vector and handles all column names uniformly."
      ),
      class = "surveytidy_error_rename_fn_bad_output"
    )
  }

  if (length(new_names) != length(old_names)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .fn} must return a character vector of the same length as its input.",
        "i" = "Input had {length(old_names)} name{?s}; {.arg .fn} returned {length(new_names)}.",
        "v" = "Check that {.arg .fn} returns a plain character vector and handles all column names uniformly."
      ),
      class = "surveytidy_error_rename_fn_bad_output"
    )
  }

  if (anyDuplicated(new_names) > 0L) {
    dupes <- new_names[duplicated(new_names)]
    cli::cli_abort(
      c(
        "x" = "{.arg .fn} must return a character vector with no duplicate names.",
        "i" = "Duplicate name{?s}: {.field {unique(dupes)}}.",
        "v" = "Check that {.arg .fn} returns a plain character vector and handles all column names uniformly."
      ),
      class = "surveytidy_error_rename_fn_bad_output"
    )
  }

  conflicts <- intersect(new_names, non_renamed)
  if (length(conflicts) > 0L) {
    n_conflicts <- length(conflicts)
    cli::cli_abort(
      c(
        "x" = "{cli::qty(n_conflicts)}{.arg .fn} returned name{?s} that conflict with existing columns.",
        "i" = "Conflicting name{?s}: {.field {conflicts}}.",
        "v" = "Check that {.arg .fn} returns a plain character vector and handles all column names uniformly."
      ),
      class = "surveytidy_error_rename_fn_bad_output"
    )
  }

  # Step 4: build rename_map and delegate to .apply_rename_map()
  rename_map <- stats::setNames(new_names, old_names)
  .apply_rename_map(.data, rename_map)
}

#' @rdname rename
#' @method rename_with survey_result
rename_with.survey_result <- function(
  .data,
  .fn,
  .cols = dplyr::everything(),
  ...
) {
  tbl <- tibble::as_tibble(.data)

  # Step 1: Resolve .cols
  resolved_cols <- tidyselect::eval_select(rlang::enquo(.cols), tbl)
  old_names <- names(resolved_cols)

  # Zero-match .cols — no-op
  if (length(old_names) == 0L) {
    return(.data)
  }

  # Step 2: Apply .fn
  new_names <- .fn(old_names, ...)

  # Step 3: Validate all four bad-output conditions
  # Build the full column list with renames applied (for duplicate check)
  full_new_names <- names(tbl)
  full_new_names[match(old_names, full_new_names)] <- new_names

  if (
    !is.character(new_names) ||
      length(new_names) != length(old_names) ||
      anyNA(new_names) ||
      anyDuplicated(full_new_names) > 0L
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg .fn} must return a character vector the same length as
               its input with no {.code NA} or duplicate names.",
        "i" = "Got class {.cls {class(new_names)}} of length {length(new_names)}."
      ),
      class = "surveytidy_error_rename_fn_bad_output"
    )
  }

  # Step 4: Build rename map and delegate
  rename_map <- stats::setNames(new_names, old_names)
  .apply_result_rename_map(.data, rename_map)
}


# ── rename.survey_collection / rename_with.survey_collection (PR 2b) ──────────

# Pre-flight helper for rename and rename_with on survey_collection.
#
# Spec §IV.4 (D3 resolution): if a renamed column is in coll@groups, every
# member must rename that column in lockstep — otherwise the resulting
# collection has half-renamed @groups (G1 violation that no .if_missing_var
# policy can recover).
#
# Inputs:
#   coll                       — the survey_collection
#   verb_name                  — "rename" or "rename_with" (for diagnostics)
#   rename_olds_per_member     — named list; one entry per member, value is
#                                a character vector of `old_name`s the
#                                member's rename map will rename
#
# For plain rename: rename_olds_per_member is computed by per-member
# tidyselect::eval_rename(); the entries are typically identical, but a
# member missing the column under a regression scenario will have an empty
# entry, which surfaces as the partial signal here.
#
# For rename_with: rename_olds_per_member is per-member tidyselect::eval_select
# of `.cols`; entries differ when `.cols` resolves differently across members
# (e.g., where(is.factor) on a heterogeneous schema), which is the genuine
# trigger for this class.
#
# @keywords internal
# @noRd
.check_group_rename_coverage <- function(
  coll,
  verb_name,
  rename_olds_per_member
) {
  if (length(coll@groups) == 0L) {
    return(invisible(NULL))
  }
  member_names <- names(rename_olds_per_member)
  for (g in coll@groups) {
    members_renaming_g <- character(0L)
    for (nm in member_names) {
      if (g %in% rename_olds_per_member[[nm]]) {
        members_renaming_g <- c(members_renaming_g, nm)
      }
    }
    n_total <- length(member_names)
    n_renaming <- length(members_renaming_g)
    if (n_renaming == 0L) {
      next
    }
    if (n_renaming < n_total) {
      missing_members <- setdiff(member_names, members_renaming_g)
      n_missing <- length(missing_members)
      cli::cli_abort(
        c(
          "x" = "{.fn {verb_name}} would partially rename group column {.field {g}} on the {.cls survey_collection}.",
          "i" = "{cli::qty(n_missing)}Member{?s} {.val {missing_members}} would not rename {.field {g}}, leaving {.code @groups} inconsistent across the collection.",
          "v" = "Either include {.field {g}} in the rename for every member, or call {.fn ungroup} on the collection first."
        ),
        class = "surveytidy_error_collection_rename_group_partial"
      )
    }
  }
  invisible(NULL)
}

#' @rdname rename
#' @method rename survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `rename()` is dispatched to each
#' member independently. Each member's `rename.survey_base` updates `@data`,
#' `@variables`, `@metadata`, and `@groups` atomically.
#'
#' Before dispatching, `rename.survey_collection` resolves the rename map
#' against each member's `@data` and raises
#' `surveytidy_error_collection_rename_group_partial` if any column in
#' `coll@groups` would be renamed on some members but not others — that
#' would leave the collection with an inconsistent `@groups` invariant
#' (G1) that no `.if_missing_var` policy can recover. For plain `rename`
#' the rename map is universal, so this branch normally fires only as a
#' defense-in-depth catch for regressions in the surveycore G1b validator.
#'
#' Renaming a non-group design variable (weights, ids, strata, fpc) emits
#' `surveytidy_warning_rename_design_var` once per member — N firings on
#' an N-member collection. Capture with `withCallingHandlers()`.
rename.survey_collection <- function(.data, ..., .if_missing_var = NULL) {
  dots <- rlang::enquos(...)

  if (length(.data@groups) > 0L && length(dots) > 0L) {
    rename_olds_per_member <- stats::setNames(
      vector("list", length(.data@surveys)),
      names(.data@surveys)
    )
    for (nm in names(.data@surveys)) {
      member <- .data@surveys[[nm]]
      olds <- tryCatch(
        {
          map <- tidyselect::eval_rename(
            rlang::expr(c(!!!dots)),
            member@data
          )
          names(member@data)[map]
        },
        error = function(e) character(0L)
      )
      rename_olds_per_member[[nm]] <- olds
    }
    .check_group_rename_coverage(.data, "rename", rename_olds_per_member)
  }

  .dispatch_verb_over_collection(
    fn = dplyr::rename,
    verb_name = "rename",
    collection = .data,
    ...,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = TRUE
  )
}

#' @rdname rename
#' @method rename_with survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `rename_with()` is dispatched to
#' each member independently. Each member resolves `.cols` against its own
#' `@data`, so a `.cols` like `where(is.factor)` may select different
#' columns on different members.
#'
#' Before dispatching, `rename_with.survey_collection` resolves `.cols`
#' per-member and raises
#' `surveytidy_error_collection_rename_group_partial` if any column in
#' `coll@groups` would be renamed on some members but not others. This
#' is the genuine trigger for the partial-rename class — `.cols`
#' resolving differently across a heterogeneous collection is the path
#' the spec is designed to catch (see §IV.4 reachability note).
#'
#' Per-member design-variable warnings fire once per affected member.
rename_with.survey_collection <- function(
  .data,
  .fn,
  .cols = dplyr::everything(),
  ...,
  .if_missing_var = NULL
) {
  # Capture .cols as a quosure: per-member dispatch must re-evaluate the
  # tidyselect expression against each member's @data. The dispatcher's
  # .unwrap_scalar_dots would eval_tidy a `.cols`-named arg in the wrong
  # context, so we route .cols around the dotted-arg path via a closure.
  cols_quo <- rlang::enquo(.cols)

  if (length(.data@groups) > 0L) {
    rename_olds_per_member <- stats::setNames(
      vector("list", length(.data@surveys)),
      names(.data@surveys)
    )
    for (nm in names(.data@surveys)) {
      member <- .data@surveys[[nm]]
      olds <- tryCatch(
        {
          sel <- tidyselect::eval_select(cols_quo, member@data)
          names(sel)
        },
        error = function(e) character(0L)
      )
      rename_olds_per_member[[nm]] <- olds
    }
    .check_group_rename_coverage(
      .data,
      "rename_with",
      rename_olds_per_member
    )
  }

  fn <- function(survey, ...) {
    rlang::inject(dplyr::rename_with(
      survey,
      .fn,
      .cols = !!cols_quo,
      ...
    ))
  }

  .dispatch_verb_over_collection(
    fn = fn,
    verb_name = "rename_with",
    collection = .data,
    ...,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = TRUE
  )
}
