# R/survey-collection.R
#
# Feature-group file for survey_collection mutators and internal helpers.
#   - .validate_collection_id()              — shared @id validator
#   - .validate_collection_if_missing_var()  — shared @if_missing_var validator
#   - .check_groups_match()        — equality-only @groups check used by
#                                     the S7 validator and [[<- setter
#   - .propagate_or_match()        — @groups propagation / equality used by
#                                     as_survey_collection() and add_survey()
#   - .repair_collection_names()   — duplicate-name repair algorithm (§3.3.1)
#   - .resolve_caller_names()      — shared name resolution for constructor
#                                     and add_survey() (§3.3)
#   - add_survey()                 — append surveys to a collection (§3.7)
#   - remove_survey()              — drop surveys from a collection (§3.7)
#   - set_collection_id()          — exported setter for @id
#   - set_collection_if_missing_var() — exported setter for @if_missing_var
#   - .dispatch_over_collection()  — dispatch helper for collection-aware get_*()
#   - .warn_on_meta_divergence()   — divergence warning helper

# ── .validate_collection_id() ─────────────────────────────────────────────────

#' Validate a `survey_collection` `@id` value
#'
#' Shared validator used by the S7 class validator, `as_survey_collection()`,
#' `.dispatch_over_collection()` (post-resolution), and `set_collection_id()`.
#' Centralises the "single non-empty non-NA character string" check so all
#' four call sites raise the same error class with consistent CLI prose.
#'
#' @param value    The candidate `@id` value.
#' @param arg_name Character(1). The user-facing argument label to render
#'   in the error message: `"id"` from the S7 validator and the setter,
#'   `".id"` from the constructor and the dispatcher.
#'
#' @return `invisible(value)` on success. Raises
#'   `surveycore_error_collection_invalid_id` on failure.
#'
#' @keywords internal
#' @noRd
.validate_collection_id <- function(value, arg_name) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg {arg_name}} must be a single non-empty, non-NA ",
          "character string."
        ),
        "i" = paste0(
          "Got {.cls {class(value)[[1L]]}} of length ",
          "{.val {length(value)}}: {.val {value}}."
        )
      ),
      class = "surveycore_error_collection_invalid_id"
    )
  }
  invisible(value)
}


# ── .validate_collection_if_missing_var() ─────────────────────────────────────

#' Validate a `survey_collection` `@if_missing_var` value
#'
#' Shared validator used by the S7 class validator,
#' `as_survey_collection()`, `.dispatch_over_collection()` (post-resolution),
#' and `set_collection_if_missing_var()`. Centralises the
#' "length-1 character string in c(\"error\", \"skip\")" check so all four
#' call sites raise the same error class with consistent CLI prose.
#'
#' @param value    The candidate `@if_missing_var` value.
#' @param arg_name Character(1). The user-facing argument label to render
#'   in the error message: `"if_missing_var"` from the S7 validator and
#'   the setter, `".if_missing_var"` from the constructor and the
#'   dispatcher.
#'
#' @return `invisible(value)` on success. Raises
#'   `surveycore_error_collection_invalid_if_missing_var` on failure.
#'
#' @keywords internal
#' @noRd
.validate_collection_if_missing_var <- function(value, arg_name) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !(value %in% c("error", "skip"))
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg {arg_name}} must be one of {.val {\"error\"}} or ",
          "{.val {\"skip\"}}."
        ),
        "i" = paste0(
          "Got {.cls {class(value)[[1L]]}} of length ",
          "{.val {length(value)}}: {.val {value}}."
        )
      ),
      class = "surveycore_error_collection_invalid_if_missing_var"
    )
  }
  invisible(value)
}


# ── .check_groups_match() ─────────────────────────────────────────────────────

#' Assert two @groups vectors are identical (order-sensitive)
#'
#' Equality-only check used by the S7 `survey_collection` validator to enforce
#' invariant G1 (every member's `@groups` is `identical()` to
#' `coll@groups`). Order-sensitive; `character(0) == character(0)` matches.
#'
#' @param candidate_groups  character. `@groups` of the incoming survey.
#' @param target_groups     character. `@groups` of the receiving collection.
#' @param error_class       character(1). Error class thrown on mismatch
#'   (currently always `"surveycore_error_collection_groups_invariant"`).
#' @param context           character(1) or `NULL`. Member name used in the
#'   error message.
#'
#' @return `TRUE` invisibly on match; errors with `error_class` otherwise.
#' @keywords internal
#' @noRd
.check_groups_match <- function(
  candidate_groups,
  target_groups,
  error_class,
  context = NULL
) {
  if (identical(candidate_groups, target_groups)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "x" = paste0(
        "Member {.val {context}} has @groups {.val {candidate_groups}}, ",
        "collection has {.val {target_groups}}."
      )
    ),
    class = error_class
  )
}


# ── .propagate_or_match() ─────────────────────────────────────────────────────

#' Resolve candidate groups against a target: propagate when empty, else match
#'
#' Used by `as_survey_collection()` (adopt-from-members branch) and
#' `add_survey()` (grouped-collection branch). Returns the groups to apply to
#' the member, or errors on a non-empty mismatch. The caller is responsible
#' for assigning the returned value to `member@groups`.
#'
#' @param candidate_groups  character. `@groups` of the incoming survey.
#' @param target_groups     character. `@groups` of the receiving collection.
#' @param name              character(1). Member name for the error message.
#' @param error_class       character(1). Error class thrown on non-empty
#'   mismatch (e.g., `"surveycore_error_collection_group_conflict"`).
#' @return character. Groups to assign to the member (`target_groups` when
#'   propagating; `candidate_groups` when already identical). Errors on
#'   non-empty mismatch.
#' @keywords internal
#' @noRd
.propagate_or_match <- function(
  candidate_groups,
  target_groups,
  name,
  error_class
) {
  if (length(candidate_groups) == 0L) {
    return(target_groups)
  }
  if (identical(candidate_groups, target_groups)) {
    return(candidate_groups)
  }
  cli::cli_abort(
    c(
      "x" = paste0(
        "Cannot add survey {.val {name}}: {.arg @groups} differs from ",
        "collection."
      ),
      "i" = paste0(
        "Collection: {.val {target_groups}}; survey: {.val ",
        "{candidate_groups}}."
      ),
      "v" = paste0(
        "Ungroup the survey via {.fn surveytidy::ungroup} before adding, ",
        "or group the collection to match."
      )
    ),
    class = error_class
  )
}


# ── .repair_collection_names() ────────────────────────────────────────────────

#' Repair duplicate names by suffixing `_1`, `_2`, …
#'
#' Implements the algorithm in spec §3.3.1. The first occurrence of any
#' duplicated name is kept as-is; subsequent occurrences are suffixed with
#' `_1`, `_2`, … Skipping over suffixes already taken by other arguments.
#'
#' @param nms Character vector of caller-supplied names.
#' @return A list with two elements:
#'   - `repaired`: the repaired name vector (same length as `nms`).
#'   - `mapping`: a named character of `old → new` pairs for entries that
#'      changed, or `character(0)` if nothing changed.
#'
#' @keywords internal
#' @noRd
.repair_collection_names <- function(nms) {
  seen <- character(0L)
  out <- character(length(nms))
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (nm %in% seen) {
      j <- 1L
      repeat {
        candidate <- paste0(nm, "_", j)
        if (!(candidate %in% seen)) {
          break
        }
        j <- j + 1L
      }
      out[i] <- candidate
    } else {
      out[i] <- nm
    }
    seen <- c(seen, out[i])
  }

  changed <- out != nms
  if (any(changed)) {
    mapping <- stats::setNames(out[changed], nms[changed])
  } else {
    mapping <- character(0L)
  }

  list(repaired = out, mapping = mapping)
}


# ── .resolve_caller_names() ───────────────────────────────────────────────────

#' Resolve names for `...` arguments captured as quosures
#'
#' For each quosure: use the caller-supplied name if any; else if the
#' argument expression is a bare symbol, use its printed form; else raise
#' `surveycore_error_collection_unnamed_expr` (row C3) naming the 1-based
#' position of the offending argument.
#'
#' Shared between `as_survey_collection()` and `add_survey()` so the
#' error-class choice and position-index formatting stay consistent.
#' Note: the error message text references `as_survey_collection` by name;
#' callers from `add_survey()` see the same text (acceptable — the usage
#' guidance in the `"v"` bullet applies equally to both callers).
#'
#' @param quosures A list of quosures from `rlang::enquos(...)`.
#' @return A character vector of resolved names, same length as `quosures`.
#'
#' @keywords internal
#' @noRd
.resolve_caller_names <- function(quosures) {
  supplied <- names(quosures)
  if (is.null(supplied)) {
    supplied <- rep("", length(quosures))
  }

  out <- character(length(quosures))
  for (i in seq_along(quosures)) {
    nm <- supplied[[i]]
    q <- quosures[[i]]
    if (!is.na(nm) && nzchar(nm)) {
      out[i] <- nm
    } else if (rlang::quo_is_symbol(q)) {
      out[i] <- rlang::as_name(rlang::quo_get_expr(q))
    } else {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Argument {i} passed to {.fn as_survey_collection} is ",
            "unnamed and is not a bare symbol."
          ),
          "i" = paste0(
            "Collection elements must be named, or be supplied as ",
            "bare variable names so they can be auto-named."
          ),
          "v" = paste0(
            "Name the argument explicitly, e.g. ",
            "{.code as_survey_collection(wave1 = as_survey(df, ...))}."
          )
        ),
        class = "surveycore_error_collection_unnamed_expr"
      )
    }
  }
  out
}


# ── as_survey_collection() ────────────────────────────────────────────────────
# Constructor lives in R/core-constructors.R to match other as_survey_*()
# constructors. See that file.

# ── add_survey() ──────────────────────────────────────────────────────────────

#' Add Surveys to a `survey_collection`
#'
#' Appends one or more surveys to an existing collection and returns a new
#' `survey_collection`. The original collection is unchanged. Surveys may be
#' passed with explicit names or as bare symbols (auto-named, like
#' [as_survey_collection()]). Duplicate names are repaired by appending
#' `_1`, `_2`, … Existing names are never modified during repair.
#'
#' @details
#' Calling `add_survey(x)` with no additional surveys returns `x` unchanged;
#' no error is raised.
#'
#' @param .collection A `survey_collection`. Named with a leading dot so it
#'   cannot collide with user-supplied names in `...` (e.g., a survey named
#'   `"x"`).
#' @param ... One or more surveys to append. Accepts named arguments
#'   (`"wave3" = d3`) or bare symbols (`d3`, auto-named to `"d3"`). If a new
#'   name collides with an existing one (or with another new one), it is
#'   repaired by appending `_1`, `_2`, … and a
#'   `surveycore_warning_collection_duplicate_name_repaired` warning is
#'   emitted with the mapping.
#'
#' @return A new `survey_collection` with the appended surveys.
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d2 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' coll <- as_survey_collection(a = d1)
#' coll2 <- add_survey(coll, b = d2)
#' names(coll2)
#' @seealso [as_survey_collection()], [remove_survey()]
#' @family collections
#' @export
add_survey <- function(.collection, ...) {
  if (!S7::S7_inherits(.collection, survey_collection)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg .collection} must be a {.cls survey_collection}, not ",
          "{.cls {class(.collection)[[1L]]}}."
        )
      ),
      class = "surveycore_error_not_survey_collection"
    )
  }

  quosures <- rlang::enquos(...)
  if (length(quosures) == 0L) {
    return(.collection)
  }

  new_caller_names <- .resolve_caller_names(quosures)
  existing_names <- names(.collection@surveys)
  combined_names <- c(existing_names, new_caller_names)

  repair <- .repair_collection_names(combined_names)
  stopifnot(identical(
    repair$repaired[seq_along(existing_names)],
    existing_names
  ))

  if (length(repair$mapping) > 0L) {
    .warn_duplicate_name_repair(repair$mapping)
  }

  new_surveys <- lapply(quosures, rlang::eval_tidy)
  names(new_surveys) <- repair$repaired[
    seq.int(length(existing_names) + 1L, length(combined_names))
  ]

  # Decision 2 — per-new-survey @groups propagation. Build the full
  # validated member list first; the single final survey_collection() call
  # at the end gives atomicity (any error aborts before any mutation).
  coll_groups <- .collection@groups
  if (length(coll_groups) == 0L) {
    # Ungrouped-coll branch: any non-empty new @groups is a conflict (G4).
    for (idx in seq_along(new_surveys)) {
      new <- new_surveys[[idx]]
      if (length(new@groups) > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Cannot add survey {.val {names(new_surveys)[[idx]]}}: ",
              "{.arg @groups} differs from collection."
            ),
            "i" = paste0(
              "Collection: {.val {coll_groups}}; survey: ",
              "{.val {new@groups}}."
            ),
            "v" = paste0(
              "Ungroup the survey via {.fn surveytidy::ungroup} before ",
              "adding, or group the collection to match."
            )
          ),
          class = "surveycore_error_collection_group_conflict"
        )
      }
    }
  } else {
    # Grouped-coll branch: propagate empty or match; error on non-empty
    # mismatch. .propagate_or_match() encapsulates both sub-cases.
    for (idx in seq_along(new_surveys)) {
      new <- new_surveys[[idx]]
      resolved <- .propagate_or_match(
        candidate_groups = new@groups,
        target_groups = coll_groups,
        name = names(new_surveys)[[idx]],
        error_class = "surveycore_error_collection_group_conflict"
      )
      new_surveys[[idx]]@groups <- resolved
    }
  }

  combined_list <- c(.collection@surveys, new_surveys)
  survey_collection(
    surveys = combined_list,
    groups = coll_groups,
    id = .collection@id,
    if_missing_var = .collection@if_missing_var
  )
}


# ── remove_survey() ───────────────────────────────────────────────────────────

#' Remove Surveys from a `survey_collection`
#'
#' Drops one or more named surveys from a collection and returns a new
#' `survey_collection`. Errors if any requested name is not present.
#'
#' @param x A `survey_collection`.
#' @param name Character vector of survey names to drop. All names must be
#'   present in `names(x)`.
#'
#' @return A new `survey_collection` without the dropped surveys. Errors
#'   `surveycore_error_collection_empty` if removing would leave the
#'   collection empty. This error is raised by the S7 class validator, not
#'   by `remove_survey()` itself.
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d2 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' coll <- as_survey_collection(a = d1, b = d2)
#' coll2 <- remove_survey(coll, "a")
#' names(coll2)
#' @seealso [as_survey_collection()], [add_survey()]
#' @family collections
#' @export
remove_survey <- function(x, name) {
  if (!S7::S7_inherits(x, survey_collection)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a {.cls survey_collection}, not ",
          "{.cls {class(x)[[1L]]}}."
        )
      ),
      class = "surveycore_error_not_survey_collection"
    )
  }
  if (!is.character(name)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg name} must be a character vector, not ",
          "{.cls {class(name)[[1L]]}}."
        ),
        "i" = "Got {.cls {class(name)[[1L]]}} instead."
      ),
      class = "surveycore_error_invalid_name_type"
    )
  }

  existing <- names(x@surveys)
  missing_names <- setdiff(name, existing)
  if (length(missing_names) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Name{?s} not found in collection: {.val {missing_names}}."
        ),
        "i" = "Available: {.val {existing}}."
      ),
      class = "surveycore_error_collection_name_not_found"
    )
  }

  keep <- setdiff(existing, name)
  new_list <- x@surveys[keep]
  survey_collection(
    surveys = new_list,
    groups = x@groups,
    id = x@id,
    if_missing_var = x@if_missing_var
  )
}


# ── set_collection_id() ───────────────────────────────────────────────────────

#' Set the Identifier Column on a `survey_collection`
#'
#' Updates the `@id` property of a `survey_collection`. The new value is
#' the column name `.dispatch_over_collection()` injects when an analysis
#' function (`get_means()`, `get_freqs()`, etc.) is dispatched across the
#' collection without an explicit per-call `.id`.
#'
#' @details
#' Setting the same value as the existing `@id` returns the collection
#' unchanged (no error, no warning). All other invariants on the
#' collection (`@surveys`, `@groups`, `@if_missing_var`) are preserved.
#'
#' Pipes naturally with the rest of the collection API:
#' \preformatted{coll |> set_collection_id("wave") |> get_means(y1)}
#'
#' @param x  A [survey_collection].
#' @param id Character(1). The new identifier column name. Must be
#'   non-`NA` and non-empty.
#'
#' @return The modified `survey_collection`, invisibly.
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' coll <- as_survey_collection(a = d1)
#' coll <- set_collection_id(coll, "wave")
#' coll@id
#' @family collections
#' @export
set_collection_id <- function(x, id) {
  if (!S7::S7_inherits(x, survey_collection)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a {.cls survey_collection}, not ",
          "{.cls {class(x)[[1L]]}}."
        )
      ),
      class = "surveycore_error_not_survey_collection"
    )
  }
  .validate_collection_id(id, "id")
  x@id <- id
  invisible(x)
}


# ── set_collection_if_missing_var() ───────────────────────────────────────────

#' Set the Missing-Variable Behaviour on a `survey_collection`
#'
#' Updates the `@if_missing_var` property of a `survey_collection`. The
#' new value is the per-call default `.dispatch_over_collection()` uses
#' when an analysis function (`get_means()`, `get_freqs()`, etc.) is
#' dispatched across the collection without an explicit per-call
#' `.if_missing_var`.
#'
#' @details
#' Setting the same value as the existing `@if_missing_var` returns the
#' collection unchanged (no error, no warning). All other invariants on
#' the collection (`@surveys`, `@groups`, `@id`) are preserved.
#'
#' Pipes naturally with the rest of the collection API:
#' \preformatted{coll |> set_collection_if_missing_var("skip") |> get_means(y1)}
#'
#' @param x A [survey_collection].
#' @param if_missing_var Character(1), one of `c("error", "skip")`.
#'   When `"skip"`, member surveys missing a requested variable are
#'   dropped from the dispatched result; when `"error"`, the dispatcher
#'   aborts.
#'
#' @return The modified `survey_collection`, invisibly.
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' coll <- as_survey_collection(a = d1)
#' coll <- set_collection_if_missing_var(coll, "skip")
#' coll@if_missing_var
#' @family collections
#' @export
set_collection_if_missing_var <- function(x, if_missing_var) {
  if (!S7::S7_inherits(x, survey_collection)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a {.cls survey_collection}, not ",
          "{.cls {class(x)[[1L]]}}."
        )
      ),
      class = "surveycore_error_not_survey_collection"
    )
  }
  .validate_collection_if_missing_var(if_missing_var, "if_missing_var")
  x@if_missing_var <- if_missing_var
  invisible(x)
}


# ── Internal warning helper ───────────────────────────────────────────────────

#' Emit the duplicate-name repair warning
#'
#' Shared between `as_survey_collection()` and `add_survey()` so the warning
#' wording and class stay consistent.
#'
#' @param mapping Named character: values = new names, names = originals.
#' @keywords internal
#' @noRd
.warn_duplicate_name_repair <- function(mapping) {
  pairs <- paste0(names(mapping), " -> ", unname(mapping))
  n <- length(pairs)
  cli::cli_warn(
    c(
      "!" = "Duplicate survey {cli::qty(n)}name{?s} in collection repaired.",
      "i" = "{cli::qty(n)}Rename{?s}: {.code {pairs}}."
    ),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
}


# ── .dispatch_over_collection() ───────────────────────────────────────────────

#' Dispatch a `get_*()` analysis function across every survey in a collection
#'
#' Runs `fn(survey, ...)` for each survey in `collection@surveys`, handles
#' missing variables per the resolved `.if_missing_var`, row-binds the
#' per-survey results, prepends the resolved `.id` identifier column, and
#' carries over per-survey `.meta` under `$per_survey`. See spec §4.1.
#'
#' @section Collection grouping:
#' Per-survey `@groups` is guaranteed uniform (`identical()` to
#' `collection@groups`) by the class invariant enforced in
#' `R/core-classes.R` (validator rules G1 / G1b / G1c). That means
#' `.resolve_groups()` inside each `get_*()` call (see
#' `R/analysis-helpers.R:441` — `unique(c(from_groups_prop, from_arg))`)
#' automatically sees the collection-level grouping without any dispatch-
#' layer branching. Any call-site `group =` quosure stacks with the
#' collection's groups (Decision 5); that behavior is a consequence of
#' the invariant + existing dispatch, not a separate code path.
#'
#' @section Precedence resolution:
#' Both `.id` and `.if_missing_var` follow the same two-tier rule:
#' a non-`NULL` value at the analysis-function call site beats the
#' value stored on the collection's `@id` / `@if_missing_var` property.
#' Because the S7 class validator guarantees both stored properties are
#' always valid, no third fallback tier is required.
#'
#' \describe{
#'   \item{`.id = NULL` (default)}{resolves to `collection@id`.}
#'   \item{`.id = "wave"`}{overrides the stored value; column name is
#'     `"wave"` regardless of `collection@id`.}
#'   \item{`.if_missing_var = NULL` (default)}{resolves to
#'     `collection@if_missing_var`.}
#'   \item{`.if_missing_var = "skip"`}{overrides the stored value
#'     for this call only.}
#' }
#'
#' @param fn          The `get_*()` function being dispatched.
#' @param collection  A `survey_collection`.
#' @param ...         Forwarded to `fn()`. NSE args must be forwarded with
#'   `{{ }}` at the dispatch-branch call site.
#' @param .id Character(1) or `NULL`. When non-`NULL`, the column name for
#'   the survey identifier; when `NULL`, the dispatcher resolves to
#'   `collection@id`.
#' @param .if_missing_var Character(1) in `c("error", "skip")` or `NULL`.
#'   When non-`NULL`, the per-call missing-variable behaviour; when
#'   `NULL`, the dispatcher resolves to `collection@if_missing_var`.
#' @return A tibble (same S3 class as the per-survey results) with the
#'   resolved `.id` column prepended and `.meta` carried over.
#'
#' @keywords internal
#' @noRd
.dispatch_over_collection <- function(
  fn,
  collection,
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  # Two-tier precedence: call-site non-NULL beats stored property.
  resolved_id <- .id %||% collection@id
  resolved_if_missing_var <- .if_missing_var %||% collection@if_missing_var

  # Single post-resolution validation gate. The S7 class validator
  # guarantees stored properties are valid, so this only catches
  # invalid call-site values; it would also catch a corrupted
  # collection (defensive but cheap).
  .validate_collection_id(resolved_id, ".id")
  .validate_collection_if_missing_var(
    resolved_if_missing_var,
    ".if_missing_var"
  )

  # Track whether the resolved id came from the call-site or the stored
  # property; the id_collision hint surfaces `set_collection_id()` only
  # when the user reached the collision via the stored property.
  id_from_stored <- is.null(.id)

  nms <- names(collection@surveys)

  results <- list()
  skipped <- character(0L)
  for (nm in nms) {
    r <- tryCatch(
      fn(collection@surveys[[nm]], ...),
      surveycore_error_variable_not_found = function(cnd) {
        if (resolved_if_missing_var == "skip") {
          skipped <<- c(skipped, nm)
          NULL
        } else {
          cli::cli_abort(
            c(
              "x" = paste0(
                "Survey {.val {nm}} in the collection is missing a ",
                "required variable."
              ),
              "i" = "Original error: {conditionMessage(cnd)}",
              "v" = paste0(
                "Set {.code .if_missing_var = \"skip\"} to drop ",
                "surveys missing the variable."
              )
            ),
            class = "surveycore_error_collection_missing_var",
            parent = cnd
          )
        }
      }
    )
    if (!is.null(r)) {
      results[[nm]] <- r
    }
  }

  if (length(skipped) > 0L) {
    cli::cli_inform(
      c(
        "i" = paste0(
          "Skipped {length(skipped)} survey{?s} missing the requested ",
          "variable: {.val {skipped}}."
        )
      ),
      class = "surveycore_message_collection_skipped_surveys"
    )
  }

  if (length(results) == 0L) {
    cli::cli_abort(
      c(
        "x" = "No surveys in the collection contained the requested variable."
      ),
      class = "surveycore_error_collection_all_skipped"
    )
  }

  first_cols <- names(results[[1L]])
  if (resolved_id %in% first_cols) {
    id_name <- resolved_id
    if (id_from_stored) {
      v_bullet <- paste0(
        "Pass a different {.arg .id} to override (e.g., ",
        "{.code .id = \"wave\"}) or update the stored property via ",
        "{.fn set_collection_id}."
      )
    } else {
      v_bullet <- paste0(
        "Pass a different {.arg .id}, e.g. {.code .id = \"wave\"}."
      )
    }
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg .id} value {.val {id_name}} conflicts with a column ",
          "produced by the analysis function."
        ),
        "v" = v_bullet
      ),
      class = "surveycore_error_collection_id_collision"
    )
  }

  for (nm in names(results)) {
    results[[nm]][[resolved_id]] <- nm
  }

  out <- tryCatch(
    dplyr::bind_rows(results),
    vctrs_error_incompatible_type = function(cnd) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Cannot combine per-survey results: column type mismatch. ",
            "Each survey must produce compatible types for shared columns."
          ),
          "i" = "Original error: {conditionMessage(cnd)}"
        ),
        class = "surveycore_error_collection_bind_type_mismatch",
        parent = cnd
      )
    },
    vctrs_error = function(cnd) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Cannot combine per-survey results: column type mismatch. ",
            "Each survey must produce compatible types for shared columns."
          ),
          "i" = "Original error: {conditionMessage(cnd)}"
        ),
        class = "surveycore_error_collection_bind_type_mismatch",
        parent = cnd
      )
    }
  )

  out <- out[, c(resolved_id, setdiff(names(out), resolved_id))]

  first_meta <- attr(results[[1L]], ".meta")
  per_survey_meta <- lapply(results, function(r) attr(r, ".meta"))
  collection_meta <- list(
    surveys = names(results),
    survey_classes = vapply(
      names(results),
      function(nm) {
        sub("^[^:]+::", "", class(collection@surveys[[nm]])[[1L]])
      },
      character(1L)
    )
  )
  new_meta <- c(
    first_meta,
    list(
      per_survey = per_survey_meta,
      collection = collection_meta
    )
  )
  attr(out, ".meta") <- new_meta

  .warn_on_meta_divergence(per_survey_meta)

  class(out) <- class(results[[1L]])
  out
}


# ── .warn_on_meta_divergence() ────────────────────────────────────────────────

#' Warn when per-survey metadata disagrees on a closed set of fields
#'
#' @details
#' The closed set of compared `(slot, var, field)` triples is:
#' `slot ∈ {"group", "x"}`, `var` is every variable-name key appearing under
#' either slot across any survey, and `field ∈ {"value_labels",
#' "variable_label", "question_preface"}`. Other keys (e.g.,
#' `variable_note`, user-added fields) are not compared. If a future change
#' adds a correctness-critical field, update this helper explicitly —
#' divergence scope is closed by design.
#'
#' Comparison uses `identical()`. Absence-vs-presence counts as divergence:
#' `identical(value, NULL)` is `FALSE`.
#'
#' @param per_survey_meta A named list of per-survey `.meta` lists.
#' @return `invisible(NULL)`. Emits a
#'   `surveycore_warning_collection_meta_divergence` warning when any
#'   divergent variable is found.
#'
#' @keywords internal
#' @noRd
.warn_on_meta_divergence <- function(per_survey_meta) {
  if (length(per_survey_meta) <= 1L) {
    return(invisible(NULL))
  }

  slots <- c("group", "x")
  fields <- c("value_labels", "variable_label", "question_preface")

  divergent <- character(0L)
  for (slot in slots) {
    vars <- unique(unlist(
      lapply(per_survey_meta, function(m) names(m[[slot]])),
      use.names = FALSE
    ))
    for (v in vars) {
      if (v %in% divergent) {
        next
      }
      for (field in fields) {
        vals <- lapply(
          per_survey_meta,
          function(m) m[[slot]][[v]][[field]]
        )
        first <- vals[[1L]]
        diverged <- FALSE
        for (i in seq.int(2L, length(vals))) {
          if (!identical(first, vals[[i]])) {
            diverged <- TRUE
            break
          }
        }
        if (diverged) {
          divergent <- c(divergent, v)
          break
        }
      }
    }
  }

  divergent <- sort(unique(divergent))

  if (length(divergent) == 0L) {
    return(invisible(NULL))
  }

  cli::cli_warn(
    c(
      "!" = paste0(
        "Per-survey metadata diverges for {length(divergent)} ",
        "variable{?s}: {.field {divergent}}."
      ),
      "i" = paste0(
        "The top-level {.code .meta} reflects only the first survey. ",
        "Per-survey metadata is preserved under ",
        "{.code attr(result, \".meta\")$per_survey}."
      ),
      "i" = paste0(
        "Downstream helpers (e.g., {.fn clean}, {.fn gt}) should consult ",
        "{.code $per_survey} for accurate per-row labeling."
      )
    ),
    class = "surveycore_warning_collection_meta_divergence"
  )
  invisible(NULL)
}
