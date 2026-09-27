# R/collection-pull-glimpse.R
#
# Collapsing collection verbs: pull.survey_collection and
# glimpse.survey_collection. These verbs do NOT route through
# `.dispatch_verb_over_collection` — the dispatcher rebuilds a
# `survey_collection`, but `pull` returns a vector and `glimpse` prints to the
# console. Both iterate per-member directly.
#
# Spec §V.1 (pull) and §V.2 (glimpse).

# ── pull.survey_collection ────────────────────────────────────────────────────

# Local class-catch wrapper for `pull.survey_collection`. Mirrors
# `.apply_class_catch()` in collection-dispatch.R: a missing column referenced
# by `var` or `name` raises `vctrs_error_subscript_oob` /
# `rlang_error_data_pronoun_not_found` from `dplyr::pull`, which under
# `.if_missing_var = "skip"` produces NULL (skipping the member) and under
# `"error"` re-raises through `surveytidy_error_collection_verb_failed` with
# `parent = cnd`.
#
# Duplicated from the dispatcher per `engineering-preferences.md` §3 — the
# alternative (generalising the dispatcher to also support collapsing return
# types) is over-engineering until a third collapsing verb appears.
.pull_apply_class_catch <- function(
  member,
  var_quo,
  name_quo,
  member_name,
  resolved_if_missing_var
) {
  tryCatch(
    rlang::inject(dplyr::pull(member, !!var_quo, name = !!name_quo)),
    vctrs_error_subscript_oob = function(cnd) {
      .handle_class_catch(cnd, member_name, "pull", resolved_if_missing_var)
    },
    rlang_error_data_pronoun_not_found = function(cnd) {
      .handle_class_catch(cnd, member_name, "pull", resolved_if_missing_var)
    },
    rlang_error = function(cnd) {
      parent_cnd <- cnd$parent
      if (
        !is.null(parent_cnd) &&
          (inherits(parent_cnd, "vctrs_error_subscript_oob") ||
            inherits(parent_cnd, "rlang_error_data_pronoun_not_found"))
      ) {
        return(.handle_class_catch(
          parent_cnd,
          member_name,
          "pull",
          resolved_if_missing_var
        ))
      }
      stop(cnd)
    }
  )
}

#' @rdname pull
#' @method pull survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `pull()` extracts the column from
#' each member and combines the per-member vectors via [vctrs::vec_c()].
#' Detection of missing columns uses class-catch only — both `var` and `name`
#' flow through a single `tryCatch` handler that re-raises
#' `vctrs_error_subscript_oob` / `rlang_error_data_pronoun_not_found` as
#' `surveytidy_error_collection_verb_failed`.
#'
#' Naming options:
#' \itemize{
#'   \item `name = NULL` (default) — unnamed combined vector.
#'   \item `name = coll@id` — by-survey naming sentinel: each combined element
#'     is named by its source survey. The sentinel string is whatever
#'     `coll@id` resolves to (default `.survey`; user-set values like `"wave"`
#'     work identically).
#'   \item `name = "<other_column>"` — passes through to `dplyr::pull`'s
#'     `name` arg unchanged (per-row names from another column inside each
#'     member), then combined across surveys via the same [vctrs::vec_c()]
#'     path as the values.
#' }
#'
#' If [vctrs::vec_c()] raises `vctrs_error_incompatible_type` (e.g., one
#' member has the column as numeric and another as character), the error is
#' re-raised as `surveytidy_error_collection_pull_incompatible_types` with
#' `parent = cnd` and the column name and conflicting surveys. No
#' auto-coercion — `pull` returns a single vector and silent coercion would
#' mask the kind of data-type bug users almost certainly want surfaced.
#' (`glimpse.survey_collection` auto-coerces with a footer; the divergence is
#' intentional — `glimpse` is diagnostic, `pull` is computational.)
#'
#' @section Domain inclusion:
#' Inherits the contract of [pull()] for `survey_base`: the returned vector
#' includes both in-domain and out-of-domain values. `pull.survey_base` calls
#' `dplyr::pull(@data, ...)` directly without filtering on the domain column,
#' so the combined vector mixes both kinds of rows. The user has no
#' per-element marker for domain membership — this is a known limitation of
#' `pull` at the per-survey verb level (not the collection layer). Use a
#' per-member [filter()] or [tibble::tibble()] before pulling if domain
#' filtering is required.
pull.survey_collection <- function(
  .data,
  var = -1,
  name = NULL,
  ...,
  .if_missing_var = NULL
) {
  resolved_if_missing_var <- .if_missing_var %||% .data@if_missing_var

  var_quo <- rlang::enquo(var)
  name_quo <- rlang::enquo(name)

  # Detect the by-survey sentinel: `name = coll@id` resolves to the same string
  # as `.data@id` in the calling environment. We detect by evaluating the
  # quosure once; if it returns a length-1 character matching `.data@id`, we
  # apply the per-survey naming convention after combining.
  name_val <- tryCatch(
    rlang::eval_tidy(name_quo),
    error = function(e) NULL
  )
  use_by_survey_naming <- is.character(name_val) &&
    length(name_val) == 1L &&
    !is.na(name_val) &&
    identical(name_val, .data@id)

  # When using the by-survey sentinel, do NOT pass `name` through to
  # dplyr::pull — that would attempt to look up a column named `.data@id`
  # inside each member's @data and almost always fail. We pull unnamed and
  # apply the by-survey naming after combining.
  inner_name_quo <- if (use_by_survey_naming) {
    rlang::quo(NULL)
  } else {
    name_quo
  }

  results <- vector("list", length(.data@surveys))
  names(results) <- names(.data@surveys)
  skipped <- character(0L)

  for (nm in names(.data@surveys)) {
    member <- .data@surveys[[nm]]
    r <- .pull_apply_class_catch(
      member,
      var_quo,
      inner_name_quo,
      nm,
      resolved_if_missing_var
    )
    if (is.null(r)) {
      skipped <- c(skipped, nm)
    } else {
      results[[nm]] <- r
    }
  }

  results <- results[!vapply(results, is.null, logical(1L))]

  if (length(skipped) > 0L) {
    cli::cli_inform(
      c(
        "i" = "{.fn pull} skipped survey{?s} {.val {skipped}} (missing referenced variable{?s})."
      ),
      class = "surveytidy_message_collection_skipped_surveys"
    )
  }

  if (length(results) == 0L) {
    id_from_stored <- is.null(.if_missing_var)
    cli::cli_abort(
      c(
        "x" = "{.fn pull} produced an empty result.",
        "i" = if (identical(resolved_if_missing_var, "skip")) {
          "All surveys were skipped because they were missing referenced variables."
        } else {
          "All surveys produced empty results."
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

  # Strip outer list names before splicing — we apply per-survey naming
  # ourselves below when `use_by_survey_naming = TRUE`. Leaving the outer
  # names on the splice would force `vctrs::vec_c` into a name-merge path
  # that errors when any element has length > 1.
  unnamed_results <- unname(results)
  combined <- tryCatch(
    rlang::inject(vctrs::vec_c(!!!unnamed_results)),
    vctrs_error_incompatible_type = function(cnd) {
      members <- names(results)
      cli::cli_abort(
        c(
          "x" = "{.fn pull} cannot combine {.val {as.character(rlang::as_label(var_quo))}}: incompatible types across surveys.",
          "i" = "Surveys involved: {.val {members}}.",
          "v" = "Coerce the column to a common type with {.fn mutate} before {.fn pull}, or pull each survey individually."
        ),
        parent = cnd,
        class = "surveytidy_error_collection_pull_incompatible_types"
      )
    }
  )

  if (use_by_survey_naming) {
    names(combined) <- unlist(
      lapply(
        names(results),
        function(nm) rep_len(nm, length(results[[nm]]))
      ),
      use.names = FALSE
    )
  }

  combined
}


# ── glimpse.survey_collection ────────────────────────────────────────────────

# Detect type conflicts between per-member columns. For each column present in
# `combined`, walk the per-member `@data`, collect the classes that appear,
# and return a tibble enumerating columns whose member classes disagree.
.detect_glimpse_type_conflicts <- function(members) {
  all_cols <- unique(unlist(lapply(members, function(m) names(m@data))))
  conflicts <- list()
  for (col in all_cols) {
    classes <- list()
    for (nm in names(members)) {
      data <- members[[nm]]@data
      if (col %in% names(data)) {
        cls <- class(data[[col]])[1L]
        classes[[cls]] <- c(classes[[cls]], nm)
      }
    }
    if (length(classes) > 1L) {
      conflicts[[col]] <- classes
    }
  }
  conflicts
}

# Render the type-coercion footer per spec §V.2 step 4 / D7. Truncate at 5
# rows; line width capped at 80 chars. Caller passes the bound-tibble's
# resulting class for each conflicting column so the "→ coerced to <X>"
# hint matches what `bind_rows` actually produced.
.render_glimpse_type_conflict_footer <- function(conflicts, combined) {
  if (length(conflicts) == 0L) {
    return(invisible(NULL))
  }
  arrow <- if (cli::is_utf8_output()) "\u2192" else "->"
  cat("! Columns with conflicting types:\n")
  truncate_at <- 5L
  shown <- conflicts[seq_len(min(length(conflicts), truncate_at))]
  for (col in names(shown)) {
    classes <- shown[[col]]
    coerced <- class(combined[[col]])[1L]
    parts <- character(0L)
    for (cls in names(classes)) {
      surveys <- classes[[cls]]
      parts <- c(
        parts,
        paste0("<", cls, "> (", paste(surveys, collapse = ", "), ")")
      )
    }
    line <- paste0(
      "  ",
      col,
      ": ",
      paste(parts, collapse = "; "),
      "  ",
      arrow,
      " coerced to <",
      coerced,
      ">"
    )
    if (nchar(line) > 80L) {
      line <- paste0(substr(line, 1L, 77L), "...")
    }
    cat(line, "\n", sep = "")
  }
  if (length(conflicts) > truncate_at) {
    n_more <- length(conflicts) - truncate_at
    plural <- if (n_more == 1L) "column" else "columns"
    cat("  + ", n_more, " more conflicting ", plural, "\n", sep = "")
  }
  invisible(NULL)
}

# Rename the surveycore domain column to `.in_domain` for display only. The
# member `@data` is untouched — only the local data frame used for printing
# gets the rename.
.rename_domain_for_display <- function(df) {
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  if (domain_col %in% names(df)) {
    names(df)[match(domain_col, names(df))] <- ".in_domain"
  }
  df
}

#' @rdname glimpse
#' @method glimpse survey_collection
#'
#' @param .by_survey If `TRUE`, render a separate labelled glimpse block per
#'   member, prefixed by the member name. Default `FALSE` renders a single
#'   bound tibble with the source survey id prepended as `coll@id` (default
#'   `.survey`).
#'
#' @section Survey collections:
#' Default mode binds every member's `@data` into a single tibble (via
#' [dplyr::bind_rows()] with `.id = coll@id`) and glimpses the result. If any
#' member's `@data` already contains a column named `coll@id`, the
#' surveytidy_error_collection_glimpse_id_collision error is raised BEFORE
#' binding — symmetric with surveycore's
#' `surveycore_error_collection_id_collision` for the construction-time case.
#' Resolve by renaming the colliding column or setting a different `coll@id`
#' via [surveycore::set_collection_id()].
#'
#' Internal column rename: when the member `@data` contains
#' [surveycore::SURVEYCORE_DOMAIN_COL] (`..surveycore_domain..`), the column
#' is renamed to `.in_domain` for the rendered output. Per-member `@data` is
#' untouched.
#'
#' Type-coercion footer: when `bind_rows()` coerces conflicting types across
#' members (e.g., `<chr>` vs `<dbl>`), a footer enumerates the affected
#' columns. Truncates after 5 columns; line width capped at 80 characters.
#' No opt-out — the footer renders only when conflicts exist.
#'
#' @return `x` invisibly.
glimpse.survey_collection <- function(
  x,
  width = NULL,
  ...,
  .by_survey = FALSE
) {
  if (isTRUE(.by_survey)) {
    for (nm in names(x@surveys)) {
      member <- x@surveys[[nm]]
      cat(symbol_pointer(), " ", nm, "\n", sep = "")
      member_data <- .rename_domain_for_display(member@data)
      dplyr::glimpse(member_data, width = width)
    }
    return(invisible(x))
  }

  # Default mode: pre-flight collision check, then bind + glimpse.
  id_col <- x@id
  collisions <- character(0L)
  for (nm in names(x@surveys)) {
    if (id_col %in% names(x@surveys[[nm]]@data)) {
      collisions <- c(collisions, nm)
    }
  }
  if (length(collisions) > 0L) {
    member_list <- paste(collisions, collapse = ", ")
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn glimpse} on a {.cls survey_collection} would collide on ",
          "column {.field ",
          id_col,
          "}."
        ),
        "i" = paste0(
          "Members already containing a {.field ",
          id_col,
          "} column: ",
          member_list,
          ". The prepended id from {.code coll@id} would clash on ",
          "{.fn bind_rows}."
        ),
        "v" = "Rename the colliding column with {.fn rename}, or set a different {.code coll@id} via {.fn surveycore::set_collection_id} before {.fn glimpse}."
      ),
      class = "surveytidy_error_collection_glimpse_id_collision"
    )
  }

  conflicts <- .detect_glimpse_type_conflicts(x@surveys)

  # vctrs no longer auto-coerces incompatible types in `bind_rows`. To keep
  # `glimpse` diagnostic (the spec contract: render the bound tibble + a
  # footer enumerating coercions), pre-coerce conflicting columns to a
  # common type before binding. Try `vctrs::vec_ptype_common` first; fall
  # back to character if vctrs has no common ptype.
  per_member <- lapply(x@surveys, function(s) s@data)
  per_member <- .coerce_conflicting_columns(per_member, conflicts)

  combined <- dplyr::bind_rows(per_member, .id = id_col)
  combined <- .rename_domain_for_display(combined)

  dplyr::glimpse(combined, width = width)
  .render_glimpse_type_conflict_footer(conflicts, combined)

  invisible(x)
}

# Coerce columns named in `conflicts` to a common type across the per-member
# data frames so `dplyr::bind_rows` does not raise
# `vctrs_error_incompatible_type`. The footer renderer (called after the
# bound glimpse) reports the resolved type per spec §V.2 step 4.
.coerce_conflicting_columns <- function(per_member, conflicts) {
  if (length(conflicts) == 0L) {
    return(per_member)
  }
  for (col in names(conflicts)) {
    cols <- lapply(per_member, function(d) {
      if (col %in% names(d)) d[[col]] else NULL
    })
    cols <- cols[!vapply(cols, is.null, logical(1L))]
    common <- tryCatch(
      rlang::inject(vctrs::vec_ptype_common(!!!cols)),
      vctrs_error_incompatible_type = function(cnd) character()
    )
    for (i in seq_along(per_member)) {
      d <- per_member[[i]]
      if (col %in% names(d)) {
        d[[col]] <- tryCatch(
          vctrs::vec_cast(d[[col]], common),
          vctrs_error_incompatible_type = function(cnd) {
            as.character(d[[col]])
          },
          vctrs_error_cast_lossy = function(cnd) {
            as.character(d[[col]])
          }
        )
        per_member[[i]] <- d
      }
    }
  }
  per_member
}

# Resolve the unicode pointer character used in `.by_survey = TRUE` headers.
# Inlined here (not in utils.R) because it is only consumed from this file.
# `▸` is BLACK RIGHT-POINTING SMALL TRIANGLE; the ASCII fallback
# avoids R CMD check's "non-ASCII characters in source" complaint.
symbol_pointer <- function() {
  if (cli::is_utf8_output()) "\u25b8" else ">"
}
