# R/metadata-infer.R
#
# Auto-detection of shared question prefaces from variable labels.
#
# Exported function:
#   infer_question_prefaces()  — detect and apply shared prefaces
#
# Internal helpers (used only in this file):
#   .collect_labels_survey()   — label map from a survey object
#   .collect_labels_df()       — label map from a data frame
#   .find_lcp()                — character-level longest common prefix
#   .trim_to_word_boundary()   — trim an LCP string to the last word boundary

# ── Internal helpers ─────────────────────────────────────────────────────────

# Collect a named character vector of variable labels from a survey object.
# Merges @metadata@variable_labels with a haven-style attr("label") fallback so
# data imported from haven that has not been explicitly labelled is processed.
# Returns only columns that have a non-empty label from either source.
.collect_labels_survey <- function(x) {
  result <- character(0L)
  for (col_name in names(x@data)) {
    lbl <- x@metadata@variable_labels[[col_name]]
    if (is.null(lbl)) {
      raw <- attr(x@data[[col_name]], "label", exact = TRUE)
      if (
        !is.null(raw) &&
          is.character(raw) &&
          length(raw) > 0L &&
          nzchar(raw[[1L]])
      ) {
        lbl <- raw[[1L]]
      }
    }
    if (!is.null(lbl) && is.character(lbl) && nzchar(lbl)) {
      result[[col_name]] <- lbl
    }
  }
  result
}


# Collect a named character vector of variable labels from a data frame.
# Reads attr(col, "label", exact = TRUE) for each column.
.collect_labels_df <- function(x) {
  result <- character(0L)
  for (col_name in names(x)) {
    raw <- attr(x[[col_name]], "label", exact = TRUE)
    if (
      !is.null(raw) &&
        is.character(raw) &&
        length(raw) > 0L &&
        nzchar(raw[[1L]])
    ) {
      result[[col_name]] <- raw[[1L]]
    }
  }
  result
}


# Compute the character-level longest common prefix of a character vector.
.find_lcp <- function(strings) {
  if (length(strings) == 0L) {
    return("")
  }
  if (length(strings) == 1L) {
    return(strings[[1L]])
  }
  min_len <- min(nchar(strings))
  if (min_len == 0L) {
    return("")
  }
  ref <- strings[[1L]]
  for (i in seq_len(min_len)) {
    if (!all(substr(strings, i, i) == substr(ref, i, i))) {
      return(substr(ref, 1L, i - 1L))
    }
  }
  substr(ref, 1L, min_len)
}


# Trim a string to the last whitespace boundary (no sub-word splits).
# Trailing whitespace is stripped first; if no internal whitespace remains,
# the trimmed string is returned as-is.
.trim_to_word_boundary <- function(prefix) {
  if (!nzchar(prefix)) {
    return("")
  }
  trimmed <- trimws(prefix, which = "right")
  if (!nzchar(trimmed)) {
    return("")
  }
  spaces <- gregexpr("\\s", trimmed, perl = TRUE)[[1L]]
  last_space <- spaces[[length(spaces)]]
  if (last_space == -1L) {
    return(trimmed)
  }
  substr(trimmed, 1L, last_space - 1L)
}


# ── Main exported function ────────────────────────────────────────────────────

#' Infer Question Prefaces from Variable Labels
#'
#' Scans variable labels in a survey design object or labelled data frame for
#' groups of variables sharing a common preface (via separator or longest
#' common prefix). For survey design objects, detected prefaces are written to
#' `@metadata@question_prefaces`. For data frames, prefaces are written to
#' `attr(col, "question_preface")` on each column (no metadata object exists
#' until `as_survey()` is called). The shared text is trimmed from each
#' variable label, leaving only the unique suffix.
#'
#' @details
#' **Detection algorithm (two passes):**
#'
#' 1. **Separator pass** — for each separator in `sep` (tried in order):
#'    - Variables whose label contains the separator are grouped by their
#'      candidate preface (text before the *first* occurrence of the separator,
#'      trimmed).
#'    - Any group with \eqn{\geq} `min_vars` members is recorded; those
#'      variables are excluded from all subsequent passes.
#'
#' 2. **LCP pass** — for remaining labelled variables (\eqn{\geq} 2):
#'    - The character-level longest common prefix (LCP) of all remaining
#'      labels is computed and trimmed to the last word boundary.
#'    - If the trimmed LCP is \eqn{\geq} `lcp_min` characters, the group
#'      is recorded.
#'
#' **Apply step:**
#' - Variables with an existing `question_preface` are skipped when
#'   `overwrite = FALSE` (default); a warning is emitted listing the count
#'   of skipped variables.
#' - Variables whose unique suffix would be empty after trimming are always
#'   skipped with a per-variable warning.
#'
#' **Data frame integration:**
#' When called on a data frame, the detected preface is written to
#' `attr(col, "question_preface")`. Passing the result to [as_survey()]
#' automatically picks up both the trimmed label and the preface via the
#' internal haven metadata extraction step.
#'
#' @param x A survey design object (`survey_taylor`, `survey_replicate`, etc.)
#'   **or** a data frame with haven-style `"label"` attributes.
#' @param sep Character vector of literal separator strings to try, in
#'   priority order. Default: `c(" - ", "- ", " \u2013 ", ": ", " | ")`.
#' @param min_vars Minimum number of variables that must share a candidate
#'   preface to trigger extraction. Default `2L`.
#' @param lcp_min Minimum character length (after trimming to a word boundary)
#'   for an LCP-derived preface to be accepted. Default `20L`.
#' @param overwrite If `FALSE` (default), variables that already have a
#'   `question_preface` are skipped and a warning is emitted. Set `TRUE` to
#'   replace existing prefaces without warning.
#' @param verbose If `TRUE` (default), emits a cli summary for each detected
#'   group.
#'
#' @return The modified `x`, invisibly.
#'
#' @examples
#' # Data frame with haven-style labels (Qualtrics / SPSS export pattern)
#' df <- data.frame(
#'   discrim_a = 1:5,
#'   discrim_b = 2:6,
#'   discrim_c = 3:7
#' )
#' attr(df$discrim_a, "label") <-
#'   "Please rate discrimination - Evangelical Christians"
#' attr(df$discrim_b, "label") <-
#'   "Please rate discrimination - Muslims"
#' attr(df$discrim_c, "label") <-
#'   "Please rate discrimination - Jews"
#'
#' df <- infer_question_prefaces(df, verbose = FALSE)
#' attr(df$discrim_a, "label") # "Evangelical Christians"
#' attr(df$discrim_a, "question_preface") # "Please rate discrimination"
#' @family metadata
#' @export
infer_question_prefaces <- function(
  x,
  sep = c(" - ", "- ", " \u2013 ", ": ", " | "),
  min_vars = 2L,
  lcp_min = 20L,
  overwrite = FALSE,
  verbose = TRUE
) {
  is_survey <- S7::S7_inherits(x, survey_base)
  is_df <- is.data.frame(x)

  if (!is_survey && !is_df) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a survey design object or a data frame,",
          " not {.cls {class(x)[[1L]]}}."
        )
      ),
      class = "surveycore_error_not_survey_or_df"
    )
  }

  # Collect labels as a named character vector (var_name → label text).
  labels <- if (is_survey) .collect_labels_survey(x) else .collect_labels_df(x)
  if (length(labels) == 0L) {
    return(invisible(x))
  }

  # ── Detection ───────────────────────────────────────────────────────────────

  remaining <- names(labels) # var names not yet assigned to a group
  groups <- list() # each entry: list(preface, vars, suffixes)

  # Pass 1: separator-based detection
  for (s in sep) {
    if (length(remaining) == 0L) {
      break
    }

    has_sep <- remaining[
      vapply(
        labels[remaining],
        function(lbl) grepl(s, lbl, fixed = TRUE),
        logical(1L)
      )
    ]
    if (length(has_sep) < min_vars) {
      next
    }

    # For each var with the separator, compute its candidate preface
    # (text before the FIRST occurrence, trimmed).
    candidate_prefaces <- vapply(
      has_sep,
      function(v) {
        lbl <- labels[[v]]
        pos <- regexpr(s, lbl, fixed = TRUE)[[1L]]
        trimws(substr(lbl, 1L, pos - 1L))
      },
      character(1L)
    )

    # Group vars by identical candidate preface.
    by_preface <- split(has_sep, candidate_prefaces)

    for (preface in names(by_preface)) {
      v_group <- by_preface[[preface]]
      if (length(v_group) < min_vars) {
        next
      }

      suffixes <- vapply(
        v_group,
        function(v) {
          lbl <- labels[[v]]
          pos <- regexpr(s, lbl, fixed = TRUE)[[1L]]
          trimws(substr(lbl, pos + nchar(s), nchar(lbl)))
        },
        character(1L)
      )

      groups <- c(
        groups,
        list(list(
          preface = preface,
          vars = v_group,
          suffixes = suffixes
        ))
      )
      remaining <- setdiff(remaining, v_group)
    }
  }

  # Pass 2: LCP-based detection for remaining labelled variables.
  if (length(remaining) >= 2L) {
    lcp <- .find_lcp(unname(labels[remaining]))
    lcp_trimmed <- .trim_to_word_boundary(lcp)
    if (nchar(lcp_trimmed) >= lcp_min) {
      suffixes <- vapply(
        remaining,
        function(v) {
          trimws(substr(
            labels[[v]],
            nchar(lcp_trimmed) + 1L,
            nchar(labels[[v]])
          ))
        },
        character(1L)
      )
      groups <- c(
        groups,
        list(list(
          preface = lcp_trimmed,
          vars = remaining,
          suffixes = suffixes
        ))
      )
    }
  }

  if (length(groups) == 0L) {
    return(invisible(x))
  }

  # ── Apply ───────────────────────────────────────────────────────────────────

  skipped <- character(0L) # vars skipped due to existing preface + overwrite=F

  for (grp in groups) {
    applied_vars <- character(0L)

    for (i in seq_along(grp$vars)) {
      var_name <- grp$vars[[i]]
      suffix <- grp$suffixes[[i]]

      # Skip if var already has a preface and overwrite = FALSE.
      if (!overwrite) {
        existing <- if (is_survey) {
          x@metadata@question_prefaces[[var_name]]
        } else {
          attr(x[[var_name]], "question_preface", exact = TRUE)
        }
        if (!is.null(existing)) {
          skipped <- c(skipped, var_name)
          next
        }
      }

      # Skip if trimming would leave an empty label.
      if (!nzchar(suffix)) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "Variable {.field {var_name}} would have an empty label",
              " after trimming the preface. Skipping."
            )
          ),
          class = "surveycore_warning_empty_label_after_trim"
        )
        next
      }

      # Write preface and trimmed label.
      if (is_survey) {
        x@metadata@question_prefaces[[var_name]] <- grp$preface
        x@metadata@variable_labels[[var_name]] <- suffix
      } else {
        attr(x[[var_name]], "question_preface") <- grp$preface
        attr(x[[var_name]], "label") <- suffix
      }
      applied_vars <- c(applied_vars, var_name)
    }

    if (verbose && length(applied_vars) > 0L) {
      cli::cli_inform(c(
        "v" = paste0(
          "Detected preface for {length(applied_vars)} variable{?s}: ",
          "{.field {applied_vars}}"
        ),
        "i" = "Preface: {.val {grp$preface}}"
      ))
    }
  }

  if (length(skipped) > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "{length(skipped)} variable{?s} already {?has/have} a question",
          " preface and {?was/were} skipped."
        ),
        "i" = "Set {.arg overwrite = TRUE} to replace them."
      ),
      class = "surveycore_warning_preface_not_overwritten"
    )
  }

  invisible(x)
}
