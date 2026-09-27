# R/collection-dispatch.R
#
# Internal dispatcher used by every verb method registered against
# "surveycore::survey_collection". Walks each member, applies the verb
# (with one of three missing-variable detection modes), then rebuilds
# the output collection through `surveycore::as_survey_collection()`.
#
# Spec §II.3.1 (six-step contract) and §VII.1 (typed conditions).
# This file ships with PR 1; verb methods that route through it land in
# PRs 2a/2b/2c/2d.

# ── Roxygen stub for shared parameter docs (D6) ───────────────────────────────

#' Shared parameters for survey_collection verb methods
#'
#' @param .if_missing_var Per-call override of `collection@if_missing_var`.
#'   One of `"error"` or `"skip"`, or `NULL` (the default) to inherit the
#'   collection's stored value. See
#'   [surveycore::set_collection_if_missing_var()].
#'
#' @return The modified collection, with members updated by the dispatched
#'   verb.
#'
#' @name survey_collection_args
#' @keywords internal
NULL


# ── Pre-check helper (data-masking verbs) ─────────────────────────────────────

# Walks each captured quosure in `dots`, extracts referenced bare names via
# all.vars(), filters them down to the truly-unresolved set, and synthesises a
# typed sentinel condition the moment a missing name is found. Returns NULL
# when nothing is missing on `survey`.
#
# Class chain on the sentinel deliberately omits "rlang_error" (D1 / Issue 3)
# so dispatcher tests can distinguish the pre-check path from the
# class-catch path via `inherits(cnd$parent, "rlang_error")`.
.pre_check_missing_vars <- function(dots, survey, survey_name) {
  data_cols <- names(survey@data)
  dot_names <- names(dots) %||% rep("", length(dots))
  for (i in seq_along(dots)) {
    # Skip dotted-name scalars (e.g., `.keep`, `.before`) — they are not
    # data-mask expressions and may already be unwrapped to plain values.
    if (startsWith(dot_names[[i]], ".")) {
      next
    }
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    candidate <- all.vars(expr)
    candidate <- setdiff(candidate, c(".data", ".env"))
    if (length(candidate) == 0L) {
      next
    }
    keep <- !vapply(
      candidate,
      function(nm) exists(nm, envir = env, inherits = TRUE),
      logical(1L)
    )
    candidate <- candidate[keep]
    if (length(candidate) == 0L) {
      next
    }
    missing_vars <- setdiff(candidate, data_cols)
    if (length(missing_vars) > 0L) {
      return(structure(
        list(
          message = paste0(
            "Survey '",
            survey_name,
            "' is missing referenced variable",
            if (length(missing_vars) > 1L) "s" else "",
            ": ",
            paste(missing_vars, collapse = ", ")
          ),
          missing_vars = missing_vars,
          survey_name = survey_name,
          quosure = quo,
          call = NULL
        ),
        class = c("surveytidy_pre_check_missing_var", "error", "condition")
      ))
    }
  }
  NULL
}


# ── Class-catch helper (tidyselect verbs) ─────────────────────────────────────

# Decide what to do with a caught tidyselect/rlang condition that has been
# identified as a missing-variable signal. Under "skip", returns NULL so the
# tryCatch result becomes NULL. Under "error", re-raises the typed
# `surveytidy_error_collection_verb_failed` with `cnd` chained as `parent`.
.handle_class_catch <- function(
  cnd,
  survey_name,
  verb_name,
  resolved_if_missing_var
) {
  if (identical(resolved_if_missing_var, "skip")) {
    return(NULL)
  }
  cli::cli_abort(
    c(
      "x" = "{.fn {verb_name}} failed on survey {.val {survey_name}}: referenced column not found."
    ),
    parent = cnd,
    class = "surveytidy_error_collection_verb_failed"
  )
}


# ── Scalar-dot unwrapper ──────────────────────────────────────────────────────

# `enquos(...)` wraps every `...` argument as a quosure to preserve data-mask
# semantics for bare-name expressions (e.g., `y1 > 50`, `region`). But scalar
# control args whose names start with `.` (e.g., `.keep`, `.before`, `.after`,
# `.preserve`) are not data-mask targets — they are values the receiving verb
# matches directly (e.g., `match.arg(.keep)`). When spliced via `!!!`, those
# stay wrapped as quosures unless we evaluate them first. This helper walks
# the captured quosures, evaluates any whose name begins with `.`, and returns
# a bare list suitable for `rlang::inject(fn(survey, !!!list))`.
.unwrap_scalar_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(dots)
  }
  nms <- names(dots) %||% rep("", length(dots))
  # Build a fresh list to avoid `[[<- NULL` deletion semantics — when a
  # scalar evaluates to NULL we still want the named slot present.
  out <- vector("list", length(dots))
  names(out) <- nms
  for (i in seq_along(dots)) {
    if (startsWith(nms[[i]], ".")) {
      out[i] <- list(rlang::eval_tidy(dots[[i]]))
    } else {
      out[[i]] <- dots[[i]]
    }
  }
  out
}


# ── Per-member apply (class-catch path) ───────────────────────────────────────

# Wraps `fn(survey, ...)` in tryCatch with handlers keyed on the two
# missing-variable conditions tidyselect / rlang reliably raise. The
# `rlang_error` handler walks one level of `cnd$parent` to recover the
# `all_of()` wrap case (where the user-facing condition is a generic
# `rlang_error` whose parent is `vctrs_error_subscript_oob`).
.apply_class_catch <- function(
  fn,
  survey,
  dots,
  survey_name,
  verb_name,
  resolved_if_missing_var,
  scalar_args = list()
) {
  tryCatch(
    rlang::inject(fn(survey, !!!dots, !!!scalar_args)),
    vctrs_error_subscript_oob = function(cnd) {
      .handle_class_catch(cnd, survey_name, verb_name, resolved_if_missing_var)
    },
    rlang_error_data_pronoun_not_found = function(cnd) {
      .handle_class_catch(cnd, survey_name, verb_name, resolved_if_missing_var)
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
          survey_name,
          verb_name,
          resolved_if_missing_var
        ))
      }
      stop(cnd)
    }
  )
}


# ── Dispatcher ────────────────────────────────────────────────────────────────

# Internal — called by every collection verb method. Spec §II.3.1.
#
# Parameters per the spec table:
#   fn                  the dplyr/tidyr generic (e.g., dplyr::filter)
#   verb_name           string — verb's bound name; used in messages
#   collection          a survey_collection
#   ...                 NSE-aware forwarding of data-mask / tidyselect args
#   .if_missing_var     per-call override; NULL inherits stored property
#   .detect_missing     "pre_check" / "class_catch" / "none" (default)
#   .may_change_groups  FALSE (default) → assert @groups invariance
.dispatch_verb_over_collection <- function(
  fn,
  verb_name,
  collection,
  ...,
  .scalar_args = list(),
  .if_missing_var = NULL,
  .detect_missing = "none",
  .may_change_groups = FALSE
) {
  # Step 1 + 1.5: resolve and track override source.
  resolved_if_missing_var <- .if_missing_var %||% collection@if_missing_var
  id_from_stored <- is.null(.if_missing_var)

  dots <- rlang::enquos(...)
  # Dotted-name args (e.g., `.keep`, `.before`, `.after`, `.preserve`) are
  # scalar control parameters — they must be evaluated to plain values before
  # `rlang::inject(fn(survey, !!!dots))` so the receiving verb method does not
  # see a quosure where it expects a string / logical / NSE expression.
  # Bare-name args remain quosures so data-masking semantics work.
  dots <- .unwrap_scalar_dots(dots)

  # Drop NULL entries from `.scalar_args`: dplyr 1.2.0 treats an explicit
  # `prop = NULL` as "user supplied prop", which (combined with a non-NULL
  # `n`) raises "Must supply `n` or `prop`, but not both." Verb methods at
  # the slice family pass scalars via `.scalar_args` so this dispatcher can
  # drop NULLs uniformly without requiring each verb method to do it.
  if (length(.scalar_args) > 0L) {
    .scalar_args <- .scalar_args[
      !vapply(.scalar_args, is.null, logical(1L))
    ]
  }

  # Step 2: per-member apply with detection mode.
  results <- vector("list", length(collection@surveys))
  names(results) <- names(collection@surveys)
  skipped <- character(0L)

  for (nm in names(collection@surveys)) {
    survey <- collection@surveys[[nm]]

    if (identical(.detect_missing, "pre_check")) {
      sentinel <- .pre_check_missing_vars(dots, survey, nm)
      if (!is.null(sentinel)) {
        if (identical(resolved_if_missing_var, "skip")) {
          skipped <- c(skipped, nm)
          next
        }
        cli::cli_abort(
          c(
            "x" = "{.fn {verb_name}} failed on survey {.val {nm}}: missing referenced variable{?s} {.field {sentinel$missing_vars}}."
          ),
          parent = sentinel,
          class = "surveytidy_error_collection_verb_failed"
        )
      }
      r <- rlang::inject(fn(survey, !!!dots, !!!.scalar_args))
    } else if (identical(.detect_missing, "class_catch")) {
      r <- .apply_class_catch(
        fn,
        survey,
        dots,
        nm,
        verb_name,
        resolved_if_missing_var,
        .scalar_args
      )
    } else {
      r <- rlang::inject(fn(survey, !!!dots, !!!.scalar_args))
    }

    if (is.null(r)) {
      skipped <- c(skipped, nm)
    } else {
      results[[nm]] <- r
    }
  }

  # Drop entries the caller never filled (skipped → results[[nm]] still NULL).
  results <- results[!vapply(results, is.null, logical(1L))]

  # Step 3: typed informational message naming every skipped survey.
  if (length(skipped) > 0L) {
    cli::cli_inform(
      c(
        "i" = "{.fn {verb_name}} skipped survey{?s} {.val {skipped}} (missing referenced variable{?s})."
      ),
      class = "surveytidy_message_collection_skipped_surveys"
    )
  }

  # Step 4: empty-result proactive check (V6).
  if (length(results) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.fn {verb_name}} produced an empty {.cls survey_collection}.",
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

  # Step 5: rebuild via the documented surveycore constructor. The
  # constructor re-derives @groups from the members and runs the
  # validator once on a fully consistent state.
  out_coll <- rlang::inject(surveycore::as_survey_collection(
    !!!results,
    .id = collection@id,
    .if_missing_var = collection@if_missing_var
  ))

  if (!isTRUE(.may_change_groups)) {
    stopifnot(identical(out_coll@groups, collection@groups))
  }

  # Step 6: return.
  out_coll
}
