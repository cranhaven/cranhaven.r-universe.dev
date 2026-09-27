# R/update-design.R
#
# update_design(): update design variables on an existing survey object.
# Covers error-messages.md row 36 (cli_inform on update).

# ── Internal helper ───────────────────────────────────────────────────────────

# Resolve a tidy-select argument for update_design().
# Returns NULL if quo is NULL (no update requested).
# If max_cols == 1L, errors when selection is not exactly 1 column.
# Otherwise returns all resolved column names.
#' @noRd
.resolve_update_arg <- function(quo, data, arg_name, max_cols = 1L) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  cols <- tidyselect::eval_select(quo, data)
  if (max_cols == 1L && length(cols) != 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg {arg_name}} must select exactly one column, ",
          "not {length(cols)}"
        )
      ),
      class = paste0("surveycore_error_", arg_name, "_multiple")
    )
  }
  names(cols)
}


# ── update_design ─────────────────────────────────────────────────────────────

#' Update Design Variables on an Existing Survey Object
#'
#' Updates one or more design variables (weights, cluster IDs, strata, FPC,
#' or replicate weights) on an existing survey design object. Use this after
#' modifying the underlying data — for example, after recalibrating weights or
#' adding a stratification variable. Emits an informational message listing
#' changed variables.
#'
#' @param x A `survey_taylor` or `survey_replicate` object. `survey_twophase`
#'   is not supported; create a new design with [as_survey_twophase()].
#' @param ids <[`tidy-select`][tidyselect::language]> New cluster (PSU) ID
#'   column(s). `NULL` (default) means no change. Only used for
#'   `survey_taylor` objects.
#' @param weights <[`tidy-select`][tidyselect::language]> New weight column
#'   (a single column, values strictly > 0). `NULL` (default) means no change.
#' @param strata <[`tidy-select`][tidyselect::language]> New stratification
#'   column (a single column). `NULL` (default) means no change. Only used
#'   for `survey_taylor` objects.
#' @param fpc <[`tidy-select`][tidyselect::language]> New finite population
#'   correction column (a single column). `NULL` (default) means no change.
#'   Only used for `survey_taylor` objects.
#' @param repweights <[`tidy-select`][tidyselect::language]> New replicate
#'   weight columns (one or more). `NULL` (default) means no change. Only
#'   used for `survey_replicate` objects.
#' @param validate Logical. If `FALSE`, temporarily marks the object to
#'   suppress validation during the variable update. In practice this has
#'   no observable effect on the returned object; `validate` is accepted
#'   for interface compatibility. Default `TRUE`.
#'
#' @return The modified survey object, invisibly. As a side effect, a
#'   `cli_inform()` message is printed listing each changed design variable
#'   (old name → new name).
#'
#' @examples
#' # NHANES has two weight columns for different analysis types;
#' # start with the MEC examination weight for exam participants
#' exam <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
#' d <- as_survey(
#'   exam,
#'   ids = sdmvpsu,
#'   weights = wtmec2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#'
#' # Switch to interview weight for interview-based variables
#' d_updated <- update_design(d, weights = wtint2yr)
#' @seealso
#'   [as_survey()] to create a `survey_taylor` object,
#'   [as_survey_replicate()] to create a `survey_replicate` object,
#'   [as_survey_twophase()] to create a `survey_twophase` object
#'
#' @export
update_design <- function(
  x,
  ids = NULL,
  weights = NULL,
  strata = NULL,
  fpc = NULL,
  repweights = NULL,
  validate = TRUE
) {
  # Capture tidy-select arguments before dispatch
  ids_quo <- rlang::enquo(ids)
  weights_quo <- rlang::enquo(weights)
  strata_quo <- rlang::enquo(strata)
  fpc_quo <- rlang::enquo(fpc)
  repweights_quo <- rlang::enquo(repweights)

  # ── Dispatch by class ───────────────────────────────────────────────────────

  if (S7::S7_inherits(x, survey_taylor)) {
    changed_vars <- character(0L)
    new_variables <- x@variables

    new_ids <- .resolve_update_arg(ids_quo, x@data, "ids", max_cols = Inf)
    new_weights <- .resolve_update_arg(
      weights_quo,
      x@data,
      "weights",
      max_cols = 1L
    )
    new_strata <- .resolve_update_arg(
      strata_quo,
      x@data,
      "strata",
      max_cols = 1L
    )
    new_fpc <- .resolve_update_arg(fpc_quo, x@data, "fpc", max_cols = 1L)

    if (!is.null(new_ids)) {
      new_variables$ids <- new_ids
      changed_vars <- c(changed_vars, "ids")
    }
    if (!is.null(new_weights)) {
      new_variables$weights <- new_weights
      changed_vars <- c(changed_vars, "weights")
    }
    if (!is.null(new_strata)) {
      new_variables$strata <- new_strata
      changed_vars <- c(changed_vars, "strata")
    }
    if (!is.null(new_fpc)) {
      new_variables$fpc <- new_fpc
      changed_vars <- c(changed_vars, "fpc")
    }

    # Warn when weights change on a calibrated design (CAL-13)
    if (!is.null(new_weights) && !is.null(x@calibration)) {
      cli::cli_warn(
        c(
          "!" = "Weight column changed on a calibrated design.",
          "i" = paste0(
            "{.field @calibration} was built from the previous weight column."
          ),
          "v" = paste0(
            "Re-run calibration or set ",
            "{.code design@calibration <- NULL} before analysing."
          )
        ),
        class = "surveycore_warning_weight_change_invalidates_calibration"
      )
    }

    if (length(changed_vars) > 0L) {
      if (!isTRUE(validate)) {
        attr(x, ".should_validate") <- FALSE
      }
      x@variables <- new_variables
      if (!isTRUE(validate)) attr(x, ".should_validate") <- NULL
    }
  } else if (S7::S7_inherits(x, survey_replicate)) {
    changed_vars <- character(0L)
    new_variables <- x@variables

    new_weights <- .resolve_update_arg(
      weights_quo,
      x@data,
      "weights",
      max_cols = 1L
    )
    new_repweights <- .resolve_update_arg(
      repweights_quo,
      x@data,
      "repweights",
      max_cols = Inf
    )

    if (!is.null(new_weights)) {
      new_variables$weights <- new_weights
      changed_vars <- c(changed_vars, "weights")
    }
    if (!is.null(new_repweights)) {
      new_variables$repweights <- new_repweights
      changed_vars <- c(changed_vars, "repweights")
    }

    # Warn when weights change on a calibrated design (CAL-13)
    if (!is.null(new_weights) && !is.null(x@calibration)) {
      cli::cli_warn(
        c(
          "!" = "Weight column changed on a calibrated design.",
          "i" = paste0(
            "{.field @calibration} was built from the previous weight column."
          ),
          "v" = paste0(
            "Re-run calibration or set ",
            "{.code design@calibration <- NULL} before analysing."
          )
        ),
        class = "surveycore_warning_weight_change_invalidates_calibration"
      )
    }

    if (length(changed_vars) > 0L) {
      if (!isTRUE(validate)) {
        attr(x, ".should_validate") <- FALSE
      }
      x@variables <- new_variables
      if (!isTRUE(validate)) attr(x, ".should_validate") <- NULL
    }
  } else if (S7::S7_inherits(x, survey_twophase)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn update_design} does not support ",
          "{.cls survey_twophase} objects."
        ),
        "i" = "Create a new design with {.fn as_survey_twophase}."
      ),
      class = "surveycore_error_unsupported_class"
    )
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a {.cls survey_taylor} or ",
          "{.cls survey_replicate} object, not ",
          "{.cls {class(x)[[1L]]}}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }

  # ── Inform message when variables changed (error-messages.md row 36) ────────
  if (length(changed_vars) > 0L) {
    cli::cli_inform(
      c(
        "i" = "Survey design updated. This may affect statistical validity.",
        "i" = "Updated: {.field {changed_vars}}"
      )
    )
  }

  invisible(x)
}
