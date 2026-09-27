# R/methods-conversion.R
#
# Conversion functions between surveycore S7 objects and the survey/srvyr
# package design objects.
#
# Step 11 (feature/conversion-to-survey):   as_svydesign(), as_tbl_svy()
# Step 12 (feature/conversion-from-survey): from_svydesign(), from_tbl_svy()
#
# Both directions require the external package to be installed.
# Errors use surveycore_error_pkg_not_installed when the package is absent.

# ── Internal helpers ──────────────────────────────────────────────────────────

# Build a one-sided formula from a character vector of column names.
# Returns NULL when vars is NULL or length-0.
# Examples:
#   c("psu", "ssu") → ~ psu + ssu
#   "strata"        → ~ strata
#   NULL            → NULL
#' @noRd
.to_formula <- function(vars) {
  if (is.null(vars) || length(vars) == 0L) {
    return(NULL)
  }
  stats::reformulate(vars)
}


# ── as_svydesign ──────────────────────────────────────────────────────────────

#' Convert a surveycore Design Object to a survey Package Design
#'
#' Converts a `survey_taylor`, `survey_replicate`, or `survey_twophase` object
#' to the corresponding `survey` package object: `svydesign`, `svrepdesign`,
#' or `twophase`. Useful for accessing `survey` package estimation functions
#' or for round-trip testing.
#'
#' Metadata (variable labels, value labels) is NOT carried over — the `survey`
#' package has no metadata system.
#'
#' @param x A `survey_taylor`, `survey_replicate`, or `survey_twophase` object.
#' @return A `survey::svydesign`, `survey::svrepdesign`, or `survey::twophase`
#'   object.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   sv <- as_svydesign(d)
#'   survey::svymean(~ridageyr, sv, na.rm = TRUE)
#' }
#' @seealso [from_svydesign()] to convert back from a `survey` design
#' @family conversion
#' @export
as_svydesign <- function(x) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg survey} must be installed to use {.fn as_svydesign}.",
        "v" = "Install it with {.code install.packages(\"survey\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }

  if (S7::S7_inherits(x, survey_taylor)) {
    .as_svydesign_taylor(x)
  } else if (S7::S7_inherits(x, survey_replicate)) {
    .as_svydesign_replicate(x)
  } else if (S7::S7_inherits(x, survey_twophase)) {
    .as_svydesign_twophase(x)
  } else {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a survey design object.",
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_survey_object"
    )
  }
}


# survey_taylor → survey::svydesign
#' @noRd
.as_svydesign_taylor <- function(x) {
  ids_var <- x@variables$ids
  strata_var <- x@variables$strata
  weights_var <- x@variables$weights
  fpc_var <- x@variables$fpc

  ids_formula <- if (!is.null(ids_var) && length(ids_var) > 0L) {
    .to_formula(ids_var)
  } else {
    ~1
  }

  survey::svydesign(
    ids = ids_formula,
    strata = .to_formula(strata_var),
    weights = .to_formula(weights_var),
    fpc = .to_formula(fpc_var),
    data = x@data,
    nest = isTRUE(x@variables$nest)
  )
}


# survey_replicate → survey::svrepdesign
#' @noRd
.as_svydesign_replicate <- function(x) {
  wts_var <- x@variables$weights
  rep_vars <- x@variables$repweights
  fpc_var <- x@variables$fpc
  fpctype <- if (!is.null(x@variables$fpctype)) {
    x@variables$fpctype
  } else {
    "fraction"
  }

  # BRR and Fay do not use a separate scale factor — survey::svrepdesign()
  # warns if scale is passed for those types.
  scale_arg <- if (isTRUE(x@variables$type %in% c("BRR", "Fay"))) {
    NULL
  } else {
    x@variables$scale
  }

  survey::svrepdesign(
    weights = x@data[[wts_var]],
    repweights = x@data[, rep_vars, drop = FALSE],
    type = x@variables$type,
    scale = scale_arg,
    rscales = x@variables$rscales,
    mse = isTRUE(x@variables$mse),
    fpc = if (!is.null(fpc_var)) x@data[[fpc_var]] else NULL,
    fpctype = fpctype,
    data = x@data
  )
}


# survey_twophase → survey::twophase
#' @noRd
.as_svydesign_twophase <- function(x) {
  p1 <- x@variables$phase1
  p2 <- x@variables$phase2
  subset_var <- x@variables$subset
  method <- if (!is.null(x@variables$method)) x@variables$method else "full"

  # Phase 1 formulas
  p1_id <- if (!is.null(p1$ids) && length(p1$ids) > 0L) {
    .to_formula(p1$ids)
  } else {
    ~1
  }
  p1_strata <- .to_formula(p1$strata)
  p1_weights <- .to_formula(p1$weights)
  p1_fpc <- .to_formula(p1$fpc)

  # Phase 2 formulas (usually NULL or ~1 for simple cases)
  p2_id <- if (!is.null(p2) && !is.null(p2$ids) && length(p2$ids) > 0L) {
    .to_formula(p2$ids)
  } else {
    ~1
  }
  p2_strata <- if (!is.null(p2)) .to_formula(p2$strata) else NULL
  p2_fpc <- if (!is.null(p2)) .to_formula(p2$fpc) else NULL

  # method="full" derives phase 2 weights from the phase 1 design and rejects
  # an explicit weights argument.  For "simple" and "approx" we pass phase 1
  # weights so survey can compute the probability-weighted estimates.
  weights_arg <- if (method == "full") NULL else list(p1_weights, NULL)

  survey::twophase(
    id = list(p1_id, p2_id),
    strata = list(p1_strata, p2_strata),
    weights = weights_arg,
    fpc = list(p1_fpc, p2_fpc),
    data = x@data,
    subset = .to_formula(subset_var),
    method = method
  )
}


# ── as_tbl_svy ────────────────────────────────────────────────────────────────

#' Convert a surveycore Design Object to an srvyr tbl_svy
#'
#' Converts a surveycore design object to an `srvyr` `tbl_svy` by first
#' converting to a `survey` design via [as_svydesign()] and then wrapping
#' with `srvyr::as_survey()`. Requires both `survey` and `srvyr`.
#'
#' Metadata (variable labels, value labels) is NOT carried over.
#'
#' @param x A `survey_taylor`, `survey_replicate`, or `survey_twophase` object.
#'   `survey_nonprob` is not supported and will error.
#' @return A `srvyr::tbl_svy` object.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' if (
#'   requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("srvyr", quietly = TRUE)
#' ) {
#'   ts <- as_tbl_svy(d)
#' }
#' @seealso [from_tbl_svy()] to convert back from a `tbl_svy` object
#' @family conversion
#' @export
as_tbl_svy <- function(x) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg survey} must be installed to use {.fn as_tbl_svy}.",
        "v" = "Install it with {.code install.packages(\"survey\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }
  if (!requireNamespace("srvyr", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg srvyr} must be installed to use {.fn as_tbl_svy}.",
        "v" = "Install it with {.code install.packages(\"srvyr\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }

  if (!S7::S7_inherits(x, survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a survey design object.",
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_survey_object"
    )
  }

  srvyr::as_survey(as_svydesign(x))
}


# ── Internal helpers for from_svydesign ───────────────────────────────────────

# Extract variable names from a formula or call expression.
# Returns NULL when f is NULL, when f is ~1 (no variables), or on error.
# Examples:
#   ~wt          → "wt"
#   ~psu + ssu   → c("psu", "ssu")
#   ~1           → NULL
#   NULL         → NULL
#' @noRd
.vars_from_formula <- function(f) {
  if (is.null(f)) {
    return(NULL)
  }
  vars <- tryCatch(all.vars(f), error = function(e) character(0L))
  if (length(vars) == 0L) NULL else vars
}


# Find the first column in data whose numeric values exactly match vals.
# Returns the column name, or NULL if no matching column exists.
# Used to recover weight/subset column names from stored vectors in survey
# design objects that do not preserve the original column name.
#' @noRd
.find_col_by_value <- function(data, vals) {
  if (is.null(vals) || length(vals) == 0L) {
    return(NULL)
  } # nocov — callers always pass non-NULL
  for (nm in names(data)) {
    col <- data[[nm]]
    if (
      (is.numeric(col) || is.logical(col)) &&
        length(col) == length(vals) &&
        isTRUE(all.equal(
          as.numeric(col),
          as.numeric(vals),
          check.attributes = FALSE
        ))
    ) {
      return(nm)
    }
  }
  NULL
}


# ── from_svydesign ────────────────────────────────────────────────────────────

#' Convert a survey Package Design to a surveycore Design Object
#'
#' Converts a `survey` package design object (`svydesign`, `svrepdesign`, or
#' `twophase`) to the corresponding surveycore S7 object. The data, design
#' variables, and replicate weights are preserved; metadata (variable labels,
#' value labels) is not — the `survey` package has no metadata system.
#'
#' Weight column names are recovered from the design call when available. When
#' the call does not contain a formula (e.g., weights were passed as a vector),
#' the weight column is identified by matching the stored weight values against
#' columns in the data. If no match is found, a `..surveycore_wt..` column is
#' added.
#'
#' @param x A `survey::svydesign`, `survey::svrepdesign`, `survey::twophase`,
#'   `survey::twophase2`, or `srvyr::tbl_svy` object. Both `"twophase"` and
#'   `"twophase2"` classes from the `survey` package are dispatched to the
#'   two-phase conversion path.
#' @return A `survey_taylor`, `survey_replicate`, or `survey_twophase` object.
#'
#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   sv <- survey::svydesign(
#'     ids = ~sdmvpsu,
#'     weights = ~wtint2yr,
#'     strata = ~sdmvstra,
#'     data = nhanes_2017,
#'     nest = TRUE
#'   )
#'   d <- from_svydesign(sv)
#'   survey_data(d)
#' }
#' @seealso [as_svydesign()] to convert in the other direction
#' @family conversion
#' @export
from_svydesign <- function(x) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg survey} must be installed to use {.fn from_svydesign}.",
        "v" = "Install it with {.code install.packages(\"survey\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }

  if (inherits(x, "twophase2") || inherits(x, "twophase")) {
    .from_svydesign_twophase(x)
  } else if (inherits(x, "svyrep.design")) {
    .from_svydesign_replicate(x)
  } else if (inherits(x, "survey.design")) {
    .from_svydesign_taylor(x)
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be a {.pkg survey} design object ",
          "({.cls svydesign}, {.cls svyrep.design}, or {.cls twophase})."
        ),
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_survey_design"
    )
  }
}


# svydesign / tbl_svy → survey_taylor
#' @noRd
.from_svydesign_taylor <- function(x) {
  data <- as.data.frame(x$variables)

  # tbl_svy stores its call as srvyr's quoteless_text (not a language object).
  # Use tryCatch so atomic / non-language calls degrade to NULL gracefully.
  .call_vars <- function(nm) {
    tryCatch(.vars_from_formula(x$call[[nm]]), error = function(e) NULL)
  }

  ids_var <- .call_vars("ids")
  strata_var <- .call_vars("strata")
  fpc_raw <- .call_vars("fpc")
  fpc_var <- if (!is.null(fpc_raw)) fpc_raw[[1L]] else NULL
  nest <- tryCatch(isTRUE(x$call$nest), error = function(e) FALSE)

  # Weight column: try the call formula first, then value-matching.
  weights_var <- .call_vars("weights")
  if (is.null(weights_var)) {
    weights_var <- .find_col_by_value(data, 1 / x$prob)
  }
  if (is.null(weights_var)) {
    weights_var <- "..surveycore_wt.."
    data[[weights_var]] <- 1 / x$prob
  }

  variables <- list(
    ids = ids_var,
    weights = weights_var,
    strata = strata_var,
    fpc = fpc_var,
    nest = nest,
    probs_provided = FALSE,
    visible_vars = NULL
  )

  survey_taylor(
    data = data,
    variables = variables,
    metadata = survey_metadata()
  )
}


# svyrep.design → survey_replicate
#' @noRd
.from_svydesign_replicate <- function(x) {
  data <- as.data.frame(x$variables)
  rep_cols <- colnames(x$repweights)

  # Weight column: find by matching pweights to data columns.
  weights_var <- .find_col_by_value(data, x$pweights)
  if (is.null(weights_var)) {
    weights_var <- "..surveycore_wt.."
    data[[weights_var]] <- x$pweights
  }

  variables <- list(
    weights = weights_var,
    repweights = rep_cols,
    type = x$type,
    scale = x$scale,
    rscales = x$rscales,
    mse = isTRUE(x$mse),
    fpc = NULL,
    fpctype = "fraction",
    probs_provided = FALSE,
    visible_vars = NULL
  )

  survey_replicate(
    data = data,
    variables = variables,
    metadata = survey_metadata()
  )
}


# twophase / twophase2 → survey_twophase
#' @noRd
.from_svydesign_twophase <- function(x) {
  # Convert the phase 1 full design to survey_taylor.
  phase1_sc <- .from_svydesign_taylor(x$phase1$full)
  phase1_data <- phase1_sc@data

  # Locate the subset column by matching x$subset to logical columns in data.
  subset_var <- .find_col_by_value(phase1_data, as.numeric(x$subset))
  if (is.null(subset_var)) {
    subset_var <- "..surveycore_subset.."
    phase1_data[[subset_var]] <- x$subset
  }

  # Derive method: check x$method first, fall back to class-based inference.
  # survey::twophase() never stores x$method — the branch below is defensive
  # for survey-compatible packages that may set it explicitly. # nocov start
  method <- if (
    !is.null(x$method) && x$method %in% c("full", "approx", "simple")
  ) {
    x$method # nocov end
  } else if (inherits(x, "twophase2")) {
    "full"
  } else {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Could not determine two-phase variance method ",
          "from the survey object."
        ),
        "i" = 'Defaulting to {.val "approx"}.'
      ),
      class = "surveycore_warning_twophase_method_unknown"
    )
    "approx"
  }

  variables <- list(
    phase1 = phase1_sc@variables,
    phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
    subset = subset_var,
    method = method
  )

  survey_twophase(
    data = phase1_data,
    variables = variables,
    metadata = survey_metadata()
  )
}


# ── from_tbl_svy ──────────────────────────────────────────────────────────────

#' Convert an srvyr tbl_svy to a surveycore Design Object
#'
#' Converts an `srvyr` `tbl_svy` to a surveycore design object by delegating
#' to [from_svydesign()]. A `tbl_svy` IS a `survey.design`, so the conversion
#' is structurally identical. Requires both `survey` and `srvyr`.
#'
#' @param x A `srvyr::tbl_svy` object.
#' @return A `survey_taylor`, `survey_replicate`, or `survey_twophase` object.
#'
#' @examples
#' if (
#'   requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("srvyr", quietly = TRUE)
#' ) {
#'   ts <- srvyr::as_survey(
#'     survey::svydesign(
#'       ids = ~sdmvpsu,
#'       weights = ~wtint2yr,
#'       strata = ~sdmvstra,
#'       data = nhanes_2017,
#'       nest = TRUE
#'     )
#'   )
#'   d <- from_tbl_svy(ts)
#' }
#' @seealso [as_tbl_svy()] to convert in the other direction
#' @family conversion
#' @export
from_tbl_svy <- function(x) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg survey} must be installed to use {.fn from_tbl_svy}.",
        "v" = "Install it with {.code install.packages(\"survey\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }
  if (!requireNamespace("srvyr", quietly = TRUE)) {
    # nocov start
    cli::cli_abort(
      c(
        "x" = "{.pkg srvyr} must be installed to use {.fn from_tbl_svy}.",
        "v" = "Install it with {.code install.packages(\"srvyr\")}."
      ),
      class = "surveycore_error_pkg_not_installed"
    )
    # nocov end
  }

  if (!inherits(x, "tbl_svy")) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a {.cls tbl_svy} object.",
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_tbl_svy"
    )
  }

  # tbl_svy inherits from survey.design — delegate directly.
  from_svydesign(x)
}
