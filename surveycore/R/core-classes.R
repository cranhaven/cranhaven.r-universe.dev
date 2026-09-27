# R/core-classes.R
#
# S7 class definitions for the surveycore package.
#
# Classes defined here (in dependency order):
#   survey_metadata   — metadata container
#   survey_base       — abstract base class for all survey design objects
#   survey_taylor     — Taylor series linearization design
#   survey_replicate  — replicate weights design
#   survey_twophase   — two-phase sampling design
#   survey_nonprob — calibrated/non-probability design (Phase 2.5 skeleton)
#
# Validators implement Layer 1 structural invariants only (per 3-layer
# validator architecture in phase-0-implementation-plan-v2.md). User-input
# validation lives in R/core-constructors.R (Layer 3).
#
# Error classes match plans/error-messages.md exactly.

# ── survey_metadata ───────────────────────────────────────────────────────────

#' Survey Metadata Container
#'
#' Stores variable labels, value labels, question prefaces, notes, and
#' transformation history for variables in a survey design object.
#' Automatically populated from haven-style attributes when
#' [as_survey()] or related constructors are called.
#'
#' @param variable_labels A named list mapping variable names to character
#'   labels (e.g., `list(age = "Age in years")`).
#' @param value_labels A named list mapping variable names to named vectors
#'   of value labels (e.g., `list(sex = c(Male = 1L, Female = 2L))`).
#' @param question_prefaces A named list mapping variable names to shared
#'   question battery preface text.
#' @param notes A named list mapping variable names to analyst notes.
#' @param universe A named list mapping variable names to universe
#'   descriptions (e.g., `list(age = "Adults 18+")`). Describes the
#'   population to which a variable applies.
#' @param missing_codes A named list mapping variable names to atomic
#'   vectors of missing-value codes
#'   (e.g., `list(age = c(Refused = 99L, DK = 98L))`).
#' @param sata A named list mapping variable names to `TRUE` for variables
#'   that are select-all-that-apply (SATA). Only variables explicitly marked
#'   as SATA appear in this list — absence means the variable is not SATA.
#' @param higher_is A named list mapping variable names to `"better"` or
#'   `"worse"`, indicating the direction of improvement for that variable.
#'   Absent keys mean the direction is unset. Use [set_higher_is()] and
#'   [extract_higher_is()] to access this property.
#' @param reverse_coded A named list mapping variable names to `TRUE` for
#'   variables that are reverse-coded. Absent keys mean the variable is not
#'   reverse-coded. Use [set_reverse_coded()] and [extract_reverse_coded()] to
#'   access this property.
#' @param transformations A named list tracking variable transformation
#'   history (populated automatically during operations).
#' @param weighting_history A list recording weighting operations applied to
#'   the survey object (e.g., raking, trimming). Each entry is written by
#'   a \pkg{surveywts} function and contains the operation name,
#'   parameters, effective sample size before/after, and design effect.
#'   Always `list()` until a \pkg{surveywts} weighting function is
#'   applied. Reserved for Phase 2.5.
#'
#' @return A `survey_metadata` object.
#'
#' @examples
#' # Empty metadata (default)
#' m <- survey_metadata()
#' m@variable_labels
#'
#' # Pre-populated metadata
#' m <- survey_metadata(
#'   variable_labels = list(age = "Respondent age", income = "Annual income"),
#'   value_labels = list(sex = c(Male = 1L, Female = 2L))
#' )
#' m@variable_labels$age
#' m@value_labels$sex
#' @family metadata
#' @export
survey_metadata <- S7::new_class(
  "survey_metadata",
  properties = list(
    variable_labels = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    value_labels = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    question_prefaces = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    notes = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    universe = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    missing_codes = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    sata = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    higher_is = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    reverse_coded = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    transformations = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    # RESERVED: Phase 2.5 — populated by surveywts calibration functions.
    # Each entry records one weighting operation (rake, trim_weights, etc.)
    # with parameters, effective n before/after, and design effect.
    # Always list() until surveywts writes to it.
    weighting_history = S7::new_property(
      S7::class_list,
      default = quote(list())
    )
  )
)


# ── survey_base ───────────────────────────────────────────────────────────────

#' Abstract Base Survey Design Class
#'
#' All survey design objects (`survey_taylor`, `survey_replicate`,
#' `survey_twophase`, `survey_nonprob`) inherit from `survey_base`. This
#' class is abstract and cannot be instantiated directly — use
#' [as_survey()], [as_survey_replicate()], [as_survey_twophase()], or
#' [as_survey_nonprob()] instead.
#'
#' @section Properties:
#' \describe{
#'   \item{`data`}{A `data.frame` containing the survey data.}
#'   \item{`metadata`}{A [survey_metadata] object.}
#'   \item{`variables`}{A named list of design specification (varies by
#'     subclass).}
#'   \item{`groups`}{Character vector of active grouping variables.
#'     Set by surveytidy's `group_by()`. Always `character(0)` in
#'     standalone surveycore use.}
#'   \item{`call`}{The language object capturing the construction call,
#'     or `NULL`.}
#' }
#'
#' @return Cannot be instantiated directly. See [survey_taylor],
#'   [survey_replicate], [survey_twophase], or [survey_nonprob] for
#'   concrete subclasses.
#' @keywords internal
#' @export
survey_base <- S7::new_class(
  "survey_base",
  abstract = TRUE,
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      default = quote(data.frame())
    ),
    metadata = S7::new_property(
      class = survey_metadata,
      default = quote(survey_metadata())
    ),
    variables = S7::new_property(
      S7::class_list,
      default = quote(list())
    ),
    # Set by surveytidy's group_by(). Always character(0) in standalone
    # surveycore use. Do NOT read or write in surveycore code.
    groups = S7::new_property(
      S7::class_character,
      default = quote(character(0))
    ),
    call = S7::new_property(default = NULL)
  )
)


# ── survey_taylor ─────────────────────────────────────────────────────────────

#' Taylor Series Linearization Survey Design
#'
#' A survey design object using Taylor series (linearization) for variance
#' estimation. Create with [as_survey()].
#'
#' @param data A `data.frame` containing the survey data. Prefer
#'   [as_survey()] over calling this constructor directly.
#' @param metadata A [survey_metadata] object. Created automatically by
#'   [as_survey()].
#' @param variables A named list of design specification (ids, weights,
#'   strata, fpc, nest, probs_provided). Set automatically by [as_survey()].
#' @param groups Set by surveytidy's `group_by()`. Always `character(0)` in
#'   standalone surveycore use.
#' @param call Language object capturing the construction call.
#' @param calibration A list of calibration data elements produced by
#'   [as_caldata()], or `NULL` (default) for no calibration. When non-`NULL`,
#'   variance estimation routines apply a Deville-Sarndal calibration
#'   correction.
#'
#' @section Design variables (`@variables`):
#' \describe{
#'   \item{`ids`}{Character vector of cluster ID column names, or `NULL`
#'     for simple random sampling.}
#'   \item{`weights`}{Character string naming the weight column.}
#'   \item{`strata`}{Character string naming the strata column, or `NULL`.}
#'   \item{`fpc`}{Character string naming the finite population correction
#'     column, or `NULL`.}
#'   \item{`nest`}{Logical. `TRUE` if cluster IDs are nested within strata
#'     (i.e., the same ID value in two strata refers to two distinct PSUs).}
#'   \item{`probs_provided`}{Logical. `TRUE` if the user supplied `probs`
#'     rather than `weights` to [as_survey()].}
#' }
#'
#' @return A `survey_taylor` object.
#' @usage survey_taylor(
#'   data = data.frame(),
#'   metadata = survey_metadata(),
#'   variables = list(),
#'   groups = character(0),
#'   call = NULL,
#'   calibration = NULL
#' )
#'
#' @examples
#' # Prefer as_survey() over calling survey_taylor() directly
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' class(d)
#' @references
#' Deville, J.-C. and Sarndal, C.-E. (1992) Calibration estimators in survey
#' sampling. \emph{Journal of the American Statistical Association}
#' \bold{87}(418), 376--382.
#'
#' Deville, J.-C., Sarndal, C.-E. and Sautory, O. (1993) Generalized raking
#' procedures in survey sampling. \emph{Journal of the American Statistical
#' Association} \bold{88}(423), 1013--1020.
#'
#' Lumley, T. (2010) \emph{Complex Surveys: A Guide to Analysis Using R}.
#' John Wiley and Sons.
#'
#' Rao, J.N.K., Yung, W. and Hidiroglou, M.A. (2002) Estimating equations for
#' the analysis of survey data using poststratification information.
#' \emph{Sankhya} \bold{64-A}, 22--36.
#'
#' Sarndal, C-E., Swensson, B. and Wretman, J. (1992)
#' \emph{Model Assisted Survey Sampling}. Springer.
#'
#' @seealso [as_survey()] to create a `survey_taylor` object.
#' @family constructors
#' @export
survey_taylor <- S7::new_class(
  "survey_taylor",
  parent = survey_base,
  properties = list(
    calibration = S7::new_property(default = NULL)
  ),
  validator = function(self) {
    # ── Gather all named design column names ─────────────────────────────────
    # c() with NULL args drops them automatically
    design_vars <- c(
      self@variables$ids,
      self@variables$weights,
      self@variables$strata,
      self@variables$fpc
    )

    # ── Error 31: design vars must exist in @data ────────────────────────────
    missing_vars <- setdiff(design_vars, names(self@data))
    if (length(missing_vars) > 0L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Design variable(s) not found in {.arg data}: ",
            "{.field {missing_vars}}"
          )
        ),
        class = "surveycore_error_design_var_missing"
      )
    }

    # ── Error 34: design vars must be atomic (not list-columns) ─────────────
    for (v in design_vars) {
      if (!is.atomic(self@data[[v]])) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Design variable {.field {v}} is a list-column. ",
              "Design variables must be atomic vectors."
            )
          ),
          class = "surveycore_error_design_var_list"
        )
      }
    }

    # ── Validate weight column ────────────────────────────────────────────────
    # The existence check above guarantees weights_var is in @data if non-NULL.
    weights_var <- self@variables$weights
    if (!is.null(weights_var)) {
      wt_col <- self@data[[weights_var]]

      # Error 32: weights must be numeric
      if (!is.numeric(wt_col)) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} must be numeric, ",
              "not {.cls {class(wt_col)}}"
            )
          ),
          class = "surveycore_error_weights_not_numeric"
        )
      }

      # Error 33: all non-NA weights must be strictly positive
      n_bad <- sum(!is.na(wt_col) & wt_col <= 0)
      if (n_bad > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} has {n_bad} ",
              "non-positive value(s). All non-NA weights must be > 0."
            )
          ),
          class = "surveycore_error_weights_nonpositive"
        )
      }
    }

    # ── Warning 35: PSU appearing in multiple strata ─────────────────────────
    # Only check when ids, strata are both present and nest = FALSE.
    ids_vars <- self@variables$ids
    strata_var <- self@variables$strata
    nest <- isTRUE(self@variables$nest)

    if (
      length(ids_vars) > 0L &&
        !is.null(strata_var) &&
        strata_var %in% names(self@data) &&
        !nest
    ) {
      psu_col <- as.character(self@data[[ids_vars[[1L]]]])
      strata_col <- as.character(self@data[[strata_var]])
      psu_n_strata <- tapply(
        strata_col,
        psu_col,
        function(s) length(unique(s))
      )
      multi_strata_psus <- names(psu_n_strata)[psu_n_strata > 1L]
      if (length(multi_strata_psus) > 0L) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "Some PSUs appear in more than one stratum: ",
              "{.val {head(multi_strata_psus, 5L)}}. ",
              "If PSUs are nested within strata, set {.code nest = TRUE}."
            )
          ),
          class = "surveycore_warning_psu_multi_strata"
        )
      }
    }

    NULL
  }
)


# ── survey_replicate ──────────────────────────────────────────────────────────

#' Replicate Weights Survey Design
#'
#' A survey design object using replicate weights for variance estimation.
#' Create with [as_survey_replicate()].
#'
#' @param data A `data.frame` containing the survey data. Prefer
#'   [as_survey_replicate()] over calling this constructor directly.
#' @param metadata A [survey_metadata] object. Created automatically by
#'   [as_survey_replicate()].
#' @param variables A named list of design specification (weights,
#'   repweights, type, scale, rscales, fpc, fpctype, mse). Set
#'   automatically by [as_survey_replicate()].
#' @param groups Set by surveytidy's `group_by()`. Always `character(0)` in
#'   standalone surveycore use.
#' @param call Language object capturing the construction call.
#' @param calibration A list of calibration data elements produced by
#'   [as_caldata()], or `NULL` (default) for no calibration. When non-`NULL`,
#'   variance estimation routines apply a Deville-Sarndal calibration
#'   correction.
#'
#' @section Design variables (`@variables`):
#' \describe{
#'   \item{`weights`}{Character string naming the weight column.}
#'   \item{`repweights`}{Character vector of replicate weight column names.
#'     The replicate weight matrix is computed on demand from
#'     `design@data[, design@variables$repweights]` — it is not stored as
#'     a property.}
#'   \item{`type`}{Replicate weight method: one of `"JK1"`, `"JK2"`,
#'     `"JKn"`, `"BRR"`, `"Fay"`, `"bootstrap"`, `"ACS"`,
#'     `"successive-difference"`, or `"other"`.}
#'   \item{`scale`}{Numeric scaling factor for variance estimation.}
#'   \item{`rscales`}{Numeric vector of replicate-specific scales, or
#'     `NULL`.}
#'   \item{`fpc`}{FPC column name or `NULL`.}
#'   \item{`fpctype`}{`"fraction"` or `"correction"`.}
#'   \item{`mse`}{Logical. Use MSE estimates?}
#' }
#'
#' @return A `survey_replicate` object.
#' @usage survey_replicate(
#'   data = data.frame(),
#'   metadata = survey_metadata(),
#'   variables = list(),
#'   groups = character(0),
#'   call = NULL,
#'   calibration = NULL
#' )
#'
#' @examples
#' # Prefer as_survey_replicate() over calling survey_replicate() directly
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(20),
#'   wt = runif(20, 1, 3),
#'   rep1 = runif(20, 0.5, 2),
#'   rep2 = runif(20, 0.5, 2)
#' )
#' d <- as_survey_replicate(
#'   df,
#'   weights = wt,
#'   repweights = starts_with("rep"),
#'   type = "BRR"
#' )
#' class(d)
#' @references
#' Canty, A.J. and Davison, A.C. (1999) Resampling-based variance estimation
#' for labour force surveys. \emph{The Statistician} \bold{48}(3), 379--391.
#'
#' Deville, J.-C. and Sarndal, C.-E. (1992) Calibration estimators in survey
#' sampling. \emph{Journal of the American Statistical Association}
#' \bold{87}(418), 376--382.
#'
#' Deville, J.-C., Sarndal, C.-E. and Sautory, O. (1993) Generalized raking
#' procedures in survey sampling. \emph{Journal of the American Statistical
#' Association} \bold{88}(423), 1013--1020.
#'
#' Judkins, D.R. (1990) Fay's method for variance estimation.
#' \emph{Journal of the American Statistical Association}
#' \bold{85}(410), 895--904.
#'
#' Rao, J.N.K., Wu, C.F.J. and Yue, K. (1992) Some recent work on resampling
#' methods for complex surveys. \emph{Survey Methodology} \bold{18}(2),
#' 209--217.
#'
#' Shao, J. and Tu, D. (1995) \emph{The Jackknife and Bootstrap}. Springer.
#'
#' @seealso [as_survey_replicate()] to create a `survey_replicate` object.
#' @family constructors
#' @export
survey_replicate <- S7::new_class(
  "survey_replicate",
  parent = survey_base,
  properties = list(
    calibration = S7::new_property(default = NULL)
  ),
  validator = function(self) {
    weights_var <- self@variables$weights
    repweights_vars <- self@variables$repweights
    all_design_vars <- c(weights_var, repweights_vars)

    # ── All design columns must exist in @data ───────────────────────────────
    missing_vars <- setdiff(all_design_vars, names(self@data))
    if (length(missing_vars) > 0L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Design variable(s) not found in {.arg data}: ",
            "{.field {missing_vars}}"
          )
        ),
        class = "surveycore_error_design_var_missing"
      )
    }

    # ── Design columns must be atomic (not list-columns) ─────────────────────
    for (v in all_design_vars) {
      if (!is.atomic(self@data[[v]])) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Design variable {.field {v}} is a list-column. ",
              "Design variables must be atomic vectors."
            )
          ),
          class = "surveycore_error_design_var_list"
        )
      }
    }

    # ── Validate weight column ────────────────────────────────────────────────
    # The existence check above guarantees weights_var is in @data if non-NULL.
    if (!is.null(weights_var)) {
      wt_col <- self@data[[weights_var]]

      if (!is.numeric(wt_col)) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} must be numeric, ",
              "not {.cls {class(wt_col)}}"
            )
          ),
          class = "surveycore_error_weights_not_numeric"
        )
      }

      n_bad <- sum(!is.na(wt_col) & wt_col <= 0)
      if (n_bad > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} has {n_bad} ",
              "non-positive value(s). All non-NA weights must be > 0."
            )
          ),
          class = "surveycore_error_weights_nonpositive"
        )
      }
    }

    # ── Replicate weight columns must be numeric ──────────────────────────────
    for (rw in repweights_vars) {
      if (rw %in% names(self@data)) {
        rw_col <- self@data[[rw]]
        if (!is.numeric(rw_col)) {
          cli::cli_abort(
            c(
              "x" = paste0(
                "Replicate weight column {.field {rw}} must be ",
                "numeric, not {.cls {class(rw_col)}}"
              )
            ),
            class = "surveycore_error_repweights_not_numeric"
          )
        }
      }
    }

    NULL
  }
)


# ── survey_twophase ───────────────────────────────────────────────────────────

#' Two-Phase Survey Design
#'
#' A survey design object for two-phase (double) sampling. Create with
#' [as_survey_twophase()].
#'
#' @param data A `data.frame` containing the survey data (all Phase 1 rows,
#'   with a logical indicator for Phase 2 membership). Prefer
#'   [as_survey_twophase()] over calling this constructor directly.
#' @param metadata A [survey_metadata] object. Inherited from the Phase 1
#'   design when using [as_survey_twophase()].
#' @param variables A named list of design specification (phase1, phase2,
#'   subset, method). Set automatically by [as_survey_twophase()].
#' @param groups Set by surveytidy's `group_by()`. Always `character(0)` in
#'   standalone surveycore use.
#' @param call Language object capturing the construction call.
#'
#' @section Design variables (`@variables`):
#' \describe{
#'   \item{`phase1`}{Named list containing the Phase 1 design specification
#'     (from a `survey_taylor` object's `@variables`).}
#'   \item{`phase2`}{Named list with optional Phase 2 design columns:
#'     `ids`, `strata`, `probs`, `fpc` — each `NULL` or a character vector
#'     of column names.}
#'   \item{`subset`}{Character string naming the logical column that
#'     indicates Phase 2 membership (`TRUE` = selected into Phase 2).}
#'   \item{`method`}{`"full"`, `"approx"`, or `"simple"`.}
#' }
#'
#' @return A `survey_twophase` object.
#' @usage survey_twophase(
#'   data = data.frame(),
#'   metadata = survey_metadata(),
#'   variables = list(),
#'   groups = character(0),
#'   call = NULL
#' )
#'
#' @examples
#' # Prefer as_survey_twophase() over calling survey_twophase() directly
#' set.seed(1)
#' df <- data.frame(
#'   id = 1:100,
#'   y = rnorm(100),
#'   x = rnorm(100),
#'   wt = runif(100, 1, 3),
#'   in_phase2 = c(rep(TRUE, 40), rep(FALSE, 60))
#' )
#' phase1 <- as_survey(df, weights = wt)
#' d <- as_survey_twophase(phase1, subset = in_phase2)
#' class(d)
#' @seealso [as_survey_twophase()] to create a `survey_twophase` object.
#' @family constructors
#' @export
survey_twophase <- S7::new_class(
  "survey_twophase",
  parent = survey_base,
  properties = list(),
  validator = function(self) {
    subset_var <- self@variables$subset
    phase2_vars <- self@variables$phase2

    # ── Subset column must exist in @data ────────────────────────────────────
    if (!is.null(subset_var) && !subset_var %in% names(self@data)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Subset column {.field {subset_var}} not found in ",
            "{.arg data}"
          )
        ),
        class = "surveycore_error_design_var_missing"
      )
    }

    # ── Subset column must be logical ────────────────────────────────────────
    if (
      !is.null(subset_var) &&
        subset_var %in% names(self@data) &&
        !is.logical(self@data[[subset_var]])
    ) {
      col_class <- class(self@data[[subset_var]])
      cli::cli_abort(
        c(
          "x" = paste0(
            "Subset column {.field {subset_var}} must be logical, ",
            "not {.cls {col_class}}"
          )
        ),
        class = "surveycore_error_subset_not_logical"
      )
    }

    # ── Phase 2 design columns must exist in @data ───────────────────────────
    if (!is.null(phase2_vars)) {
      non_null_p2 <- phase2_vars[!vapply(phase2_vars, is.null, logical(1L))]
      phase2_cols <- unlist(non_null_p2, use.names = FALSE)
      missing_p2 <- setdiff(phase2_cols, names(self@data))
      if (length(missing_p2) > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Phase 2 design variable(s) not found in {.arg data}: ",
              "{.field {missing_p2}}"
            )
          ),
          class = "surveycore_error_design_var_missing"
        )
      }
    }

    # ── Warning 26: phase 2 design var all-NA within phase 2 subset ──────────
    if (
      !is.null(subset_var) &&
        subset_var %in% names(self@data) &&
        !is.null(phase2_vars)
    ) {
      subset_col <- self@data[[subset_var]]
      phase2_rows <- which(!is.na(subset_col) & subset_col)

      if (length(phase2_rows) > 0L) {
        non_null_p2 <- phase2_vars[!vapply(phase2_vars, is.null, logical(1L))]
        phase2_cols <- unlist(non_null_p2, use.names = FALSE)
        for (v in phase2_cols) {
          if (v %in% names(self@data)) {
            col_p2 <- self@data[[v]][phase2_rows]
            if (length(col_p2) > 0L && all(is.na(col_p2))) {
              cli::cli_warn(
                c(
                  "!" = paste0(
                    "Phase 2 design variable {.field {v}} is all NA ",
                    "within the Phase 2 subset. Check rows where ",
                    "{.field {subset_var}} is TRUE."
                  )
                ),
                class = "surveycore_warning_phase2_all_na"
              )
            }
          }
        }
      }
    }

    NULL
  }
)


# ── survey_collection ─────────────────────────────────────────────────────────

#' Multi-Survey Container
#'
#' An S7 container that holds multiple independent `survey_base` objects
#' (e.g., multiple waves of a panel or cross-sectional series) for
#' comparative analysis. Create with [as_survey_collection()].
#'
#' @details
#' `survey_collection` deliberately does **not** inherit from
#' [survey_base]. This prevents collection-of-collections nesting: a
#' `survey_collection` passed as an element of another collection fails
#' the element-type check automatically.
#'
#' Each element of `@surveys` is an independent `survey_base` subclass
#' object (e.g., `survey_taylor`, `survey_replicate`, `survey_twophase`,
#' `survey_nonprob`). Mixed-type collections are allowed — the collection
#' never combines designs, so heterogeneous classes cannot produce an
#' invalid state.
#'
#' @section Properties:
#' \describe{
#'   \item{`surveys`}{A fully named list of `survey_base` objects.
#'     Length \eqn{\geq 1}. Names are unique, non-`NA`, and non-empty.}
#'   \item{`groups`}{A character vector of grouping variable names
#'     applied uniformly across every member survey. Default
#'     `character(0)` (ungrouped). When non-empty, every member's
#'     `@groups` is asserted `identical()` to this value.}
#'   \item{`id`}{Character(1). Identifier column name injected by
#'     `.dispatch_over_collection()` when a `get_*()` is called on the
#'     collection. Default `".survey"`. Stored on the collection and
#'     consumed as the per-call default; a non-`NULL` `.id` at the
#'     analysis-function call site overrides this stored value.
#'     Mutate via [set_collection_id()].}
#'   \item{`if_missing_var`}{Character(1), one of `c("error", "skip")`.
#'     Default `"error"`. Controls how dispatched `get_*()` functions
#'     behave when a member is missing a requested variable. Stored on
#'     the collection and consumed as the per-call default; a non-`NULL`
#'     `.if_missing_var` at the analysis-function call site overrides
#'     this stored value. Mutate via [set_collection_if_missing_var()].}
#' }
#'
#' @param surveys A named list of `survey_base` objects.
#' @param groups Character vector of grouping variable names. Every member's
#'   `@groups` must be `identical()` to this value. Default `character(0)`.
#' @param id Character(1). Identifier column name used when dispatching
#'   analysis functions across the collection. Default `".survey"`.
#' @param if_missing_var Character(1), one of `c("error", "skip")`. Default
#'   `"error"`. Controls how dispatched `get_*()` functions behave when a
#'   member survey is missing a requested variable.
#'
#' @return A `survey_collection` object.
#' @usage survey_collection(
#'   surveys = list(),
#'   groups = character(0),
#'   id = ".survey",
#'   if_missing_var = "error"
#' )
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' coll <- survey_collection(surveys = list(gss = d1))
#' length(coll)
#' names(coll)
#' @seealso [as_survey_collection()] to build a collection from survey
#'   objects; [add_survey()] / [remove_survey()] to mutate an existing
#'   collection.
#' @family collections
#' @export
survey_collection <- S7::new_class(
  "survey_collection",
  properties = list(
    surveys = S7::class_list,
    groups = S7::new_property(
      S7::class_character,
      default = quote(character(0))
    ),
    id = S7::new_property(
      S7::class_character,
      default = ".survey"
    ),
    if_missing_var = S7::new_property(
      S7::class_character,
      default = "error"
    )
  ),
  validator = function(self) {
    # C13 — @id well-formed (length-1, character, non-NA, non-empty).
    # Run before C1/C2/C4/G1 so the most descriptive error fires first
    # when multiple invariants would otherwise trigger.
    .validate_collection_id(self@id, "id")

    # C15 — @if_missing_var well-formed (length-1, character, non-NA,
    # in c("error", "skip")). Run after @id so the @id error has
    # precedence when both are simultaneously invalid.
    .validate_collection_if_missing_var(self@if_missing_var, "if_missing_var")

    # C1 — empty list
    if (length(self@surveys) == 0L) {
      cli::cli_abort(
        c("x" = "Collection must contain at least one survey."),
        class = "surveycore_error_collection_empty"
      )
    }

    # C1 — missing / empty / NA names
    nms <- names(self@surveys)
    if (is.null(nms) || any(nms == "") || any(is.na(nms))) {
      cli::cli_abort(
        c("x" = "All surveys in the collection must be named."),
        class = "surveycore_error_collection_unnamed"
      )
    }

    # C2 — duplicate names (backstop; constructors repair)
    if (anyDuplicated(nms) > 0L) {
      dups <- unique(nms[duplicated(nms)])
      cli::cli_abort(
        c(
          "x" = "Collection names must be unique.",
          "i" = "Duplicate name{?s}: {.val {dups}}."
        ),
        class = "surveycore_error_collection_duplicate_name"
      )
    }

    # C4 — every element must inherit survey_base
    not_surveys <- !vapply(
      self@surveys,
      function(x) S7::S7_inherits(x, survey_base),
      logical(1L)
    )
    if (any(not_surveys)) {
      bad <- nms[not_surveys]
      cli::cli_abort(
        c(
          "x" = "All elements must inherit from {.cls survey_base}.",
          "i" = "Bad element{?s}: {.val {bad}}."
        ),
        class = "surveycore_error_collection_bad_element"
      )
    }

    # G1c — @groups well-formed (no NA, no "", no duplicates). Checked
    # before G1 / G1b so a malformed target produces a single clear error
    # rather than cascading through per-member checks.
    grps <- self@groups
    n_na <- sum(is.na(grps))
    n_empty <- sum(!is.na(grps) & grps == "")
    dup_idx <- duplicated(grps) & !is.na(grps)
    duplicated_names <- unique(grps[dup_idx])
    if (n_na > 0L || n_empty > 0L || length(duplicated_names) > 0L) {
      bullets <- c(
        "x" = paste0(
          "{.arg groups} must be a character vector of column names with ",
          "no {.val NA}, empty strings, or duplicates."
        )
      )
      if (n_na > 0L) {
        bullets <- c(
          bullets,
          "i" = "Contains {.val {n_na}} {.val NA} value(s)."
        )
      }
      if (n_empty > 0L) {
        bullets <- c(
          bullets,
          "i" = "Contains {.val {n_empty}} empty string(s)."
        )
      }
      if (length(duplicated_names) > 0L) {
        bullets <- c(
          bullets,
          "i" = "Duplicates: {.val {duplicated_names}}."
        )
      }
      cli::cli_abort(
        bullets,
        class = "surveycore_error_collection_groups_malformed"
      )
    }

    # G1 — every member's @groups must be identical() to self@groups.
    # Loop with early exit on first mismatch so the error names the first
    # divergent member (by name).
    for (idx in seq_along(self@surveys)) {
      member <- self@surveys[[idx]]
      member_name <- nms[[idx]]
      .check_groups_match(
        candidate_groups = member@groups,
        target_groups = self@groups,
        error_class = "surveycore_error_collection_groups_invariant",
        context = member_name
      )
    }

    # G1b — every column in self@groups must exist in every member's @data.
    # Only runs when @groups is non-empty (otherwise vacuously satisfied).
    if (length(self@groups) > 0L) {
      for (idx in seq_along(self@surveys)) {
        member <- self@surveys[[idx]]
        member_name <- nms[[idx]]
        data_cols <- names(member@data)
        missing_cols <- setdiff(self@groups, data_cols)
        if (length(missing_cols) > 0L) {
          first_missing <- missing_cols[[1L]]
          cli::cli_abort(
            c(
              "x" = paste0(
                "Member {.val {member_name}} has @groups ",
                "{.val {self@groups}} but column {.field {first_missing}} ",
                "is not in its @data."
              ),
              "i" = "Columns available: {.val {data_cols}}."
            ),
            class = "surveycore_error_collection_group_not_in_member_data"
          )
        }
      }
    }

    NULL
  }
)


# ── survey_nonprob ──────────────────────────────────────────────────────────

#' Non-probability Samples
#'
#' A survey design object for non-probability samples (e.g., online panels,
#' quota samples, volunteer panels) with calibration weights (including raking
#' and post-stratification) or inverse probability weighting (IPW)
#' pseudo-weights. Create with
#' [as_survey_nonprob()].
#'
#' @section Variance estimation:
#' Two modes are available, selected by whether `@variables$repweights` is
#' `NULL`:
#' \describe{
#'   \item{**SRS approximation** (no replicate weights)}{Standard errors treat
#'     the calibrated weights as fixed and assume simple random sampling. This
#'     understates calibration uncertainty and should only be used when replicate
#'     weights are unavailable.}
#'   \item{**Replicate variance** (repweights supplied)}{Bootstrap or jackknife
#'     replicate weights propagate calibration uncertainty into the variance
#'     estimate. Each replicate column must contain calibrated weights
#'     re-estimated on one replicate draw. This is the recommended approach.}
#' }
#' See [as_survey_nonprob()] for the full parameter interface, including
#' `type`, `scale`, `rscales`, and `mse`.
#'
#' @section Non-probability samples:
#' Unlike [as_survey()], [as_survey_replicate()], and [as_survey_twophase()],
#' this class does **not** assume a probability sampling design. When no
#' replicate weights are supplied, standard errors rest on a model-assisted SRS
#' assumption, which is consistent with common practice for calibrated
#' non-probability samples (e.g., raked online panels). When replicate weights
#' are supplied, bootstrap or jackknife variance is used instead. See
#' `vignette("creating-survey-objects")` for guidance on choosing between these
#' modes and the limitations of each.
#'
#' @param data A `data.frame` containing the survey data. Prefer
#'   [as_survey_nonprob()] over calling this constructor directly.
#' @param metadata A [survey_metadata] object. Created automatically by
#'   [as_survey_nonprob()].
#' @param variables A named list of design specification (`weights`,
#'   `probs_provided`). Set automatically by [as_survey_nonprob()].
#' @param calibration The calibration provenance object returned by a
#'   \pkg{surveywts} calibration function (e.g., `surveywts::rake()`),
#'   or `NULL` if calibration was performed externally. Stores the
#'   calibration targets, variables, and trimming parameters for
#'   reproducibility and future bootstrap re-calibration. Default `NULL`.
#' @param reference_sample Optional [survey_taylor] object representing the
#'   probability-based reference sample used to estimate propensity scores or
#'   calibration targets. Stored for reproducibility. Default `NULL`.
#' @param groups Set by surveytidy's `group_by()`. Always `character(0)` in
#'   standalone surveycore use.
#' @param call Language object capturing the construction call.
#'
#' @section Design variables (`@variables`):
#' \describe{
#'   \item{`weights`}{Character string naming the (calibrated) weight column.}
#'   \item{`repweights`}{Character vector of bootstrap replicate weight column
#'     names, or `NULL` when no replicate weights are present.}
#'   \item{`type`}{Replicate type. Only `"bootstrap"` is supported for
#'     non-probability samples (`"JK1"`, `"JK2"`, and `"JKn"` are not
#'     accepted); or `NULL` when no replicate weights are present.}
#'   \item{`scale`}{Numeric scale factor for the variance formula, or `NULL`.}
#'   \item{`rscales`}{Per-replicate scale factors, or `NULL`.}
#'   \item{`mse`}{Logical. `TRUE` for MSE form of variance, or `NULL`.}
#'   \item{`probs_provided`}{Always `FALSE` for calibrated designs.}
#' }
#'
#' @section Calibration provenance (`@calibration`):
#' When calibration is performed via \pkg{surveywts}, the returned calibration
#' object is stored here. It contains the calibration targets, variables used,
#' trimming cap, effective sample size before and after, and design effect.
#' `NULL` when calibration was performed externally (e.g., via `anesrake`).
#'
#' @return A `survey_nonprob` object.
#' @usage survey_nonprob(
#'   data = data.frame(),
#'   metadata = survey_metadata(),
#'   variables = list(),
#'   groups = character(0),
#'   call = NULL,
#'   calibration = NULL,
#'   reference_sample = NULL
#' )
#' @seealso [as_survey_nonprob()] to create a `survey_nonprob` object.
#' @family constructors
#' @export
survey_nonprob <- S7::new_class(
  "survey_nonprob",
  parent = survey_base,
  properties = list(
    # Stores calibration provenance from surveywts output.
    # NULL when calibration was done externally.
    # RESERVED for full population in Phase 2.5.
    calibration = S7::new_property(default = NULL),
    # Optional survey_taylor reference sample used for propensity estimation.
    # NULL when not supplied. Type-checked in as_survey_nonprob() (Layer 3).
    reference_sample = S7::new_property(default = NULL)
  ),
  validator = function(self) {
    weights_var <- self@variables$weights

    # ── Weight column must exist in @data ─────────────────────────────────────
    if (!is.null(weights_var) && !weights_var %in% names(self@data)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Weight column {.field {weights_var}} not found in {.arg data}."
          ),
          "i" = paste0(
            "This is an internal consistency error in the ",
            "{.cls survey_nonprob} object."
          ),
          "v" = paste0(
            "Use {.fn as_survey_nonprob} instead of calling the ",
            "constructor directly."
          )
        ),
        class = "surveycore_error_design_var_missing"
      )
    }

    # ── Weight column must be numeric and have at least one valid value ───────
    if (!is.null(weights_var) && weights_var %in% names(self@data)) {
      wt_col <- self@data[[weights_var]]

      if (!is.numeric(wt_col)) {
        cli::cli_abort(
          c(
            "x" = "Weight column {.field {weights_var}} must be numeric.",
            "i" = "Got {.cls {class(wt_col)}}.",
            "v" = paste0(
              "Convert with {.code as.numeric({.field {weights_var}})} ",
              "before calling {.fn as_survey_nonprob}."
            )
          ),
          class = "surveycore_error_weights_not_numeric"
        )
      }

      non_na <- wt_col[!is.na(wt_col)]
      if (length(non_na) == 0L) {
        cli::cli_abort(
          c(
            "x" = "Weight column {.field {weights_var}} has no non-NA values.",
            "i" = paste0(
              "All weights are {.val NA} \u2014 no valid weights ",
              "for estimation."
            ),
            "v" = paste0(
              "Check {.field {weights_var}} for missing data before ",
              "calling {.fn as_survey_nonprob}."
            )
          ),
          class = "surveycore_error_weights_all_zero"
        )
      }

      # Condition 4a — reject negative weights
      n_neg <- sum(non_na < 0)
      if (n_neg > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} has ",
              "{n_neg} negative value(s)."
            ),
            "i" = "All non-NA weights must be non-negative (>= 0).",
            "v" = paste0(
              "Remove or replace rows where ",
              "{.field {weights_var}} is negative."
            )
          ),
          class = "surveycore_error_weights_negative"
        )
      }

      # Condition 4b — reject all-zero weights (no positive values)
      if (!any(non_na > 0)) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Weight column {.field {weights_var}} has no positive values."
            ),
            "i" = "At least one non-NA weight must be greater than 0.",
            "v" = paste0(
              "Check that {.field {weights_var}} contains valid ",
              "survey weights."
            )
          ),
          class = "surveycore_error_weights_all_zero"
        )
      }
    }

    NULL
  }
)
