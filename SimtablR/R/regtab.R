# MULTI-OUTCOME REGRESSION TABLE INTERFACE
# Fits generalized linear and penalized models across outcomes with shared predictors.
# Preserves unrounded numerical coefficients, robust covariance, and diagnostic evidence.

#' Fit one regression model per outcome
#'
#' `regtab()` fits the same set of predictors to one or more outcomes and
#' returns the estimates side by side in a single publication-style table, one
#' column per outcome. The default Poisson-log model with robust standard
#' errors gives rate ratios for counts and prevalence or risk ratios for 0/1
#' outcomes (modified Poisson); use `family = binomial()` for odds ratios or
#' `gaussian()` for mean differences. For crude estimates alongside a
#' descriptive table use [table1()].
#'
#' @param data A data frame.
#' @param outcomes Character vector of outcome column names. Each outcome gets
#'   its own model with the same predictors.
#' @param predictors The right-hand side of the model, as a one-sided formula
#'   (`~ age + sex`) or a string (`"age * sex + smoking"`). Write
#'   transformations such as centring directly in the formula.
#' @param family A [stats::family()] object. Poisson-log needs a numeric
#'   outcome (counts or 0/1); `binomial()` also accepts two-level factors.
#' @param offset Optional person-time column: a bare name or string. With a
#'   Poisson-log model, `offset(log(offset))` is added so estimates become
#'   incidence-rate ratios.
#' @param robust Standard errors: `TRUE` for HC0 robust errors, `FALSE` for
#'   model-based errors, or one of `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`. Prefer
#'   `"HC3"` in small samples (Long & Ervin, 2000).
#' @param method String. `"glm"` fits with [stats::glm()]; `"firth"` fits
#'   Firth penalised logistic regression with the logistf package, for
#'   binomial-logit models only.
#' @param exponentiate Logical. Whether to report exponentiated estimates. If
#'   `NULL`, they are exponentiated for Poisson, binomial, and quasi- families
#'   and left on the original scale for Gaussian models.
#' @param labels Named character vector of display labels for outcomes, e.g.
#'   `c(ed_visits = "ED revisits")`.
#' @param predictor_labels Named character vector of display labels for model
#'   terms, e.g. `c(sexMale = "Male sex")`.
#' @param d Integer. Decimal places for estimates and intervals.
#' @param conf.level Number between 0 and 1. Confidence level for intervals.
#' @param include_intercept Logical. If `TRUE`, show the intercept row.
#' @param p_values Logical. If `TRUE`, add a p-value column for each outcome.
#' @param design String. Optional study design recorded on the result. With a
#'   Poisson-log model and an `offset`, it lets the estimate be labelled as an
#'   incidence-rate ratio.
#'
#' @details
#' ## Statistical methods
#' Each outcome is fitted with [stats::glm()] and the requested family.
#' Intervals are Wald intervals on the link scale, using HC0 sandwich
#' standard errors by default; a Poisson-log model with robust errors on a 0/1
#' outcome is the modified Poisson approach for prevalence and risk ratios
#' (Zou, 2004). `method = "firth"` uses penalised-likelihood (profile)
#' inference instead, so the HC options do not apply.
#'
#' For models with two or more predictor terms, generalized variance
#' inflation factors (Fox & Monette, 1992) are stored on the result. Show them
#' with `as.data.frame(fit, vif = TRUE)` or `generics::glance(fit, vif = TRUE)`.
#'
#' ## Missing data
#' Rows with a missing outcome or predictor are dropped from that outcome's
#' model, so the N can differ between outcomes. It is shown in the table and
#' in [model_info()].
#'
#' ## Modifying the result
#' `coef()`, `confint()`, `vcov()`, `formula()`, and `nobs()` work on the
#' result, returning one entry per outcome or a single one with
#' `outcome = "name"`. [model_info()] reports convergence and failed
#' models, `generics::tidy()` returns one row per term, and
#' `ggplot2::autoplot()` draws a forest plot.
#'
#' ## Limitations
#' The result is a reporting table, not a fitted model: it keeps no fitted
#' values or residuals and cannot predict. For conditional logistic
#' regression, prediction, or model diagnostics, fit [stats::glm()],
#' `logistf::logistf()`, or `survival::clogit()` directly.
#'
#' @return A `simtab_result` of class `simtab_regtab`. Print it to see the
#'   formatted table, convert it with `as.data.frame()`, or save it with
#'   [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded results
#'   are stored in `$data`.
#' @seealso [model_info()] for convergence, [survtab()] for time-to-event
#'   outcomes, [table1()] for crude and adjusted effects in a descriptive
#'   table, and [simtablr_references] for all references cited by SimtablR.
#' @examples
#' # Rate ratios for two count outcomes (Poisson, robust SEs)
#' fit <- regtab(
#'   epitabl,
#'   outcomes = c("ed_visits", "length_of_stay"),
#'   predictors = ~ age + sex + smoking
#' )
#' fit
#'
#' # Odds ratios for a binary outcome, with p-values
#' regtab(
#'   epitabl, "rehospitalized", ~ age + sex + diabetes,
#'   family = binomial(), p_values = TRUE
#' )
#'
#' # One row per term, for further processing
#' generics::tidy(fit)
#' @references Zou, G. (2004). A modified Poisson regression approach to
#'   prospective studies with binary data. \emph{American Journal of
#'   Epidemiology}, 159(7), 702--706. \doi{10.1093/aje/kwh090}.
#'
#'   Firth, D. (1993). Bias reduction of maximum likelihood estimates.
#'   \emph{Biometrika}, 80(1), 27--38. \doi{10.1093/biomet/80.1.27}.
#'
#'   Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics.
#'   \emph{Journal of the American Statistical Association}, 87(417),
#'   178--183. \doi{10.1080/01621459.1992.10475190}.
#'
#'   Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
#'   standard errors in the linear regression model. \emph{The American
#'   Statistician}, 54(3), 217--224. \doi{10.1080/00031305.2000.10474549}.
#' @export
#########
# REGRESSION TABLE CONSTRUCTOR
# Public entry point for generalized linear models and Firth penalized regression.
regtab <- function(
  data,
  outcomes,
  predictors,
  family = poisson(link = "log"),
  offset = NULL,
  robust = TRUE,
  method = "glm",
  exponentiate = NULL,
  labels = NULL,
  predictor_labels = NULL,
  d = 2,
  conf.level = 0.95,
  include_intercept = FALSE,
  p_values = FALSE,
  design = NULL
) {
  spec <- .regtab_spec(
    data = data,
    outcomes = outcomes,
    predictors = predictors,
    family = family,
    offset_quo = rlang::enquo(offset),
    robust = robust,
    method = method,
    exponentiate = exponentiate,
    labels = labels,
    predictor_labels = predictor_labels,
    d = d,
    conf.level = conf.level,
    include_intercept = include_intercept,
    p_values = p_values,
    call = match.call()
  )
  if (!is.null(design)) {
    spec <- set_design(spec, design)
  }
  evaluate(spec)
}

#########
# TABULAR FILE EXPORTERS
# File output helpers for saving regression tables to delimited CSV or Excel workbooks.

#' Export regtab results to CSV
#'
#' @param x A `regtab` result.
#' @param file Output file path. A missing `.csv` suffix is added.
#' @param overwrite Logical. Existing files are protected by default; pass
#'   `TRUE` to replace the destination explicitly.
#' @param ... Passed to [utils::write.csv()].
#' @return Invisibly returns `x`.
#' @details The file is completed in the destination directory before it is
#'   published. Backend failures remove partial output and preserve any existing
#'   destination.
#' @examples
#' \dontrun{
#' mod <- regtab(epitabl, outcomes = "rehospitalized", predictors = ~ age + sex)
#' export_regtab_csv(mod, tempfile(fileext = ".csv"))
#' }
#' @export
export_regtab_csv <- function(x, file, overwrite = FALSE, ...) {
  target <- .prepare_export_path(file, "csv", overwrite = overwrite)
  .write_export_transaction(target, function(temporary) {
    utils::write.csv(
      as.data.frame(x, tidy = FALSE),
      temporary,
      row.names = FALSE,
      ...
    )
  })
  message(sprintf("Table exported to: %s", target$path))
  invisible(x)
}

#' Export regtab results to Excel
#'
#' @param x A `regtab` result.
#' @param file Output file path. A missing `.xlsx` suffix is added.
#' @inheritParams export_regtab_csv
#' @param ... Passed to [export_xlsx()].
#' @return Invisibly returns `x`.
#' @examples
#' \dontrun{
#' mod <- regtab(epitabl, outcomes = "rehospitalized", predictors = ~ age + sex)
#' export_regtab_xlsx(mod, tempfile(fileext = ".xlsx"))
#' }
#' @export
export_regtab_xlsx <- function(x, file, overwrite = FALSE, ...) {
  export_xlsx(x, path = file, overwrite = overwrite, ...)
  invisible(x)
}
