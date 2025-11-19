#' G-computation Analysis for a Single Visit
#'
#' Performs logistic regression and estimates marginal effects for binary outcomes.
#'
#' @param data A data.frame with one visit of data.
#' @param vars A list containing `group`, `outcome`, `covariates`, and `visit`.
#' @param reference_levels Optional vector specifying reference level(s) of the treatment factor.
#' @param var_method Marginal variance estimation method (default: "Ge").
#' @param type Type of robust variance estimator (default: "HC0").
#' @param contrast Type of contrast to compute (default: "diff").
#'
#' @return A named list containing estimates and standard errors for treatment comparisons and within-arm means.
#' @export
#'
gcomp_responder <- function(
  data,
  vars,
  reference_levels = NULL,
  var_method = "Ge",
  type = "HC0",
  contrast = "diff"
) {
  outcome <- vars$outcome
  group <- vars$group
  covariates <- vars$covariates
  visit <- vars$visit

  if (is.null(reference_levels)) {
    reference_levels <- levels(data[[group]])[1]
  }

  frm <- as_simple_formula2(
    outcome,
    setdiff(unique(c(group, extract_covariates2(covariates))), visit)
  )

  model <- stats::glm(frm, data = data, family = binomial)

  marginal_fit <- beeca::get_marginal_effect(
    model,
    trt = group,
    method = var_method,
    type = type,
    contrast = contrast,
    reference = reference_levels
  )

  res <- marginal_fit$marginal_results |>
    dplyr::filter(STAT %in% c("diff", "diff_se", "risk", "risk_se"))

  lsm <- res |>
    dplyr::filter(STAT %in% c("risk", "risk_se")) |>
    dplyr::group_by(TRTVAL) |>
    dplyr::group_map(
      ~ {
        list_name <- paste0("lsm_", .y$TRTVAL)
        named_list <- list(
          est = .x$STATVAL[.x$STAT == "risk"],
          se = .x$STATVAL[.x$STAT == "risk_se"],
          df = NA
        )
        setNames(list(named_list), list_name)
      }
    ) |>
    purrr::flatten()

  trt <- res |>
    dplyr::filter(STAT %in% c("diff", "diff_se")) |>
    dplyr::group_by(TRTVAL) |>
    dplyr::group_map(
      ~ {
        trtval <- sub("diff: ", "", .y$TRTVAL)
        list_name <- paste0("trt_", trtval)
        named_list <- list(
          est = .x$STATVAL[.x$STAT == "diff"],
          se = .x$STATVAL[.x$STAT == "diff_se"],
          df = NA
        )
        setNames(list(named_list), list_name)
      }
    ) |>
    purrr::flatten()

  return(c(trt, lsm))
}


#' G-computation for a Binary Outcome at Multiple Visits
#'
#' Applies `gcomp_responder()` separately for each unique visit in the data.
#'
#' @param data A data.frame containing multiple visits.
#' @param vars A list specifying analysis variables.
#' @param reference_levels Optional reference level for the treatment variable.
#' @param ... Additional arguments passed to `gcomp_responder()`.
#'
#' @return A named list of estimates for each visit and treatment group.
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#'
#' data("ADMI")
#'
#' ADMI <- ADMI |>
#'   mutate(
#'     TRT = factor(TRT, levels = c("Placebo", "Drug A")),
#'     STRATA = factor(STRATA),
#'     REGION = factor(REGION)
#'   )
#'
#' # Note: method must match the original used for imputation
#' method <- method_bayes(
#'   n_samples = 100,
#'   control = control_bayes(warmup = 20, thin = 2)
#' )
#'
#' vars_binary <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CRIT1FLN",
#'   covariates = c("BASE", "STRATA", "REGION")
#' )
#'
#' ana_obj_prop <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars_binary,
#'   method = method,
#'   fun = gcomp_responder_multi,
#'   reference_levels = "Placebo",
#'   contrast = "diff",
#'   var_method = "Ge",
#'   type = "HC0"
#' )
#'
#' pool(ana_obj_prop)
#' }
gcomp_responder_multi <- function(data, vars, reference_levels = NULL, ...) {
  visit_var <- vars$visit
  visits <- unique(data[[visit_var]])

  results <- lapply(visits, function(v) {
    dat <- data[data[[visit_var]] == v, ]
    res <- gcomp_responder(
      data = dat,
      vars = vars,
      reference_levels = reference_levels,
      ...
    )
    stats::setNames(res, paste0(names(res), "_", v))
  })

  return(unlist(results, recursive = FALSE))
}


#' Convert character variable list to formula
#' @param outcome Character string, the outcome variable.
#' @param covars Character vector of covariates.
#' @return A formula object.
#' @keywords internal
as_simple_formula2 <- function(outcome, covars) {
  frm <- stats::as.formula(
    paste0(
      outcome,
      " ~ 1 + ",
      paste0(covars, collapse = " + ")
    )
  )
  environment(frm) <- globalenv()
  frm
}

#' Extract variable names from model terms
#'
#' Takes a character vector including potentially model terms like `*` and `:` and
#' extracts out the individual variables.
#'
#' @param x A character vector of model terms.
#' @return A character vector of unique variable names.
#' @keywords internal
extract_covariates2 <- function(x) {
  if (is.null(x)) return(x)
  x_split <- strsplit(x, ":|\\*")
  unique(trimws(unlist(x_split)))
}
