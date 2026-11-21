#' Summarizing OPSR Model Fits
#'
#' Follows the convention that [`opsr`] does the bare minimum model fitting and
#' inference is performed in `summary`.
#'
#' @param object an object of class `"opsr"`.
#' @param rob if `TRUE`, the [`sandwich::sandwich`] covariance matrix extimator is used.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of class `"summary.opsr"`.
#' In particular the elements `GOF`, `GOFcomponents` and `wald` require further
#' explanation:
#' \item{GOF}{Contains the conventional \emph{goodness of fit} indicators for the full
#'   model. `LL2step` is the log-likelihood of the Heckman two-step solution (if
#'   the default starting values were used). `LLfinal` is the log-likelihood at
#'   final convergence and `AIC`, `BIC` the corresponding information critereon.}
#' \item{GOFcomponents}{Contains the \emph{goodness of fit}  for the model components.
#'   `LLprobit` is the log-likelihood (LL) contribution of the ordered probit model.
#'   `LLprobitEl` the LL of the "equally likely" and `LLprobitMs` the LL of the
#'   "market share" model. With these three metrics the pseudo R2 is computed and
#'   returned as `pseudoR2el` and `pseudoR2ms`. `R2` reports the usual coefficient
#'   of determination (for the continuous outcomes jointly and for each regime
#'   separately).}
#' \item{wald}{Contains the results of two \emph{Wald-tests} as conducted with help
#'   of [`car::linearHypothesis`]. The two H0 hypothesis are 1. All coefficients
#'   of the explanatory variables are 0 and 2. The rho parameters (capturing error
#'   correlation) are zero.}
#'
#' @method summary opsr
#'
#' @export
summary.opsr <- function(object, rob = TRUE, ...) {
  model <- object
  varcov <- if (rob) sandwich::sandwich(model) else stats::vcov(model)

  ## LL
  LL2step <- sum(model$loglik(model$start))
  LLfinal <- model$maximum

  ## R2
  ## selection
  LLprobit <- ll_probit(model)
  LLprobitEl <- stats::nobs(model) * log(1 / model$nReg)
  ms <- model$nObs[-1] / stats::nobs(model)
  LLprobitMs <- as.numeric(model$nObs[-1] %*% log(ms))
  pseudoR2el <- 1 - LLprobit / LLprobitEl  # equally likely
  pseudoR2ms <- 1 - LLprobit / LLprobitMs  # market share

  ## outcome
  R2 <- r2(model)

  ## wald test
  wald_test <- function(model, hypothesis, varcov) {
    if (is_opsr_null(model)) {  # not meaningful for null model
      return(list(df = NA_real_, chisq = NA_real_, pval = NA_real_))
    }
    wt <- car::linearHypothesis(model, hypothesis, vcov. = varcov)
    out <- list()
    out$df <- wt[["Df"]][2]
    out$chisq <- wt[["Chisq"]][2]
    out$pval <- wt[["Pr(>Chisq)"]][2]
    out
  }

  ## independent equations (rho = 0)
  nm <- names(coef(model))
  pattern <- "^rho"
  h_rho <- nm[grepl(pattern, nm)]
  wald_test_rho <- wald_test(model, h_rho, varcov)

  ## constants only model
  pattern <- "^kappa|^sigma|^rho|(Intercept)"
  h_null <- nm[!grepl(pattern, nm)]
  if (any(model$fixed) && !is_opsr_null(model)) {  # issue 10
    fixed <- names(model$fixed)[model$fixed]
    h_null <- h_null[!(h_null %in% fixed)]
  }
  wald_test_null <- wald_test(model, h_null, varcov)

  coef_inf <- function(model, varcov) {
    varcov[, model$fixed] <- NA
    varcov[model$fixed, ] <- NA
    se <- sqrt(diag(varcov))
    se[model$fixed] <- NA
    trat_0 <- model$estimate / se
    trat_1 <- (model$estimate - 1) / se
    pval_0 <- 2 * stats::pnorm(-abs(trat_0))
    pval_1 <- 2 * stats::pnorm(-abs(trat_1))
    out <- list()
    out$varcov <- varcov
    out$se <- se
    out$trat_0 <- trat_0
    out$trat_1 <- trat_1
    out$pval_0 <- pval_0
    out$pval_1 <- pval_1
    out
  }

  cofi <- coef_inf(model, varcov)

  coef_table <- data.frame(
    est = model$estimate,
    se = cofi$se,
    tval = cofi$trat_0,
    pval = cofi$pval_0
  )
  colnames(coef_table) <- c("Estimate", "Std. error", "t value", "Pr(> t)")

  ms <- list()
  ms$call <- model$call
  ms$formula <- model$formula
  ms$robust <- rob
  ms$runtime <- model$runtime
  ms$maxim_type <- model$type
  ms$iterations <- model$iterations
  ms$return_code <- model$code
  ms$message <- model$message
  ms$coef_table <- coef_table
  ms$varcov <- cofi$varcov
  ms$nReg <- model$nReg
  ms$nObs <- model$nObs
  ms$nParams <- model$nParams
  ms$df <- model$df

  ms$GOF <- list(
    LL2step = LL2step,
    LLfinal = LLfinal,
    AIC = AIC(model),
    BIC = BIC(model)
  )

  ms$GOFcomponents <- list(
    LLprobit = LLprobit,
    LLprobitEl = LLprobitEl,
    LLprobitMs = LLprobitMs,
    pseudoR2el = pseudoR2el,
    pseudoR2ms = pseudoR2ms,
    R2 = R2
  )

  ms$wald <- list(
    null = wald_test_null,
    rho = wald_test_rho
  )

  class(ms) <- "summary.opsr"
  ms
}
