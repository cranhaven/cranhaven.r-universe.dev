#' Fit Generalized Kumaraswamy Regression Models
#'
#' @description
#' Fits regression models using the Generalized Kumaraswamy (GKw) family of
#' distributions for modeling response variables strictly bounded in the interval
#' (0, 1). The function provides a unified interface for fitting seven nested
#' submodels of the GKw family, allowing flexible modeling of proportions, rates,
#' and other bounded continuous outcomes through regression on distributional
#' parameters.
#'
#' Maximum Likelihood Estimation is performed via automatic differentiation using
#' the TMB (Template Model Builder) package, ensuring computational efficiency and
#' numerical accuracy. The interface follows standard R regression modeling
#' conventions similar to \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, and
#' \code{\link[betareg]{betareg}}, making it immediately familiar to R users.
#'
#' @param formula An object of class \code{\link[Formula]{Formula}} (or one that
#'   can be coerced to that class). The formula uses extended syntax to specify
#'   potentially different linear predictors for each distribution parameter:
#'
#'   \code{y ~ model_alpha | model_beta | model_gamma | model_delta | model_lambda}
#'
#'   where:
#'   \itemize{
#'     \item \code{y} is the response variable (must be in the open interval (0, 1))
#'     \item \code{model_alpha} specifies predictors for the \eqn{\alpha} parameter
#'     \item \code{model_beta} specifies predictors for the \eqn{\beta} parameter
#'     \item \code{model_gamma} specifies predictors for the \eqn{\gamma} parameter
#'     \item \code{model_delta} specifies predictors for the \eqn{\delta} parameter
#'     \item \code{model_lambda} specifies predictors for the \eqn{\lambda} parameter
#'   }
#'
#'   If a part is omitted or specified as \code{~ 1}, an intercept-only model is
#'   used for that parameter. Parts corresponding to fixed parameters (determined
#'   by \code{family}) are automatically ignored. See Details and Examples for
#'   proper usage.
#'
#' @param data A data frame containing the variables specified in \code{formula}.
#'   Standard R subsetting and missing value handling apply.
#'
#' @param family A character string specifying the distribution family from the
#'   Generalized Kumaraswamy hierarchy. Must be one of:
#'   \describe{
#'     \item{\code{"gkw"}}{Generalized Kumaraswamy (default). Five parameters:
#'       \eqn{\alpha, \beta, \gamma, \delta, \lambda}. Most flexible, suitable
#'       when data show complex behavior not captured by simpler families.}
#'     \item{\code{"bkw"}}{Beta-Kumaraswamy. Four parameters:
#'       \eqn{\alpha, \beta, \gamma, \delta} (fixes \eqn{\lambda = 1}).
#'       Combines Beta and Kumaraswamy flexibility.}
#'     \item{\code{"kkw"}}{Kumaraswamy-Kumaraswamy. Four parameters:
#'       \eqn{\alpha, \beta, \delta, \lambda} (fixes \eqn{\gamma = 1}).
#'       Alternative four-parameter generalization.}
#'     \item{\code{"ekw"}}{Exponentiated Kumaraswamy. Three parameters:
#'       \eqn{\alpha, \beta, \lambda} (fixes \eqn{\gamma = 1, \delta = 0}).
#'       Adds flexibility to standard Kumaraswamy.}
#'     \item{\code{"mc"}}{McDonald (Beta Power). Three parameters:
#'       \eqn{\gamma, \delta, \lambda} (fixes \eqn{\alpha = 1, \beta = 1}).
#'       Generalization of Beta distribution.}
#'     \item{\code{"kw"}}{Kumaraswamy. Two parameters: \eqn{\alpha, \beta}
#'       (fixes \eqn{\gamma = 1, \delta = 0, \lambda = 1}). Computationally
#'       efficient alternative to Beta with closed-form CDF.}
#'     \item{\code{"beta"}}{Beta distribution. Two parameters: \eqn{\gamma, \delta}
#'       (fixes \eqn{\alpha = 1, \beta = 1, \lambda = 1}). Standard choice for
#'       proportions and rates, corresponds to shape1 = \eqn{\gamma},
#'       shape2 = \eqn{\delta}.}
#'   }
#'   See Details for guidance on family selection.
#'
#' @param link Link function(s) for the distributional parameters. Can be specified as:
#'   \itemize{
#'     \item \strong{Single character string}: Same link for all relevant parameters.
#'       Example: \code{link = "log"} applies log link to all parameters.
#'     \item \strong{Named list}: Parameter-specific links for fine control.
#'       Example: \code{link = list(alpha = "log", beta = "log", delta = "logit")}
#'   }
#'
#'   \strong{Default links} (used if \code{link = NULL}):
#'   \itemize{
#'     \item \code{"log"} for \eqn{\alpha, \beta, \gamma, \lambda} (positive parameters)
#'     \item \code{"logit"} for \eqn{\delta} (parameter in (0, 1))
#'   }
#'
#'   \strong{Available link functions:}
#'   \describe{
#'     \item{\code{"log"}}{Logarithmic link. Maps \eqn{(0, \infty) \to (-\infty, \infty)}.
#'       Ensures positivity. Most common for shape parameters.}
#'     \item{\code{"logit"}}{Logistic link. Maps \eqn{(0, 1) \to (-\infty, \infty)}.
#'       Standard for probability-type parameters like \eqn{\delta}.}
#'     \item{\code{"probit"}}{Probit link using normal CDF. Maps \eqn{(0, 1) \to (-\infty, \infty)}.
#'       Alternative to logit, symmetric tails.}
#'     \item{\code{"cloglog"}}{Complementary log-log. Maps \eqn{(0, 1) \to (-\infty, \infty)}.
#'       Asymmetric, useful for skewed probabilities.}
#'     \item{\code{"cauchy"}}{Cauchy link using Cauchy CDF. Maps \eqn{(0, 1) \to (-\infty, \infty)}.
#'       Heavy-tailed alternative to probit.}
#'     \item{\code{"identity"}}{Identity link (no transformation). Use with caution;
#'       does not guarantee parameter constraints.}
#'     \item{\code{"sqrt"}}{Square root link. Maps \eqn{x \to \sqrt{x}}.
#'       Variance-stabilizing for some contexts.}
#'     \item{\code{"inverse"}}{Inverse link. Maps \eqn{x \to 1/x}.
#'       Useful for rate-type parameters.}
#'     \item{\code{"inverse-square"}}{Inverse squared link. Maps \eqn{x \to 1/x^2}.}
#'   }
#'
#' @param link_scale Numeric scale factor(s) controlling the transformation intensity
#'   of link functions. Can be:
#'   \itemize{
#'     \item \strong{Single numeric}: Same scale for all parameters.
#'     \item \strong{Named list}: Parameter-specific scales for fine-tuning.
#'       Example: \code{link_scale = list(alpha = 10, beta = 10, delta = 1)}
#'   }
#'
#'   \strong{Default scales} (used if \code{link_scale = NULL}):
#'   \itemize{
#'     \item 10 for \eqn{\alpha, \beta, \gamma, \lambda}
#'     \item 1 for \eqn{\delta}
#'   }
#'
#'   Larger values produce more gradual transformations; smaller values produce
#'   more extreme transformations. For probability-type links (logit, probit),
#'   smaller scales (e.g., 0.5-2) create steeper response curves, while larger
#'   scales (e.g., 5-20) create gentler curves. Adjust if convergence issues arise
#'   or if you need different response sensitivities.
#'
#' @param subset Optional vector specifying a subset of observations to be used
#'   in fitting. Can be a logical vector, integer indices, or expression evaluating
#'   to one of these. Standard R subsetting rules apply.
#'
#' @param weights Optional numeric vector of prior weights (e.g., frequency weights)
#'   for observations. Should be non-negative. Currently experimental; use with
#'   caution and validate results.
#'
#' @param offset Optional numeric vector or matrix specifying an \emph{a priori} known
#'   component to be included in the linear predictor(s). If a vector, it is applied
#'   to the first parameter's predictor. If a matrix, columns correspond to parameters
#'   in order (\eqn{\alpha, \beta, \gamma, \delta, \lambda}). Offsets are added to
#'   the linear predictor \emph{before} applying the link function.
#'
#' @param na.action A function specifying how to handle missing values (\code{NA}s).
#'   Options include:
#'   \describe{
#'     \item{\code{na.fail}}{Stop with error if \code{NA}s present (default via
#'       \code{getOption("na.action")})}
#'     \item{\code{\link[stats]{na.omit}}}{Remove observations with \code{NA}s}
#'     \item{\code{\link[stats]{na.exclude}}}{Like \code{na.omit} but preserves
#'       original length in residuals/fitted values}
#'   }
#'   See \code{\link[stats]{na.action}} for details.
#'
#' @param contrasts Optional list specifying contrasts for factor variables in the
#'   model. Format: named list where names are factor variable names and values are
#'   contrast specifications. See \code{\link[stats]{contrasts}} and the
#'   \code{contrasts.arg} argument of \code{\link[stats]{model.matrix}}.
#'
#' @param control A list of control parameters from \code{\link{gkw_control}}
#'   specifying technical details of the fitting process. This includes:
#'   \itemize{
#'     \item Optimization algorithm (\code{method})
#'     \item Starting values (\code{start})
#'     \item Fixed parameters (\code{fixed})
#'     \item Convergence tolerances (\code{maxit}, \code{reltol}, \code{abstol})
#'     \item Hessian computation (\code{hessian})
#'     \item Verbosity (\code{silent}, \code{trace})
#'   }
#'
#'   Default is \code{gkw_control()} which uses sensible defaults for most problems.
#'   See \code{\link{gkw_control}} for complete documentation of all options.
#'   \strong{Most users never need to modify control parameters.}
#'
#' @param model Logical. If \code{TRUE} (default), the model frame (data frame
#'   containing all variables used in fitting) is returned as component \code{model}
#'   of the result. Useful for prediction and diagnostics. Set to \code{FALSE} to
#'   reduce object size.
#'
#' @param x Logical. If \code{TRUE}, the list of model matrices (one for each
#'   modeled parameter) is returned as component \code{x}. Default \code{FALSE}.
#'   Set to \code{TRUE} if you need direct access to design matrices for custom
#'   calculations.
#'
#' @param y Logical. If \code{TRUE} (default), the response vector (after processing
#'   by \code{na.action} and \code{subset}) is returned as component \code{y}.
#'   Useful for residual calculations and diagnostics.
#'
#' @param ... Additional arguments. Currently used only for backward compatibility
#'   with deprecated arguments from earlier versions. Using deprecated arguments
#'   triggers informative warnings with migration guidance. Examples of deprecated
#'   arguments: \code{plot}, \code{conf.level}, \code{method}, \code{start},
#'   \code{fixed}, \code{hessian}, \code{silent}, \code{optimizer.control}.
#'   These should now be passed via the \code{control} argument.
#'
#' @return
#' An object of class \code{"gkwreg"}, which is a list containing the following
#' components. Standard S3 methods are available for this class (see Methods section).
#'
#' \strong{Model Specification:}
#' \describe{
#'   \item{\code{call}}{The matched function call}
#'   \item{\code{formula}}{The \code{Formula} object used}
#'   \item{\code{family}}{Character string: distribution family used}
#'   \item{\code{link}}{Named list: link functions for each parameter}
#'   \item{\code{link_scale}}{Named list: link scale values for each parameter}
#'   \item{\code{param_names}}{Character vector: names of parameters for this family}
#'   \item{\code{fixed_params}}{Named list: parameters fixed by family definition}
#'   \item{\code{control}}{The \code{gkw_control} object used for fitting}
#' }
#'
#' \strong{Parameter Estimates:}
#' \describe{
#'   \item{\code{coefficients}}{Named numeric vector: estimated regression coefficients
#'     (on link scale). Names follow the pattern "parameter:predictor", e.g.,
#'     "alpha:(Intercept)", "alpha:x1", "beta:(Intercept)", "beta:x2".}
#'   \item{\code{fitted_parameters}}{Named list: mean values for each distribution
#'     parameter (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) averaged across
#'     all observations}
#'   \item{\code{parameter_vectors}}{Named list: observation-specific parameter
#'     values. Contains vectors \code{alphaVec}, \code{betaVec}, \code{gammaVec},
#'     \code{deltaVec}, \code{lambdaVec}, each of length \code{nobs}}
#' }
#'
#' \strong{Fitted Values and Residuals:}
#' \describe{
#'   \item{\code{fitted.values}}{Numeric vector: fitted mean values
#'     \eqn{E[Y|X]} for each observation}
#'   \item{\code{residuals}}{Numeric vector: response residuals
#'     (observed - fitted) for each observation}
#' }
#'
#' \strong{Inference:}
#' \describe{
#'   \item{\code{vcov}}{Variance-covariance matrix of coefficient estimates.
#'     Only present if \code{control$hessian = TRUE}. \code{NULL} otherwise.}
#'   \item{\code{se}}{Numeric vector: standard errors of coefficients.
#'     Only present if \code{control$hessian = TRUE}. \code{NULL} otherwise.}
#' }
#'
#' \strong{Model Fit Statistics:}
#' \describe{
#'   \item{\code{loglik}}{Numeric: maximized log-likelihood value}
#'   \item{\code{aic}}{Numeric: Akaike Information Criterion
#'     (AIC = -2*loglik + 2*npar)}
#'   \item{\code{bic}}{Numeric: Bayesian Information Criterion
#'     (BIC = -2*loglik + log(nobs)*npar)}
#'   \item{\code{deviance}}{Numeric: deviance (-2 * loglik)}
#'   \item{\code{df.residual}}{Integer: residual degrees of freedom (nobs - npar)}
#'   \item{\code{nobs}}{Integer: number of observations used in fit}
#'   \item{\code{npar}}{Integer: total number of estimated parameters}
#' }
#'
#' \strong{Diagnostic Statistics:}
#' \describe{
#'   \item{\code{rmse}}{Numeric: Root Mean Squared Error of response residuals}
#'   \item{\code{efron_r2}}{Numeric: Efron's pseudo R-squared
#'     (1 - SSE/SST, where SSE = sum of squared errors,
#'     SST = total sum of squares)}
#'   \item{\code{mean_absolute_error}}{Numeric: Mean Absolute Error of response
#'     residuals}
#' }
#'
#' \strong{Optimization Details:}
#' \describe{
#'   \item{\code{convergence}}{Logical: \code{TRUE} if optimizer converged
#'     successfully, \code{FALSE} otherwise}
#'   \item{\code{message}}{Character: convergence message from optimizer}
#'   \item{\code{iterations}}{Integer: number of iterations used by optimizer}
#'   \item{\code{method}}{Character: optimization method used
#'     (e.g., "nlminb", "BFGS")}
#' }
#'
#' \strong{Optional Components} (returned if requested via \code{model}, \code{x}, \code{y}):
#' \describe{
#'   \item{\code{model}}{Data frame: the model frame (if \code{model = TRUE})}
#'   \item{\code{x}}{Named list: model matrices for each parameter
#'     (if \code{x = TRUE})}
#'   \item{\code{y}}{Numeric vector: the response variable (if \code{y = TRUE})}
#' }
#'
#' \strong{Internal:}
#' \describe{
#'   \item{\code{tmb_object}}{The raw object returned by \code{\link[TMB]{MakeADFun}}.
#'     Contains the TMB automatic differentiation function and environment.
#'     Primarily for internal use and advanced debugging.}
#' }
#'
#' @section Methods:
#' The following S3 methods are available for objects of class \code{"gkwreg"}:
#'
#' \strong{Basic Methods:}
#' \itemize{
#'   \item \code{\link{print.gkwreg}}: Print basic model information
#'   \item \code{\link{summary.gkwreg}}: Detailed model summary with coefficient
#'     tables, tests, and fit statistics
#'   \item \code{\link{coef.gkwreg}}: Extract coefficients
#'   \item \code{\link{vcov.gkwreg}}: Extract variance-covariance matrix
#'   \item \code{\link[stats]{logLik}}: Extract log-likelihood
#'   \item \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}: Information criteria
#' }
#'
#' \strong{Prediction and Fitted Values:}
#' \itemize{
#'   \item \code{\link{fitted.gkwreg}}: Extract fitted values
#'   \item \code{\link{residuals.gkwreg}}: Extract residuals (multiple types available)
#'   \item \code{\link{predict.gkwreg}}: Predict on new data
#' }
#'
#' \strong{Inference:}
#' \itemize{
#'   \item \code{\link{confint.gkwreg}}: Confidence intervals for parameters
#'   \item \code{\link{anova.gkwreg}}: Compare nested models via likelihood ratio tests
#' }
#'
#' \strong{Diagnostics:}
#' \itemize{
#'   \item \code{\link{plot.gkwreg}}: Comprehensive diagnostic plots (6 types)
#' }
#'
#' @details
#'
#' \subsection{Distribution Family Selection}{
#'
#' The Generalized Kumaraswamy family provides a flexible hierarchy for modeling
#' bounded responses. Selection should be guided by:
#'
#' \strong{1. Start Simple:} Begin with two-parameter families (\code{"kw"} or
#' \code{"beta"}) unless you have strong reasons to use more complex models.
#'
#' \strong{2. Model Comparison:} Use information criteria (AIC, BIC) and likelihood
#' ratio tests to compare nested models:
#' \preformatted{
#'   # Fit sequence of nested models
#'   fit_kw   <- gkwreg(y ~ x, data, family = "kw")
#'   fit_ekw  <- gkwreg(y ~ x, data, family = "ekw")
#'   fit_gkw  <- gkwreg(y ~ x, data, family = "gkw")
#'
#'   # Compare via AIC
#'   AIC(fit_kw, fit_ekw, fit_gkw)
#'
#'   # Formal test (nested models only)
#'   anova(fit_kw, fit_ekw, fit_gkw)
#' }
#'
#' \strong{3. Family Characteristics:}
#' \itemize{
#'   \item \strong{Beta}: Traditional choice, well-understood, good for symmetric
#'     or moderately skewed data
#'   \item \strong{Kumaraswamy (kw)}: Computationally efficient alternative to Beta,
#'     closed-form CDF, similar flexibility
#'   \item \strong{Exponentiated Kumaraswamy (ekw)}: Adds flexibility for extreme
#'     values and heavy tails
#'   \item \strong{Beta-Kumaraswamy (bkw)}, \strong{Kumaraswamy-Kumaraswamy (kkw)}:
#'     Four-parameter alternatives when three parameters insufficient
#'   \item \strong{McDonald (mc)}: Beta generalization via power parameter, useful
#'     for J-shaped distributions
#'   \item \strong{Kumaraswamy-Kumaraswamy (kkw)}: Most flexible, use only when
#'     simpler families inadequate. It extends kw
#'   \item \strong{Generalized Kumaraswamy (gkw)}: Most flexible, use only when
#'     simpler families inadequate
#' }
#'
#' \strong{4. Avoid Overfitting:} More different parameters better model. Use cross-validation
#' or hold-out validation to assess predictive performance.
#' }
#'
#' \subsection{Formula Specification}{
#'
#' The extended formula syntax allows different predictors for each parameter:
#'
#' \strong{Basic Examples:}
#' \preformatted{
#'   # Same predictors for both parameters (two-parameter family)
#'   y ~ x1 + x2
#'   # Equivalent to: y ~ x1 + x2 | x1 + x2
#'
#'   # Different predictors per parameter
#'   y ~ x1 + x2 | x3 + x4
#'   # alpha depends on x1, x2
#'   # beta depends on x3, x4
#'
#'   # Intercept-only for some parameters
#'   y ~ x1 | 1
#'   # alpha depends on x1
#'   # beta has only intercept
#'
#'   # Complex specification (five-parameter family)
#'   y ~ x1 | x2 | x3 | x4 | x5
#'   # alpha ~ x1, beta ~ x2, gamma ~ x3, delta ~ x4, lambda ~ x5
#' }
#'
#' \strong{Important Notes:}
#' \itemize{
#'   \item Formula parts correspond to parameters in order:
#'     \eqn{\alpha}, \eqn{\beta}, \eqn{\gamma}, \eqn{\delta}, \eqn{\lambda}
#'   \item Unused parts (due to family constraints) are automatically ignored
#'   \item Use \code{.} to include all predictors: \code{y ~ . | .}
#'   \item Standard R formula features work: interactions (\code{x1:x2}),
#'     polynomials (\code{poly(x, 2)}), transformations (\code{log(x)}), etc.
#' }
#' }
#'
#' \subsection{Link Functions and Scales}{
#'
#' Link functions map the range of distributional parameters to the real line,
#' ensuring parameter constraints are satisfied during optimization.
#'
#' \strong{Choosing Links:}
#' \itemize{
#'   \item \strong{Defaults are usually best}: The automatic choices
#'     (log for shape parameters, logit for delta) work well in most cases
#'   \item \strong{Alternative links}: Consider if you have theoretical reasons
#'     (e.g., probit for latent variable interpretation) or convergence issues
#'   \item \strong{Identity link}: Avoid unless you have constraints elsewhere;
#'     can lead to invalid parameter values during optimization
#' }
#'
#' \strong{Link Scales:}
#' The \code{link_scale} parameter controls transformation intensity. Think of it
#' as a "sensitivity" parameter:
#' \itemize{
#'   \item \strong{Larger values} (e.g., 20): Gentler response to predictor changes
#'   \item \strong{Smaller values} (e.g., 2): Steeper response to predictor changes
#'   \item \strong{Default (10)}: Balanced, works well for most cases
#' }
#'
#' Adjust only if:
#' \itemize{
#'   \item Convergence difficulties arise
#'   \item You need very steep or very gentle response curves
#'   \item Predictors have unusual scales (very large or very small)
#' }
#' }
#'
#' \subsection{Optimization and Convergence}{
#'
#' The default optimizer (\code{method = "nlminb"}) works well for most problems.
#' If convergence issues occur:
#'
#' \strong{1. Check Data:}
#' \itemize{
#'   \item Ensure response is strictly in (0, 1)
#'   \item Check for extreme outliers or influential points
#'   \item Verify predictors aren't perfectly collinear
#'   \item Consider rescaling predictors to similar ranges
#' }
#'
#' \strong{2. Try Alternative Optimizers:}
#' \preformatted{
#'   # BFGS often more robust for difficult problems
#'   fit <- gkwreg(y ~ x, data,
#'                 control = gkw_control(method = "BFGS"))
#'
#'   # Nelder-Mead for non-smooth objectives
#'   fit <- gkwreg(y ~ x, data,
#'                 control = gkw_control(method = "Nelder-Mead"))
#' }
#'
#' \strong{3. Adjust Tolerances:}
#' \preformatted{
#'   # Increase iterations and loosen tolerance
#'   fit <- gkwreg(y ~ x, data,
#'                 control = gkw_control(maxit = 1000, reltol = 1e-6))
#' }
#'
#' \strong{4. Provide Starting Values:}
#' \preformatted{
#'   # Fit simpler model first, use as starting values
#'   fit_simple <- gkwreg(y ~ 1, data, family = "kw")
#'   start_vals <- list(
#'     alpha = c(coef(fit_simple)[1], rep(0, ncol(X_alpha) - 1)),
#'     beta  = c(coef(fit_simple)[2], rep(0, ncol(X_beta) - 1))
#'   )
#'   fit_complex <- gkwreg(y ~ x1 + x2 | x3 + x4, data, family = "kw",
#'                          control = gkw_control(start = start_vals))
#' }
#'
#' \strong{5. Simplify Model:}
#' \itemize{
#'   \item Use simpler family (e.g., "kw" instead of "gkw")
#'   \item Reduce number of predictors
#'   \item Use intercept-only for some parameters
#' }
#' }
#'
#' \subsection{Standard Errors and Inference}{
#'
#' By default, standard errors are computed via the Hessian matrix at the MLE.
#' This provides valid asymptotic standard errors under standard regularity conditions.
#'
#' \strong{When Standard Errors May Be Unreliable:}
#' \itemize{
#'   \item Small sample sizes (n < 30-50 per parameter)
#'   \item Parameters near boundaries
#'   \item Highly collinear predictors
#'   \item Mis-specified models
#' }
#'
#' \strong{Alternatives:}
#' \itemize{
#'   \item Bootstrap confidence intervals (more robust, computationally expensive)
#'   \item Profile likelihood intervals via \code{confint(..., type = "profile")}
#'     (not yet implemented)
#'   \item Cross-validation for predictive performance assessment
#' }
#'
#' To skip Hessian computation (faster, no SEs):
#' \preformatted{
#'   fit <- gkwreg(y ~ x, data,
#'                 control = gkw_control(hessian = FALSE))
#' }
#' }
#'
#' \subsection{Model Diagnostics}{
#'
#' Always check model adequacy using diagnostic plots:
#' \preformatted{
#'   fit <- gkwreg(y ~ x, data, family = "kw")
#'   plot(fit)  # Six diagnostic plots
#' }
#'
#' Key diagnostics:
#' \itemize{
#'   \item \strong{Residual plots}: Check for patterns, heteroscedasticity
#'   \item \strong{Half-normal plot}: Assess distributional adequacy
#'   \item \strong{Cook's distance}: Identify influential observations
#'   \item \strong{Predicted vs observed}: Overall fit quality
#' }
#'
#' See \code{\link{plot.gkwreg}} for detailed interpretation guidance.
#' }
#'
#' \subsection{Computational Considerations}{
#'
#' \strong{Performance Tips:}
#' \itemize{
#'   \item GKw family: Most computationally expensive (~2-5x slower than kw/beta)
#'   \item Beta/Kw families: Fastest, use when adequate
#'   \item Large datasets (n > 10,000): Consider sampling for exploratory analysis
#'   \item TMB uses automatic differentiation: Fast gradient/Hessian computation
#'   \item Disable Hessian (\code{hessian = FALSE}) for faster fitting without SEs
#' }
#'
#' \strong{Memory Usage:}
#' \itemize{
#'   \item Set \code{model = FALSE}, \code{x = FALSE}, \code{y = FALSE} to reduce
#'     object size (but limits some post-fitting capabilities)
#'   \item Hessian matrix scales as O(p²) where p = number of parameters
#' }
#' }
#'
#' @references
#' \strong{Generalized Kumaraswamy Distribution:}
#'
#' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' \emph{Journal of Statistical Computation and Simulation}, \strong{81}(7), 883-898.
#' \doi{10.1080/00949650903530745}
#'
#' \strong{Kumaraswamy Distribution:}
#'
#' Kumaraswamy, P. (1980). A generalized probability density function for
#' double-bounded random processes. \emph{Journal of Hydrology}, \strong{46}(1-2), 79-88.
#' \doi{10.1016/0022-1694(80)90036-0}
#'
#' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution with
#' some tractability advantages. \emph{Statistical Methodology}, \strong{6}(1), 70-81.
#' \doi{10.1016/j.stamet.2008.04.001}
#'
#' \strong{Beta Regression:}
#'
#' Ferrari, S. L. P., & Cribari-Neto, F. (2004). Beta regression for modelling
#' rates and proportions. \emph{Journal of Applied Statistics}, \strong{31}(7), 799-815.
#' \doi{10.1080/0266476042000214501}
#'
#' Smithson, M., & Verkuilen, J. (2006). A better lemon squeezer? Maximum-likelihood
#' regression with beta-distributed dependent variables. \emph{Psychological Methods},
#' \strong{11}(1), 54-71.
#' \doi{10.1037/1082-989X.11.1.54}
#'
#' \strong{Template Model Builder (TMB):}
#'
#' Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M. (2016).
#' TMB: Automatic Differentiation and Laplace Approximation. \emph{Journal of
#' Statistical Software}, \strong{70}(5), 1-21.
#' \doi{10.18637/jss.v070.i05}
#'
#' \strong{Related Software:}
#'
#' Zeileis, A., & Croissant, Y. (2010). Extended Model Formulas in R: Multiple
#' Parts and Multiple Responses. \emph{Journal of Statistical Software},
#' \strong{34}(1), 1-13.
#' \doi{10.18637/jss.v034.i01}
#'
#' @author Lopes, J. E.
#'
#' Maintainer: Lopes, J. E.
#'
#' @seealso
#' \strong{Control and Inference:}
#' \code{\link{gkw_control}} for fitting control parameters,
#' \code{\link{confint.gkwreg}} for confidence intervals,
#' \code{\link{anova.gkwreg}} for model comparison
#'
#' \strong{Methods:}
#' \code{\link{summary.gkwreg}}, \code{\link{plot.gkwreg}},
#' \code{\link{coef.gkwreg}}, \code{\link{vcov.gkwreg}},
#' \code{\link{fitted.gkwreg}}, \code{\link{residuals.gkwreg}},
#' \code{\link{predict.gkwreg}}
#'
#' \strong{Distributions:}
#' \code{\link[gkwdist]{dgkw}}, \code{\link[gkwdist]{pgkw}},
#' \code{\link[gkwdist]{qgkw}}, \code{\link[gkwdist]{rgkw}}
#' for the GKw distribution family functions
#'
#' \strong{Related Packages:}
#' \code{\link[betareg]{betareg}} for traditional beta regression,
#' \code{\link[Formula]{Formula}} for extended formula interface,
#' \code{\link[TMB]{MakeADFun}} for TMB functionality
#'
#' @examples
#' \donttest{
#' # SECTION 1: Basic Usage - Getting Started
#' # Load packages and data
#' library(gkwreg)
#' library(gkwdist)
#' data(GasolineYield)
#'
#' # Example 1.1: Simplest possible model (intercept-only, all defaults)
#' fit_basic <- gkwreg(yield ~ 1, data = GasolineYield, family = "kw")
#' summary(fit_basic)
#'
#' # Example 1.2: Model with predictors (uses all defaults)
#' # Default: family = "gkw", method = "nlminb", hessian = TRUE
#' fit_default <- gkwreg(yield ~ batch + temp, data = GasolineYield)
#' summary(fit_default)
#'
#' # Example 1.3: Kumaraswamy model (two-parameter family)
#' # Default link functions: log for both alpha and beta
#' fit_kw <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' summary(fit_kw)
#'
#' par(mfrow = c(3, 2))
#' plot(fit_kw, ask = FALSE)
#'
#' # Example 1.4: Beta model for comparison
#' # Default links: log for gamma and delta
#' fit_beta <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "beta")
#'
#' # Compare models using AIC/BIC
#' AIC(fit_kw, fit_beta)
#' BIC(fit_kw, fit_beta)
#'
#' # SECTION 2: Using gkw_control() for Customization
#'
#' # Example 2.1: Change optimization method to BFGS
#' fit_bfgs <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(method = "BFGS")
#' )
#' summary(fit_bfgs)
#'
#' # Example 2.2: Increase iterations and enable verbose output
#' fit_verbose <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(
#'     method = "nlminb",
#'     maxit = 1000,
#'     silent = FALSE, # Show optimization progress
#'     trace = 1 # Print iteration details
#'   )
#' )
#'
#' # Example 2.3: Fast fitting without standard errors
#' # Useful for model exploration or large datasets
#' fit_fast <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(hessian = FALSE)
#' )
#' # Note: Cannot compute confint() without hessian
#' coef(fit_fast) # Point estimates still available
#'
#' # Example 2.4: Custom convergence tolerances
#' fit_tight <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(
#'     reltol = 1e-10, # Tighter convergence
#'     maxit = 2000 # More iterations allowed
#'   )
#' )
#'
#' # SECTION 3: Advanced Formula Specifications
#'
#' # Example 3.1: Different predictors for different parameters
#' # alpha depends on batch, beta depends on temp
#' fit_diff <- gkwreg(
#'   yield ~ batch | temp,
#'   data = GasolineYield,
#'   family = "kw"
#' )
#' summary(fit_diff)
#'
#' # Example 3.2: Intercept-only for one parameter
#' # alpha varies with predictors, beta is constant
#' fit_partial <- gkwreg(
#'   yield ~ batch + temp | 1,
#'   data = GasolineYield,
#'   family = "kw"
#' )
#'
#' # Example 3.3: Complex model with interactions
#' fit_interact <- gkwreg(
#'   yield ~ batch * temp | temp + I(temp^2),
#'   data = GasolineYield,
#'   family = "kw"
#' )
#'
#' # SECTION 4: Working with Different Families
#'
#' # Example 4.1: Fit multiple families and compare
#' families <- c("beta", "kw", "ekw", "bkw", "gkw")
#' fits <- lapply(families, function(fam) {
#'   gkwreg(yield ~ batch + temp, data = GasolineYield, family = fam)
#' })
#' names(fits) <- families
#'
#' # Compare via information criteria
#' comparison <- data.frame(
#'   Family = families,
#'   LogLik = sapply(fits, logLik),
#'   AIC = sapply(fits, AIC),
#'   BIC = sapply(fits, BIC),
#'   npar = sapply(fits, function(x) x$npar)
#' )
#' print(comparison)
#'
#' # Example 4.2: Formal nested model testing
#' fit_kw <- gkwreg(yield ~ batch + temp, GasolineYield, family = "kw")
#' fit_ekw <- gkwreg(yield ~ batch + temp, GasolineYield, family = "ekw")
#' fit_gkw <- gkwreg(yield ~ batch + temp, GasolineYield, family = "gkw")
#' anova(fit_kw, fit_ekw, fit_gkw)
#'
#' # SECTION 5: Link Functions and Scales
#'
#' # Example 5.1: Custom link functions
#' fit_links <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   link = list(alpha = "sqrt", beta = "log")
#' )
#'
#' # Example 5.2: Custom link scales
#' # Smaller scale = steeper response curve
#' fit_scale <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   link_scale = list(alpha = 5, beta = 15)
#' )
#'
#' # Example 5.3: Uniform link for all parameters
#' fit_uniform <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   link = "log" # Single string applied to all
#' )
#'
#' # SECTION 6: Prediction and Inference
#'
#' # Fit model for prediction examples
#' fit <- gkwreg(yield ~ batch + temp, GasolineYield, family = "kw")
#'
#' # Example 6.1: Confidence intervals at different levels
#' confint(fit, level = 0.95) # 95% CI
#' confint(fit, level = 0.90) # 90% CI
#' confint(fit, level = 0.99) # 99% CI
#'
#' # SECTION 7: Diagnostic Plots and Model Checking
#'
#' fit <- gkwreg(yield ~ batch + temp, GasolineYield, family = "kw")
#'
#' # Example 7.1: All diagnostic plots (default)
#' par(mfrow = c(3, 2))
#' plot(fit, ask = FALSE)
#'
#' # Example 7.2: Select specific plots
#' par(mfrow = c(3, 1))
#' plot(fit, which = c(2, 4, 5)) # Cook's distance, Residuals, Half-normal
#'
#' # Example 7.3: Using ggplot2 for modern graphics
#' plot(fit, use_ggplot = TRUE, arrange_plots = TRUE)
#'
#' # Example 7.4: Customized half-normal plot
#' par(mfrow = c(1, 1))
#' plot(fit,
#'   which = 5,
#'   type = "quantile",
#'   nsim = 200, # More simulations for smoother envelope
#'   level = 0.95
#' ) # 95% confidence envelope
#'
#' # Example 7.5: Extract diagnostic data programmatically
#' diagnostics <- plot(fit, save_diagnostics = TRUE)
#' head(diagnostics$data) # Residuals, Cook's distance, etc.
#'
#' # SECTION 8: Real Data Example - Food Expenditure
#'
#' # Load and prepare data
#' data(FoodExpenditure, package = "betareg")
#' food_data <- FoodExpenditure
#' food_data$prop <- food_data$food / food_data$income
#'
#' # Example 8.1: Basic model
#' fit_food <- gkwreg(
#'   prop ~ persons | income,
#'   data = food_data,
#'   family = "kw"
#' )
#' summary(fit_food)
#'
#' # Example 8.2: Compare with Beta regression
#' fit_food_beta <- gkwreg(
#'   prop ~ persons | income,
#'   data = food_data,
#'   family = "beta"
#' )
#'
#' # Which fits better?
#' AIC(fit_food, fit_food_beta)
#'
#' # Example 8.3: Model diagnostics
#' par(mfrow = c(3, 1))
#' plot(fit_food, which = c(2, 5, 6))
#'
#' # Example 8.4: Interpretation via effects
#' # How does proportion spent on food change with income?
#' income_seq <- seq(min(food_data$income), max(food_data$income), length = 50)
#' pred_data <- data.frame(
#'   persons = median(food_data$persons),
#'   income = income_seq
#' )
#' pred_food <- predict(fit_food, newdata = pred_data, type = "response")
#'
#' par(mfrow = c(1, 1))
#' plot(food_data$income, food_data$prop,
#'   xlab = "Income", ylab = "Proportion Spent on Food",
#'   main = "Food Expenditure Pattern"
#' )
#' lines(income_seq, pred_food, col = "red", lwd = 2)
#'
#' # SECTION 9: Simulation Studies
#'
#' # Example 9.1: Simple Kumaraswamy simulation
#' set.seed(123)
#' n <- 500
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#'
#' # True model: log(alpha) = 0.8 + 0.3*x1, log(beta) = 1.2 - 0.2*x2
#' eta_alpha <- 0.8 + 0.3 * x1
#' eta_beta <- 1.2 - 0.2 * x2
#' alpha_true <- exp(eta_alpha)
#' beta_true <- exp(eta_beta)
#'
#' # Generate response
#' y <- rkw(n, alpha = alpha_true, beta = beta_true)
#' sim_data <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit and check parameter recovery
#' fit_sim <- gkwreg(y ~ x1 | x2, data = sim_data, family = "kw")
#'
#' # Compare estimated vs true coefficients
#' cbind(
#'   True = c(0.8, 0.3, 1.2, -0.2),
#'   Estimated = coef(fit_sim),
#'   SE = fit_sim$se
#' )
#'
#' # Example 9.2: Complex simulation with all five parameters
#' set.seed(2203)
#' n <- 2000
#' x <- runif(n, -1, 1)
#'
#' # True parameters
#' alpha <- exp(0.5 + 0.3 * x)
#' beta <- exp(1.0 - 0.2 * x)
#' gamma <- exp(0.7 + 0.4 * x)
#' delta <- plogis(0.0 + 0.5 * x) # logit scale
#' lambda <- exp(-0.3 + 0.2 * x)
#'
#' # Generate from GKw
#' y <- rgkw(n,
#'   alpha = alpha, beta = beta, gamma = gamma,
#'   delta = delta, lambda = lambda
#' )
#' sim_data2 <- data.frame(y = y, x = x)
#'
#' # Fit GKw model
#' fit_gkw <- gkwreg(
#'   y ~ x | x | x | x | x,
#'   data = sim_data2,
#'   family = "gkw",
#'   control = gkw_control(method = "L-BFGS-B", maxit = 2000)
#' )
#' summary(fit_gkw)
#'
#' # SECTION 10: Handling Convergence Issues
#'
#' # Example 10.1: Try different optimizers
#' methods <- c("nlminb", "BFGS", "Nelder-Mead", "CG")
#' fits_methods <- lapply(methods, function(m) {
#'   tryCatch(
#'     gkwreg(yield ~ batch + temp, GasolineYield,
#'       family = "kw",
#'       control = gkw_control(method = m, silent = TRUE)
#'     ),
#'     error = function(e) NULL
#'   )
#' })
#' names(fits_methods) <- methods
#'
#' # Check which converged
#' converged <- sapply(fits_methods, function(f) {
#'   if (is.null(f)) {
#'     return(FALSE)
#'   }
#'   f$convergence
#' })
#' print(converged)
#'
#' # Example 10.2: Verbose mode for debugging
#' fit_debug <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   control = gkw_control(
#'     method = "BFGS",
#'     silent = TRUE,
#'     trace = 0, # 2, Maximum verbosity
#'     maxit = 1000
#'   )
#' )
#'
#' # SECTION 11: Memory and Performance Optimization
#'
#' # Example 11.1: Minimal object for large datasets
#' fit_minimal <- gkwreg(
#'   yield ~ batch + temp,
#'   data = GasolineYield,
#'   family = "kw",
#'   model = FALSE, # Don't store model frame
#'   x = FALSE, # Don't store design matrices
#'   y = FALSE, # Don't store response
#'   control = gkw_control(hessian = FALSE) # Skip Hessian
#' )
#'
#' # Much smaller object
#' object.size(fit_minimal)
#'
#' # Trade-off: Limited post-fitting capabilities
#' # Can still use: coef(), logLik(), AIC(), BIC()
#' # Cannot use: predict(), some diagnostics
#'
#' # Example 11.2: Fast exploratory analysis
#' # Fit many models quickly without standard errors
#' formulas <- list(
#'   yield ~ batch,
#'   yield ~ temp,
#'   yield ~ batch + temp,
#'   yield ~ batch * temp
#' )
#'
#' fast_fits <- lapply(formulas, function(f) {
#'   gkwreg(f, GasolineYield,
#'     family = "kw",
#'     control = gkw_control(hessian = FALSE),
#'     model = FALSE, x = FALSE, y = FALSE
#'   )
#' })
#'
#' # Compare models via AIC
#' sapply(fast_fits, AIC)
#'
#' # Refit best model with full inference
#' best_formula <- formulas[[which.min(sapply(fast_fits, AIC))]]
#' fit_final <- gkwreg(best_formula, GasolineYield, family = "kw")
#' summary(fit_final)
#'
#' # SECTION 12: Model Selection and Comparison
#'
#' # Example 12.1: Nested model testing
#' fit1 <- gkwreg(yield ~ 1, GasolineYield, family = "kw")
#' fit2 <- gkwreg(yield ~ batch, GasolineYield, family = "kw")
#' fit3 <- gkwreg(yield ~ batch + temp, GasolineYield, family = "kw")
#'
#' # Likelihood ratio tests
#' anova(fit1, fit2, fit3)
#'
#' # Example 12.2: Information criteria table
#' models <- list(
#'   "Intercept only" = fit1,
#'   "Batch effect" = fit2,
#'   "Batch + Temp" = fit3
#' )
#'
#' ic_table <- data.frame(
#'   Model = names(models),
#'   df = sapply(models, function(m) m$npar),
#'   LogLik = sapply(models, logLik),
#'   AIC = sapply(models, AIC),
#'   BIC = sapply(models, BIC),
#'   Delta_AIC = sapply(models, AIC) - min(sapply(models, AIC))
#' )
#' print(ic_table)
#'
#' # Example 12.3: Cross-validation for predictive performance
#' # 5-fold cross-validation
#' set.seed(2203)
#' n <- nrow(GasolineYield)
#' folds <- sample(rep(1:5, length.out = n))
#'
#' cv_rmse <- sapply(1:5, function(fold) {
#'   train <- GasolineYield[folds != fold, ]
#'   test <- GasolineYield[folds == fold, ]
#'
#'   fit_train <- gkwreg(yield ~ batch + temp, train,
#'     family = "kw"
#'   )
#'   pred_test <- predict(fit_train, newdata = test, type = "response")
#'
#'   sqrt(mean((test$yield - pred_test)^2))
#' })
#'
#' cat("Cross-validated RMSE:", mean(cv_rmse), "\n")
#' }
#' @keywords regression models
#' @export
gkwreg <- function(formula,
                   data,
                   family = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"),
                   link = NULL,
                   link_scale = NULL,
                   subset = NULL,
                   weights = NULL,
                   offset = NULL,
                   na.action = getOption("na.action"),
                   contrasts = NULL,
                   control = gkw_control(),
                   model = TRUE,
                   x = FALSE,
                   y = TRUE,
                   ...) {
  # BACKWARD COMPATIBILITY LAYER
  # Handle deprecated arguments with informative warnings


  dots <- list(...)

  # Check for completely removed arguments (violated Separation of Concerns)
  if ("plot" %in% names(dots)) {
    warning(
      "Argument 'plot' has been removed from gkwreg().\n",
      "  Reason: This argument had no effect and was misleading.\n",
      "  Solution: Use plot(fitted_model) after fitting to generate diagnostics.\n",
      "  Example: fit <- gkwreg(...); plot(fit)",
      call. = FALSE
    )
    dots$plot <- NULL
  }

  if ("conf.level" %in% names(dots)) {
    warning(
      "Argument 'conf.level' has been removed from gkwreg().\n",
      "  Reason: Confidence intervals can be computed at any level without refitting.\n",
      "  Solution: Use confint(fitted_model, level = your_level) after fitting.\n",
      "  Example: fit <- gkwreg(...); confint(fit, level = 0.90)",
      call. = FALSE
    )
    dots$conf.level <- NULL
  }

  # Check for arguments that moved to control object
  moved_to_control <- c(
    "method", "start", "fixed", "hessian",
    "silent", "optimizer.control"
  )
  found_moved <- intersect(names(dots), moved_to_control)

  if (length(found_moved) > 0) {
    warning(
      "Arguments ", paste0("'", found_moved, "'", collapse = ", "),
      " should now be passed via 'control' argument.\n",
      "  New syntax: gkwreg(..., control = gkw_control(",
      paste(found_moved, "= ...", collapse = ", "), "))\n",
      "  Attempting to use provided values, but please update your code.",
      call. = FALSE, immediate. = TRUE
    )

    # Build control from deprecated arguments
    deprecated_control_args <- list()
    for (arg in found_moved) {
      if (arg == "optimizer.control") {
        # Special handling: merge optimizer.control into control
        opt_ctrl <- dots[[arg]]
        if (is.list(opt_ctrl)) {
          deprecated_control_args <- utils::modifyList(deprecated_control_args, opt_ctrl)
        }
      } else {
        deprecated_control_args[[arg]] <- dots[[arg]]
      }
      dots[[arg]] <- NULL
    }

    # Merge deprecated args with user-provided control (user takes precedence)
    if (inherits(control, "gkw_control")) {
      # Convert control object to list, merge, then recreate
      control_list <- as.list(control)
      control_list <- utils::modifyList(deprecated_control_args, control_list)
      control <- do.call(gkw_control, control_list[names(control_list) %in%
        names(formals(gkw_control))])
    } else {
      # User provided a list directly, merge it
      deprecated_control_args <- utils::modifyList(
        deprecated_control_args,
        as.list(control)
      )
      control <- do.call(gkw_control, deprecated_control_args)
    }
  }

  # Warn about completely unused arguments
  if (length(dots) > 0) {
    warning(
      "Unused arguments: ", paste(names(dots), collapse = ", "),
      call. = FALSE
    )
  }

  # VALIDATION & SETUP
  # Store the call for the result
  call <- match.call()

  # Match family argument
  family <- match.arg(family, choices = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

  # Validate control object
  if (!inherits(control, "gkw_control")) {
    stop(
      "'control' must be an object from gkw_control().\n",
      "  Use: control = gkw_control(...)",
      call. = FALSE
    )
  }

  # Check required packages
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop(
      "Package 'Formula' is required but not installed.\n",
      "  Install with: install.packages('Formula')",
      call. = FALSE
    )
  }

  if (!requireNamespace("TMB", quietly = TRUE)) {
    stop(
      "Package 'TMB' is required but not installed.\n",
      "  Install with: install.packages('TMB')",
      call. = FALSE
    )
  }

  # Extract control parameters for easier access
  method <- control$method
  use_nlminb <- (method == "nlminb")
  silent <- control$silent

  # PARAMETER & FORMULA PROCESSING
  # Using existing internal functions

  # Get parameter information for the specified family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  fixed_params <- param_info$fixed
  param_positions <- param_info$positions

  # Convert to Formula object
  formula_obj <- Formula::as.Formula(formula)

  # Process formula parts for each parameter
  formula_list <- .process_formula_parts(
    formula_obj, param_names,
    fixed_params, data
  )

  # Process link functions
  link_list <- .process_link(link, param_names, fixed_params)

  # Process link scales
  link_scale_list <- .process_link_scale(
    link_scale, link_list,
    param_names, fixed_params
  )

  # Convert link strings to integers for TMB
  link_ints <- .convert_links_to_int(link_list)

  # DATA EXTRACTION & VALIDATION
  # Using existing internal functions

  # Extract model frames, responses, and model matrices
  model_data <- .extract_model_data(
    formula_list = formula_list,
    data = data,
    subset = subset,
    weights = weights,
    na.action = na.action,
    offset = offset,
    contrasts = contrasts,
    original_call = call
  )

  # Get response variable
  y_var <- model_data$y

  # Validate response is in (0, 1)
  invisible(.validate_data(y_var, length(param_names)))

  # FIXED PARAMETERS PROCESSING
  # Using existing internal function

  # Process fixed parameters (from control or family definition)
  fixed_processed <- .process_fixed(control$fixed, param_names, fixed_params)

  # TMB PREPARATION
  # Using existing internal functions

  # Prepare TMB data with correct matrices based on family
  tmb_data <- .prepare_tmb_data(
    model_data, family, param_names, fixed_processed,
    link_ints, link_scale_list, y_var, param_positions
  )

  # Prepare TMB parameters in the correct structure for the family
  tmb_params <- .prepare_tmb_params(
    model_data, family, param_names, fixed_processed,
    param_positions
  )

  # Override with user-provided starting values if available
  if (!is.null(control$start)) {
    for (param_name in names(control$start)) {
      if (param_name %in% param_names) {
        pos <- param_positions[[param_name]]
        tmb_param_name <- paste0("beta", pos)
        if (tmb_param_name %in% names(tmb_params)) {
          user_start <- control$start[[param_name]]
          expected_length <- length(tmb_params[[tmb_param_name]])
          if (length(user_start) == expected_length) {
            tmb_params[[tmb_param_name]] <- user_start
          } else {
            warning(
              "Starting value for '", param_name, "' has length ",
              length(user_start), " but expected ", expected_length,
              ". Ignoring user-provided starting value.",
              call. = FALSE
            )
          }
        }
      }
    }
  }

  # TMB MODEL COMPILATION
  # Using existing internal function

  # Determine TMB model name based on family
  if (family == "beta") {
    dll_name <- "gkwbetareg"
  } else {
    dll_name <- paste0(family, "reg")
  }

  if (!silent) {
    message("Using TMB model: ", dll_name)
  }

  # Check and compile TMB model if needed
  .check_and_compile_TMB_code(dll_name, verbose = !silent)

  # Create TMB object
  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_params,
    DLL = dll_name,
    silent = silent
  )

  # OPTIMIZATION

  if (use_nlminb) {
    # Use nlminb optimizer
    if (!silent) message("Optimizing with nlminb...")

    opt <- tryCatch(
      stats::nlminb(
        start = obj$par,
        objective = obj$fn,
        gradient = obj$gr,
        control = control$nlminb_control
      ),
      error = function(e) {
        stop("Optimization with nlminb failed: ", e$message, call. = FALSE)
      }
    )

    # Extract results
    fit_result <- list(
      coefficients = obj$env$last.par,
      loglik = -opt$objective,
      convergence = (opt$convergence == 0),
      message = opt$message,
      iterations = opt$iterations
    )
  } else {
    # Use optim with specified method
    if (!silent) {
      message("Optimizing with optim (method = ", method, ")...")
    }

    opt <- tryCatch(
      stats::optim(
        par = obj$par,
        fn = obj$fn,
        gr = obj$gr,
        method = method,
        control = control$optim_control
      ),
      error = function(e) {
        stop("Optimization with optim (", method, ") failed: ",
          e$message,
          call. = FALSE
        )
      }
    )

    # Extract results
    fit_result <- list(
      coefficients = opt$par,
      loglik = -opt$value,
      convergence = (opt$convergence == 0),
      message = if (opt$convergence == 0) {
        "Successful convergence"
      } else {
        paste("Optimization failed (code ", opt$convergence, ")")
      },
      iterations = opt$counts[1]
    )
  }

  # COEFFICIENT NAMING
  # Using existing internal function

  # Format coefficient names with parameter mappings
  coef_names <- .format_coefficient_names(param_names, model_data, param_positions)
  names(fit_result$coefficients) <- coef_names

  # HESSIAN & STANDARD ERRORS


  if (control$hessian) {
    if (!silent) message("Computing standard errors...")

    tryCatch(
      {
        sd_report <- TMB::sdreport(obj, getJointPrecision = TRUE)
        fit_result$se <- as.vector(sd_report$sd)
        fit_result$vcov <- as.matrix(sd_report$cov)

        # Add names
        names(fit_result$se) <- coef_names
        rownames(fit_result$vcov) <- coef_names
        colnames(fit_result$vcov) <- coef_names
      },
      error = function(e) {
        warning(
          "Could not compute standard errors: ", e$message, "\n",
          "  Model results are still valid for point estimates.",
          call. = FALSE
        )
        fit_result$se <- rep(NA_real_, length(fit_result$coefficients))
        fit_result$vcov <- matrix(
          NA_real_,
          length(fit_result$coefficients),
          length(fit_result$coefficients)
        )
        names(fit_result$se) <- coef_names
        rownames(fit_result$vcov) <- coef_names
        colnames(fit_result$vcov) <- coef_names
      }
    )
  } else {
    # Hessian not requested
    fit_result$se <- NULL
    fit_result$vcov <- NULL
  }


  # EXTRACT TMB REPORT


  tmb_report <- obj$report()

  # Extract parameter means
  alpha_mean <- tmb_report$alpha_mean
  beta_mean <- tmb_report$beta_mean
  gamma_mean <- tmb_report$gamma_mean
  delta_mean <- tmb_report$delta_mean
  lambda_mean <- tmb_report$lambda_mean

  # Extract parameter vectors for each observation
  alphaVec <- if ("alphaVec" %in% names(tmb_report)) {
    tmb_report$alphaVec
  } else {
    rep(alpha_mean, length(y_var))
  }

  betaVec <- if ("betaVec" %in% names(tmb_report)) {
    tmb_report$betaVec
  } else {
    rep(beta_mean, length(y_var))
  }

  gammaVec <- if ("gammaVec" %in% names(tmb_report)) {
    tmb_report$gammaVec
  } else {
    rep(gamma_mean, length(y_var))
  }

  deltaVec <- if ("deltaVec" %in% names(tmb_report)) {
    tmb_report$deltaVec
  } else {
    rep(delta_mean, length(y_var))
  }

  lambdaVec <- if ("lambdaVec" %in% names(tmb_report)) {
    tmb_report$lambdaVec
  } else {
    rep(lambda_mean, length(y_var))
  }

  # Extract fitted values
  fitted_values <- if ("fitted" %in% names(tmb_report)) {
    tmb_report$fitted
  } else {
    rep(NA_real_, length(y_var))
  }


  # FIT STATISTICS


  # Calculate residuals
  response_residuals <- y_var - fitted_values

  # Sample size and parameter count
  n_obs <- length(y_var)
  npar <- length(fit_result$coefficients)

  # Information criteria
  nll <- -fit_result$loglik
  deviance <- 2.0 * nll
  aic <- deviance + 2.0 * npar
  bic <- deviance + log(n_obs) * npar

  # Degrees of freedom
  df.residual <- n_obs - npar

  # Additional fit statistics
  rmse <- sqrt(mean(response_residuals^2, na.rm = TRUE))
  sse <- sum((y_var - fitted_values)^2, na.rm = TRUE)
  sst <- sum((y_var - mean(y_var, na.rm = TRUE))^2, na.rm = TRUE)
  efron_r2 <- if (sst > 0) 1 - sse / sst else NA_real_
  mean_absolute_error <- mean(abs(response_residuals), na.rm = TRUE)


  # BUILD RESULT OBJECT
  # CRITICAL: Maintain exact same structure and names as original


  result <- list(
    call = call,
    family = family,
    formula = formula,
    coefficients = fit_result$coefficients,
    fitted.values = as.vector(fitted_values),
    residuals = as.vector(response_residuals),
    fitted_parameters = list(
      alpha = alpha_mean,
      beta = beta_mean,
      gamma = gamma_mean,
      delta = delta_mean,
      lambda = lambda_mean
    ),
    parameter_vectors = list(
      alphaVec = alphaVec,
      betaVec = betaVec,
      gammaVec = gammaVec,
      deltaVec = deltaVec,
      lambdaVec = lambdaVec
    ),
    link = link_list,
    link_scale = link_scale_list,
    param_names = param_names,
    fixed_params = fixed_params,
    loglik = fit_result$loglik,
    aic = aic,
    bic = bic,
    deviance = deviance,
    df.residual = df.residual,
    nobs = n_obs,
    npar = npar,
    vcov = fit_result$vcov,
    se = fit_result$se,
    convergence = fit_result$convergence,
    message = fit_result$message,
    iterations = fit_result$iterations,
    rmse = rmse,
    efron_r2 = efron_r2,
    mean_absolute_error = mean_absolute_error,
    method = method,
    control = control, # Store control for reference
    tmb_object = obj
  )

  # Add optional components
  if (x) result$x <- model_data$matrices
  if (y) result$y <- y_var
  if (model) result$model <- model_data$model

  # Set class
  class(result) <- "gkwreg"

  return(result)
}


#' Prepare TMB Parameters for GKw Regression
#'
#' @param model_data List of model data.
#' @param family Family name.
#' @param param_names Names of parameters.
#' @param fixed List of fixed parameters.
#' @param param_positions Parameter position mapping for the family.
#' @return A list with TMB parameters.
#' @keywords internal
.prepare_tmb_params <- function(model_data, family, param_names, fixed, param_positions) {
  # Initialize params list with empty vectors for beta1, beta2, etc.
  params <- list()

  # Number of beta parameters needed depends on the family
  num_params <- switch(family,
    "gkw" = 5,
    "bkw" = 4,
    "kkw" = 4,
    "ekw" = 3,
    "mc" = 3,
    "kw" = 2,
    "beta" = 2
  )

  # Initialize empty vectors for all beta parameters
  for (i in 1:num_params) {
    params[[paste0("beta", i)]] <- numeric(0)
  }

  # Get non-fixed parameters
  non_fixed_params <- setdiff(param_names, names(fixed))

  # Fill in the parameter vectors
  for (param in non_fixed_params) {
    # Get TMB parameter position based on family
    tmb_pos <- param_positions[[param]]

    # Skip if not mapped
    if (is.null(tmb_pos) || is.na(tmb_pos)) next

    # Get the model matrix for this parameter
    mat_name <- paste0("beta", tmb_pos)

    # If parameter exists in model_data, use it
    if (param %in% names(model_data$matrices)) {
      X <- model_data$matrices[[param]]

      # Initialize with zeros
      params[[mat_name]] <- rep(0, ncol(X))

      # Set reasonable starting values for intercept
      if (ncol(X) > 0) {
        # For the intercept, use a reasonable value
        if (param %in% c("alpha", "beta", "gamma", "lambda")) {
          params[[mat_name]][1] <- 0.0 # log(1) = 0
        } else if (param == "delta") {
          params[[mat_name]][1] <- 0.0 # logit(0.5) = 0
        }
      }
    }
  }

  return(params)
}


#' Prepare TMB Data for GKw Regression
#'
#' @param model_data List of model data.
#' @param family Family name.
#' @param param_names Names of parameters.
#' @param fixed List of fixed parameters and coefficients.
#' @param link_ints List of link function integers.
#' @param link_scale_list List of link scale values.
#' @param y Response variable.
#' @param param_positions Parameter position mapping for the family.
#' @return A list with TMB data.
#' @keywords internal
.prepare_tmb_data <- function(model_data, family, param_names, fixed, link_ints, link_scale_list, y, param_positions) {
  # Initialize TMB data
  tmb_data <- list(
    y = y,
    useMeanCache = 1, # Enable mean caching
    calcFitted = 1, # Calculate fitted values
    userChunkSize = 100 # Reasonable chunk size
  )

  # All families need matrices and link types
  # The number of X matrices and link types needed depends on the family
  num_params <- switch(family,
    "gkw" = 5,
    "bkw" = 4,
    "kkw" = 4,
    "ekw" = 3,
    "mc" = 3,
    "kw" = 2,
    "beta" = 2
  )

  # Initialize default matrices and links for all required parameters
  for (i in 1:num_params) {
    matrix_name <- paste0("X", i)
    link_name <- paste0("link_type", i)
    scale_name <- paste0("scale", i)

    # Default empty matrix with 1 column (intercept only)
    tmb_data[[matrix_name]] <- matrix(0, nrow = length(y), ncol = 1)

    # Default link is log (1) and scale is 10
    tmb_data[[link_name]] <- 1
    tmb_data[[scale_name]] <- 10.0
  }

  # Fill in actual matrices and links for non-fixed parameters
  non_fixed_params <- setdiff(param_names, names(fixed))

  for (param in non_fixed_params) {
    # Get TMB parameter position based on family
    tmb_pos <- param_positions[[param]]

    # Skip if not mapped
    if (is.null(tmb_pos) || is.na(tmb_pos)) next

    # Update matrix, link type and scale
    matrix_name <- paste0("X", tmb_pos)
    link_name <- paste0("link_type", tmb_pos)
    scale_name <- paste0("scale", tmb_pos)

    # If parameter exists in model_data, use it
    if (param %in% names(model_data$matrices)) {
      tmb_data[[matrix_name]] <- model_data$matrices[[param]]

      # If link exists, use it
      if (param %in% names(link_ints)) {
        tmb_data[[link_name]] <- link_ints[[param]]
      }

      # If link_scale exists for this parameter, use it
      if (param %in% names(link_scale_list)) {
        tmb_data[[scale_name]] <- link_scale_list[[param]]
      }
    }
  }

  return(tmb_data)
}


#' #' Fit Generalized Kumaraswamy Regression Models
#' #'
#' #' @description
#' #' Fits regression models using the Generalized Kumaraswamy (GKw) family of
#' #' distributions for response variables strictly bounded in the interval (0, 1).
#' #' The function allows modeling parameters from all seven submodels of the GKw
#' #' family as functions of predictors using appropriate link functions. Estimation
#' #' is performed using Maximum Likelihood via the TMB (Template Model Builder) package.
#' #' Requires the \code{Formula} and \code{TMB} packages.
#' #'
#' #' @param formula An object of class \code{\link[Formula]{Formula}} (or one that
#' #'   can be coerced to that class). It should be structured as
#' #'   \code{y ~ model_alpha | model_beta | model_gamma | model_delta | model_lambda},
#' #'   where \code{y} is the response variable and each \code{model_*} part specifies
#' #'   the linear predictor for the corresponding parameter (\eqn{\alpha}, \eqn{\beta},
#' #'   \eqn{\gamma}, \eqn{\delta}, \eqn{\lambda}). If a part is omitted or specified
#' #'   as \code{~ 1} or \code{.}, an intercept-only model is used for that parameter.
#' #'   See Details for parameter correspondence in subfamilies.
#' #' @param data A data frame containing the variables specified in the \code{formula}.
#' #' @param family A character string specifying the desired distribution family.
#' #'   Defaults to \code{"gkw"}. Supported families are:
#' #'   \itemize{
#' #'     \item \code{"gkw"}: Generalized Kumaraswamy (5 parameters: \eqn{\alpha, \beta, \gamma, \delta, \lambda})
#' #'     \item \code{"bkw"}: Beta-Kumaraswamy (4 parameters: \eqn{\alpha, \beta, \gamma, \delta}; \eqn{\lambda = 1} fixed)
#' #'     \item \code{"kkw"}: Kumaraswamy-Kumaraswamy (4 parameters: \eqn{\alpha, \beta, \delta, \lambda}; \eqn{\gamma = 1} fixed)
#' #'     \item \code{"ekw"}: Exponentiated Kumaraswamy (3 parameters: \eqn{\alpha, \beta, \lambda}; \eqn{\gamma = 1, \delta = 0} fixed)
#' #'     \item \code{"mc"}: McDonald / Beta Power (3 parameters: \eqn{\gamma, \delta, \lambda}; \eqn{\alpha = 1, \beta = 1} fixed)
#' #'     \item \code{"kw"}: Kumaraswamy (2 parameters: \eqn{\alpha, \beta}; \eqn{\gamma = 1, \delta = 0, \lambda = 1} fixed)
#' #'     \item \code{"beta"}: Beta distribution (2 parameters: \eqn{\gamma, \delta}; \eqn{\alpha = 1, \beta = 1, \lambda = 1} fixed)
#' #'   }
#' #' @param link Either a single character string specifying the same link function
#' #'   for all relevant parameters, or a named list specifying the link function for each
#' #'   modeled parameter (e.g., \code{list(alpha = "log", beta = "log", delta = "logit")}).
#' #'   Defaults are \code{"log"} for \eqn{\alpha, \beta, \gamma, \lambda} (parameters > 0)
#' #'   and \code{"logit"} for \eqn{\delta} (parameter in (0, 1)).
#' #'   Supported link functions are:
#' #'   \itemize{
#' #'     \item \code{"log"}: logarithmic link, maps \eqn{(0, \infty)} to \eqn{(-\infty, \infty)}
#' #'     \item \code{"identity"}: identity link, no transformation
#' #'     \item \code{"inverse"}: inverse link, maps \eqn{x} to \eqn{1/x}
#' #'     \item \code{"sqrt"}: square root link, maps \eqn{x} to \eqn{\sqrt{x}}
#' #'     \item \code{"inverse-square"}: inverse squared link, maps \eqn{x} to \eqn{1/x^2}
#' #'     \item \code{"logit"}: logistic link, maps \eqn{(0, 1)} to \eqn{(-\infty, \infty)}
#' #'     \item \code{"probit"}: probit link, using normal CDF
#' #'     \item \code{"cloglog"}: complementary log-log
#' #'     \item \code{"cauchy"}: Cauchy link, using Cauchy CDF
#' #'   }
#' #' @param link_scale Either a single numeric value specifying the same scale for all
#' #'   link functions, or a named list specifying the scale for each parameter's link
#' #'   function (e.g., \code{list(alpha = 10, beta = 5, delta = 1)}). The scale affects
#' #'   how the link function transforms the linear predictor. Default is 10 for most
#' #'   parameters and 1 for parameters using probability-type links (such as \code{delta}).
#' #'   For probability-type links (logit, probit, cloglog, cauchy), smaller values
#' #'   produce more extreme transformations.
#' #' @param start An optional named list providing initial values for the regression
#' #'   coefficients. Parameter names should match the distribution parameters (alpha,
#' #'   beta, etc.), and values should be vectors corresponding to the coefficients
#' #'   in the respective linear predictors (including intercept). If \code{NULL}
#' #'   (default), suitable starting values are automatically determined based on
#' #'   global parameter estimates.
#' #' @param fixed An optional named list specifying parameters or coefficients to be
#' #'   held fixed at specific values during estimation. Currently not fully implemented.
#' #' @param method Character string specifying the optimization algorithm to use.
#' #'   Options are \code{"nlminb"} (default, using \code{\link[stats]{nlminb}}),
#' #'   \code{"BFGS"}, \code{"Nelder-Mead"}, \code{"CG"}, \code{"SANN"}, or \code{"L-BFGS-B"}.
#' #'   If \code{"nlminb"} is selected, R's \code{\link[stats]{nlminb}} function is used;
#' #'   otherwise, R's \code{\link[stats]{optim}} function is used with the specified method.
#' #' @param hessian Logical. If \code{TRUE} (default), the Hessian matrix is computed
#' #'   via \code{\link[TMB]{sdreport}} to obtain standard errors and the covariance
#' #'   matrix of the estimated coefficients. Setting to \code{FALSE} speeds up fitting
#' #'   but prevents calculation of standard errors and confidence intervals.
#' #' @param plot Logical. If \code{TRUE} (default), enables the generation of diagnostic
#' #'   plots when calling the generic \code{plot()} function on the fitted object.
#' #'   Actual plotting is handled by the \code{plot.gkwreg} method.
#' #' @param conf.level Numeric. The confidence level (1 - alpha) for constructing
#' #'   confidence intervals for the parameters. Default is 0.95. Used only if
#' #'   \code{hessian = TRUE}.
#' #' @param optimizer.control A list of control parameters passed directly to the
#' #'   chosen optimizer (\code{\link[stats]{nlminb}} or \code{\link[stats]{optim}}).
#' #'   See their respective documentation for details.
#' #' @param subset An optional vector specifying a subset of observations from \code{data}
#' #'   to be used in the fitting process.
#' #' @param weights An optional vector of prior weights (e.g., frequency weights)
#' #'   to be used in the fitting process. Should be \code{NULL} or a numeric vector
#' #'   of non-negative values.
#' #' @param offset An optional numeric vector or matrix specifying an a priori known
#' #'   component to be included *on the scale of the linear predictor* for each parameter.
#' #'   If a vector, it's applied to the predictor of the first parameter in the standard
#' #'   order (\eqn{\alpha}). If a matrix, columns must correspond to parameters in the
#' #'   order \eqn{\alpha, \beta, \gamma, \delta, \lambda}.
#' #' @param na.action A function which indicates what should happen when the data
#' #'   contain \code{NA}s. The default (\code{na.fail}) stops if \code{NA}s are
#' #'   present. Other options include \code{\link[stats]{na.omit}} or
#' #'   \code{\link[stats]{na.exclude}}.
#' #' @param contrasts An optional list specifying the contrasts to be used for factor
#' #'   variables in the model. See the \code{contrasts.arg} of
#' #'   \code{\link[stats]{model.matrix.default}}.
#' #' @param x Logical. If \code{TRUE}, the list of model matrices (one for each modeled
#' #'   parameter) is returned as component \code{x} of the fitted object. Default \code{FALSE}.
#' #' @param y Logical. If \code{TRUE} (default), the response variable (after processing
#' #'   by \code{na.action}, \code{subset}) is returned as component \code{y}.
#' #' @param model Logical. If \code{TRUE} (default), the model frame (containing all
#' #'   variables used from \code{data}) is returned as component \code{model}.
#' #' @param silent Logical. If \code{TRUE} (default), suppresses progress messages
#' #'   from TMB compilation and optimization. Set to \code{FALSE} for verbose output.
#' #' @param ... Additional arguments, currently unused or passed down to internal
#' #'   methods (potentially).
#' #'
#' #' @return An object of class \code{gkwreg}. This is a list containing the
#' #'   following components:
#' #'   \item{call}{The matched function call.}
#' #'   \item{family}{The specified distribution family string.}
#' #'   \item{formula}{The \code{\link[Formula]{Formula}} object used.}
#' #'   \item{coefficients}{A named vector of estimated regression coefficients.}
#' #'   \item{fitted.values}{Vector of estimated means (expected values) of the response.}
#' #'   \item{residuals}{Vector of response residuals (observed - fitted mean).}
#' #'   \item{fitted_parameters}{List containing the estimated mean value for each distribution parameter (\eqn{\alpha, \beta, \gamma, \delta, \lambda}).}
#' #'   \item{parameter_vectors}{List containing vectors of the estimated parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) for each observation, evaluated on the link scale.}
#' #'   \item{link}{List of link functions used for each parameter.}
#' #'   \item{param_names}{Character vector of names of the parameters modeled by the family.}
#' #'   \item{fixed_params}{Named list indicating which parameters are fixed by the family definition.}
#' #'   \item{loglik}{The maximized log-likelihood value.}
#' #'   \item{aic}{Akaike Information Criterion.}
#' #'   \item{bic}{Bayesian Information Criterion.}
#' #'   \item{deviance}{The deviance ( -2 * loglik ).}
#' #'   \item{df.residual}{Residual degrees of freedom (nobs - npar).}
#' #'   \item{nobs}{Number of observations used in the fit.}
#' #'   \item{npar}{Total number of estimated parameters (coefficients).}
#' #'   \item{vcov}{The variance-covariance matrix of the coefficients (if \code{hessian = TRUE}).}
#' #'   \item{se}{Standard errors of the coefficients (if \code{hessian = TRUE}).}
#' #'   \item{convergence}{Convergence code from the optimizer (0 typically indicates success).}
#' #'   \item{message}{Convergence message from the optimizer.}
#' #'   \item{iterations}{Number of iterations used by the optimizer.}
#' #'   \item{rmse}{Root Mean Squared Error of response residuals.}
#' #'   \item{efron_r2}{Efron's pseudo R-squared.}
#' #'   \item{mean_absolute_error}{Mean Absolute Error of response residuals.}
#' #'   \item{x}{List of model matrices (if \code{x = TRUE}).}
#' #'   \item{y}{The response vector (if \code{y = TRUE}).}
#' #'   \item{model}{The model frame (if \code{model = TRUE}).}
#' #'   \item{tmb_object}{The raw object returned by \code{\link[TMB]{MakeADFun}}.}
#' #'
#' #' @details
#' #' The \code{gkwreg} function provides a regression framework for the Generalized
#' #' Kumaraswamy (GKw) family and its submodels, extending density estimation
#' #' to include covariates. The response variable must be strictly bounded in the
#' #' (0, 1) interval.
#' #'
#' #' \strong{Model Specification:}
#' #' The extended \code{\link[Formula]{Formula}} syntax is crucial for specifying
#' #' potentially different linear predictors for each relevant distribution parameter.
#' #' The parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) correspond sequentially
#' #' to the parts of the formula separated by \code{|}. For subfamilies where some
#' #' parameters are fixed by definition (see \code{family} argument), the corresponding
#' #' parts of the formula are automatically ignored. For example, in a \code{family = "kw"}
#' #' model, only the first two parts (for \eqn{\alpha} and \eqn{\beta}) are relevant.
#' #'
#' #' \strong{Parameter Constraints and Link Functions:}
#' #' The parameters \eqn{\alpha, \beta, \gamma, \lambda} are constrained to be positive,
#' #' while \eqn{\delta} is constrained to the interval (0, 1). The default link functions
#' #' (\code{"log"} for positive parameters, \code{"logit"} for \eqn{\delta}) ensure these
#' #' constraints during estimation. Users can specify alternative link functions suitable
#' #' for the parameter's domain via the \code{link} argument.
#' #'
#' #' \strong{Link Scales:}
#' #' The \code{link_scale} parameter allows users to control how aggressively the link
#' #' function transforms the linear predictor. For probability-type links (logit, probit,
#' #' cloglog, cauchy), smaller values (e.g., 1) produce more extreme transformations,
#' #' while larger values (e.g., 10) produce more gradual transformations. For continuous
#' #' parameters, scale values control the sensitivity of the transformation.
#' #'
#' #' \strong{Families and Parameters:}
#' #' The function automatically handles parameter fixing based on the chosen \code{family}:
#' #' \itemize{
#' #'   \item \strong{GKw}: All 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) modeled.
#' #'   \item \strong{BKw}: Models \eqn{\alpha, \beta, \gamma, \delta}; fixes \eqn{\lambda = 1}.
#' #'   \item \strong{KKw}: Models \eqn{\alpha, \beta, \delta, \lambda}; fixes \eqn{\gamma = 1}.
#' #'   \item \strong{EKw}: Models \eqn{\alpha, \beta, \lambda}; fixes \eqn{\gamma = 1, \delta = 0}.
#' #'   \item \strong{Mc} (McDonald): Models \eqn{\gamma, \delta, \lambda}; fixes \eqn{\alpha = 1, \beta = 1}.
#' #'   \item \strong{Kw} (Kumaraswamy): Models \eqn{\alpha, \beta}; fixes \eqn{\gamma = 1, \delta = 0, \lambda = 1}.
#' #'   \item \strong{Beta}: Models \eqn{\gamma, \delta}; fixes \eqn{\alpha = 1, \beta = 1, \lambda = 1}. This parameterization corresponds to the standard Beta distribution with shape1 = \eqn{\gamma} and shape2 = \eqn{\delta}.
#' #' }
#' #'
#' #' \strong{Estimation Engine:}
#' #' Maximum Likelihood Estimation (MLE) is performed using C++ templates via the
#' #' \code{TMB} package, which provides automatic differentiation and efficient
#' #' optimization capabilities. The specific TMB template used depends on the chosen \code{family}.
#' #'
#' #' \strong{Optimizer Method (\code{method} argument):}
#' #' \itemize{
#' #'   \item \code{"nlminb"}: Uses R's built-in \code{stats::nlminb} optimizer. Good for problems with box constraints. Default option.
#' #'   \item \code{"Nelder-Mead"}: Uses R's \code{stats::optim} with the Nelder-Mead simplex algorithm, which doesn't require derivatives.
#' #'   \item \code{"BFGS"}: Uses R's \code{stats::optim} with the BFGS quasi-Newton method for unconstrained optimization.
#' #'   \item \code{"CG"}: Uses R's \code{stats::optim} with conjugate gradients method for unconstrained optimization.
#' #'   \item \code{"SANN"}: Uses R's \code{stats::optim} with simulated annealing, a global optimization method useful for problems with multiple local minima.
#' #'   \item \code{"L-BFGS-B"}: Uses R's \code{stats::optim} with the limited-memory BFGS method with box constraints.
#' #' }
#' #'
#' #' @examples
#' #' \donttest{
#' #'
#' #' ## -------------------------------------------------------------------------
#' #' ## 1. Real-world Case Studies
#' #' ## -------------------------------------------------------------------------
#' #'
#' #' ## Example 1: Food Expenditure Data
#' #' # Load required package
#' #' require(gkwdist)
#' #' require(gkwreg)
#' #'
#' #' # Get FoodExpenditure data and create response variable 'y' as proportion of income spent on food
#' #' data(FoodExpenditure)
#' #' food_data <- FoodExpenditure
#' #' food_data <- within(food_data, {
#' #'   y <- food / income
#' #' })
#' #'
#' #' # Define formula: y depends on 'persons' with 'income' as predictor for second parameter
#' #' formu_fe <- y ~ persons | income
#' #'
#' #' # Fit Kumaraswamy model with log link for both parameters
#' #' kw_model <- gkwreg(formu_fe, food_data,
#' #'   family = "kw",
#' #'   link = rep("log", 2), method = "nlminb"
#' #' )
#' #'
#' #' # Display model summary and diagnostics
#' #' summary(kw_model)
#' #' plot(kw_model, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#' #'
#' #' ## Example 2: Gasoline Yield Data
#' #' # Load GasolineYield dataset
#' #' data(GasolineYield)
#' #' gasoline_data <- GasolineYield
#' #'
#' #' # Formula: yield depends on batch and temperature
#' #' # First part (for alpha/gamma) includes batch and temp
#' #' # Second part (for beta/delta/phi) includes only temp
#' #' formu_gy <- yield ~ batch + temp | temp
#' #'
#' #' # Fit Kumaraswamy model with log link and BFGS optimization
#' #' kw_model_gas <- gkwreg(formu_gy, gasoline_data,
#' #'   family = "kw",
#' #'   link = rep("log", 2), method = "BFGS"
#' #' )
#' #'
#' #' # Display results
#' #' summary(kw_model_gas)
#' #' plot(kw_model_gas, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#' #'
#' #' ## -------------------------------------------------------------------------
#' #' ## 2. Simulation Studies
#' #' ## -------------------------------------------------------------------------
#' #'
#' #' ## Example 1: Simple Kumaraswamy Regression Model
#' #' # Set seed for reproducibility
#' #' set.seed(123)
#' #' n <- 1000
#' #' x1 <- runif(n, -2, 2)
#' #' x2 <- rnorm(n)
#' #'
#' #' # Define true regression coefficients
#' #' alpha_coef <- c(0.8, 0.3, -0.2) # Intercept, x1, x2
#' #' beta_coef <- c(1.2, -0.4, 0.1) # Intercept, x1, x2
#' #'
#' #' # Generate linear predictors and transform using exponential link
#' #' eta_alpha <- alpha_coef[1] + alpha_coef[2] * x1 + alpha_coef[3] * x2
#' #' eta_beta <- beta_coef[1] + beta_coef[2] * x1 + beta_coef[3] * x2
#' #' alpha_true <- exp(eta_alpha)
#' #' beta_true <- exp(eta_beta)
#' #'
#' #' # Generate responses from Kumaraswamy distribution
#' #' y <- rkw(n, alpha = alpha_true, beta = beta_true)
#' #' df1 <- data.frame(y = y, x1 = x1, x2 = x2)
#' #'
#' #' # Fit Kumaraswamy regression model with formula notation
#' #' # Model: alpha ~ x1 + x2 and beta ~ x1 + x2
#' #' kw_reg <- gkwreg(y ~ x1 + x2 | x1 + x2, data = df1, family = "kw", silent = TRUE)
#' #'
#' #' # Alternative model with custom link scales
#' #' kw_reg2 <- gkwreg(y ~ x1 + x2 | x1 + x2,
#' #'   data = df1, family = "kw",
#' #'   link_scale = list(alpha = 5, beta = 8), silent = TRUE
#' #' )
#' #'
#' #' # Display model summary
#' #' summary(kw_reg)
#' #'
#' #' ## Example 2: Generalized Kumaraswamy Regression
#' #' # Set seed for reproducibility
#' #' set.seed(456)
#' #' n <- 1000
#' #' x1 <- runif(n, -1, 1)
#' #' x2 <- rnorm(n)
#' #' x3 <- factor(rbinom(n, 1, 0.5), labels = c("A", "B")) # Factor variable
#' #'
#' #' # Define true regression coefficients for all parameters
#' #' alpha_coef <- c(0.5, 0.2) # Intercept, x1
#' #' beta_coef <- c(0.8, -0.3, 0.1) # Intercept, x1, x2
#' #' gamma_coef <- c(0.6, 0.4) # Intercept, x3B
#' #' delta_coef <- c(0.0, 0.2) # Intercept, x3B (logit scale)
#' #' lambda_coef <- c(-0.2, 0.1) # Intercept, x2
#' #'
#' #' # Create design matrices
#' #' X_alpha <- model.matrix(~x1, data = data.frame(x1 = x1))
#' #' X_beta <- model.matrix(~ x1 + x2, data = data.frame(x1 = x1, x2 = x2))
#' #' X_gamma <- model.matrix(~x3, data = data.frame(x3 = x3))
#' #' X_delta <- model.matrix(~x3, data = data.frame(x3 = x3))
#' #' X_lambda <- model.matrix(~x2, data = data.frame(x2 = x2))
#' #'
#' #' # Generate parameters through linear predictors and appropriate link functions
#' #' alpha <- exp(X_alpha %*% alpha_coef)
#' #' beta <- exp(X_beta %*% beta_coef)
#' #' gamma <- exp(X_gamma %*% gamma_coef)
#' #' delta <- plogis(X_delta %*% delta_coef) # logit link for delta
#' #' lambda <- exp(X_lambda %*% lambda_coef)
#' #'
#' #' # Generate response from Generalized Kumaraswamy distribution
#' #' y <- rgkw(n, alpha = alpha, beta = beta, gamma = gamma, delta = delta, lambda = lambda)
#' #' df2 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#' #'
#' #' # Fit GKw regression with parameter-specific formulas
#' #' gkw_reg <- gkwreg(y ~ x1 | x1 + x2 | x3 | x3 | x2, data = df2, family = "gkw")
#' #'
#' #' # Alternative model with custom link scales
#' #' gkw_reg2 <- gkwreg(y ~ x1 | x1 + x2 | x3 | x3 | x2,
#' #'   data = df2, family = "gkw",
#' #'   link_scale = list(
#' #'     alpha = 12, beta = 12, gamma = 12,
#' #'     delta = 0.8, lambda = 12
#' #'   )
#' #' )
#' #'
#' #' # Compare true vs. estimated coefficients
#' #' print("Estimated Coefficients (GKw):")
#' #' print(coef(gkw_reg))
#' #' print("True Coefficients (approx):")
#' #' print(list(
#' #'   alpha = alpha_coef, beta = beta_coef, gamma = gamma_coef,
#' #'   delta = delta_coef, lambda = lambda_coef
#' #' ))
#' #'
#' #' ## Example 3: Beta Regression for Comparison
#' #' # Set seed for reproducibility
#' #' set.seed(789)
#' #' n <- 1000
#' #' x1 <- runif(n, -1, 1)
#' #'
#' #' # True coefficients for Beta parameters (gamma = shape1, delta = shape2)
#' #' gamma_coef <- c(1.0, 0.5) # Intercept, x1 (log scale)
#' #' delta_coef <- c(1.5, -0.7) # Intercept, x1 (log scale)
#' #'
#' #' # Generate parameters through linear predictors and log link
#' #' X_beta_eg <- model.matrix(~x1, data.frame(x1 = x1))
#' #' gamma_true <- exp(X_beta_eg %*% gamma_coef)
#' #' delta_true <- exp(X_beta_eg %*% delta_coef)
#' #'
#' #' # Generate response from Beta distribution
#' #' y <- rbeta_(n, gamma_true, delta_true)
#' #' df_beta <- data.frame(y = y, x1 = x1)
#' #'
#' #' # Fit Beta regression model using gkwreg
#' #' beta_reg <- gkwreg(y ~ x1 | x1,
#' #'   data = df_beta, family = "beta",
#' #'   link = list(gamma = "log", delta = "log")
#' #' )
#' #'
#' #' ## Example 4: Model Comparison using AIC/BIC
#' #' # Fit an alternative model (Kumaraswamy) to the same beta-generated data
#' #' kw_reg2 <- try(gkwreg(y ~ x1 | x1, data = df_beta, family = "kw"))
#' #'
#' #' # Compare models using information criteria
#' #' print("AIC Comparison (Beta vs Kw):")
#' #' c(AIC(beta_reg), AIC(kw_reg2))
#' #' print("BIC Comparison (Beta vs Kw):")
#' #' c(BIC(beta_reg), BIC(kw_reg2))
#' #'
#' #' ## Example 5: Prediction with Fitted Models
#' #' # Create new data for predictions
#' #' newdata <- data.frame(x1 = seq(-1, 1, length.out = 20))
#' #'
#' #' # Predict expected response (mean of the Beta distribution)
#' #' pred_response <- predict(beta_reg, newdata = newdata, type = "response")
#' #'
#' #' # Predict parameters on the scale of the link function
#' #' pred_link <- predict(beta_reg, newdata = newdata, type = "link")
#' #'
#' #' # Predict parameters on the original scale
#' #' pred_params <- predict(beta_reg, newdata = newdata, type = "parameter")
#' #'
#' #' # Visualize fitted model and data
#' #' plot(df_beta$x1, df_beta$y,
#' #'   pch = 20, col = "grey", xlab = "x1", ylab = "y",
#' #'   main = "Beta Regression Fit (using gkwreg)"
#' #' )
#' #' lines(newdata$x1, pred_response, col = "red", lwd = 2)
#' #' legend("topright", legend = "Predicted Mean", col = "red", lty = 1, lwd = 2)
#' #' }
#' #'
#' #' @references
#' #' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
#' #' random processes. \emph{Journal of Hydrology}, \strong{46}(1-2), 79-88.
#' #'
#' #' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' #' \emph{Journal of Statistical Computation and Simulation}, \strong{81}(7), 883-898.
#' #'
#' #' Ferrari, S. L. P., & Cribari-Neto, F. (2004). Beta regression for modelling rates and
#' #' proportions. \emph{Journal of Applied Statistics}, \strong{31}(7), 799-815.
#' #'
#' #' Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M. (2016). TMB:
#' #' Automatic Differentiation and Laplace Approximation. \emph{Journal of Statistical
#' #' Software}, \strong{70}(5), 1-21.
#' #' (Underlying TMB package)
#' #'
#' #' Zeileis, A., Kleiber, C., Jackman, S. (2008). Regression Models for Count Data in R.
#' #' \emph{Journal of Statistical Software}, \strong{27}(8), 1-25.
#' #'
#' #'
#' #' Smithson, M., & Verkuilen, J. (2006). A Better Lemon Squeezer? Maximum-Likelihood
#' #' Regression with Beta-Distributed Dependent Variables. \emph{Psychological Methods},
#' #' \strong{11}(1), 54–71.
#' #'
#' #' @author Lopes, J. E.
#' #'
#' #' @seealso \code{\link{summary.gkwreg}}, \code{\link{predict.gkwreg}},
#' #'   \code{\link{plot.gkwreg}}, \code{\link{coef.gkwreg}}, \code{\link{vcov.gkwreg}},
#' #'   \code{\link[stats]{logLik}}, \code{\link[stats]{AIC}},
#' #'   \code{\link[Formula]{Formula}}, \code{\link[TMB]{MakeADFun}},
#' #'   \code{\link[TMB]{sdreport}}
#' #'
#' #' @keywords regression models
#' #' @author  Lopes, J. E.
#' #' @export
#' gkwreg <- function(formula,
#'                    data,
#'                    family = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"),
#'                    link = NULL,
#'                    link_scale = NULL,
#'                    start = NULL,
#'                    fixed = NULL,
#'                    method = c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B"),
#'                    hessian = TRUE,
#'                    plot = TRUE,
#'                    conf.level = 0.95,
#'                    optimizer.control = list(),
#'                    subset = NULL,
#'                    weights = NULL,
#'                    offset = NULL,
#'                    na.action = getOption("na.action"),
#'                    contrasts = NULL,
#'                    x = FALSE,
#'                    y = TRUE,
#'                    model = TRUE,
#'                    silent = TRUE,
#'                    ...) {
#'   # Match arguments
#'   family <- match.arg(family, choices = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))
#'   method <- match.arg(method, choices = c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B"))
#'
#'   # Determine if we're using nlminb or optim (with specified method)
#'   use_nlminb <- method == "nlminb"
#'
#'   call <- match.call()
#'
#'   # Load Formula package for multi-part formula
#'   if (!requireNamespace("Formula", quietly = TRUE)) {
#'     stop("The 'Formula' package is required for this function. Please install it.")
#'   }
#'
#'   # Load TMB package for model fitting
#'   if (!requireNamespace("TMB", quietly = TRUE)) {
#'     stop("The 'TMB' package is required for this function. Please install it.")
#'   }
#'
#'   # Get parameter information for the specified family
#'   param_info <- .get_family_param_info(family)
#'   param_names <- param_info$names
#'   fixed_params <- param_info$fixed
#'   param_positions <- param_info$positions
#'
#'   # Convert to Formula object
#'   formula_obj <- Formula::as.Formula(formula)
#'
#'   # Process Formula object to get individual formulas for each parameter
#'   formula_list <- .process_formula_parts(formula_obj, param_names, fixed_params, data)
#'
#'   # Process link functions
#'   link_list <- .process_link(link, param_names, fixed_params)
#'
#'   # Process link scales
#'   link_scale_list <- .process_link_scale(link_scale, link_list, param_names, fixed_params)
#'
#'   # Convert link strings to integers for TMB
#'   link_ints <- .convert_links_to_int(link_list)
#'
#'   # Extract model frames, responses, and model matrices
#'   model_data <- .extract_model_data(
#'     formula_list = formula_list,
#'     data = data,
#'     subset = subset,
#'     weights = weights,
#'     na.action = na.action,
#'     offset = offset,
#'     contrasts = contrasts,
#'     original_call = call # Pass the original call for correct scoping
#'   )
#'
#'   # Validate response variable is in (0, 1)
#'   y_var <- model_data$y
#'   invisible(.validate_data(y_var, length(param_names)))
#'
#'   # Initialize result list
#'   result <- list(
#'     call = call,
#'     family = family,
#'     formula = formula,
#'     link = link_list,
#'     link_scale = link_scale_list,
#'     param_names = param_names,
#'     fixed_params = fixed_params
#'   )
#'
#'   # Process fixed parameters
#'   fixed_processed <- .process_fixed(fixed, param_names, fixed_params)
#'
#'   # Prepare TMB data with correct matrices based on family
#'   tmb_data <- .prepare_tmb_data(
#'     model_data, family, param_names, fixed_processed,
#'     link_ints, link_scale_list, y_var, param_positions
#'   )
#'
#'   # Prepare TMB parameters in the correct structure required by each family
#'   tmb_params <- .prepare_tmb_params(
#'     model_data, family, param_names, fixed_processed,
#'     param_positions
#'   )
#'
#'   # Compile and load the appropriate TMB model based on the family
#'   if (family == "beta") {
#'     dll_name <- "gkwbetareg"
#'   } else {
#'     dll_name <- paste0(family, "reg")
#'   }
#'
#'   if (!silent) {
#'     message("Using TMB model: ", dll_name)
#'   }
#'
#'   # Use the existing function to check and compile the TMB model
#'   .check_and_compile_TMB_code(dll_name, verbose = !silent)
#'
#'   # Create TMB object
#'   obj <- TMB::MakeADFun(
#'     data = tmb_data,
#'     parameters = tmb_params,
#'     DLL = dll_name,
#'     silent = silent
#'   )
#'
#'   # Set up optimizer control parameters
#'   if (use_nlminb) {
#'     default_control <- list(eval.max = 500, iter.max = 300, trace = ifelse(silent, 0, 1))
#'   } else { # optim methods
#'     default_control <- list(maxit = 500, trace = ifelse(silent, 0, 1))
#'   }
#'
#'   opt_control <- utils::modifyList(default_control, optimizer.control)
#'
#'   # Optimize the model
#'   if (use_nlminb) {
#'     if (!silent) message("Optimizing with nlminb...")
#'     opt <- tryCatch(
#'       stats::nlminb(
#'         start = obj$par,
#'         objective = obj$fn,
#'         gradient = obj$gr,
#'         control = opt_control
#'       ),
#'       error = function(e) {
#'         stop("Optimization with nlminb failed: ", e$message)
#'       }
#'     )
#'     fit_result <- list(
#'       coefficients = obj$env$last.par,
#'       loglik = -opt$objective,
#'       convergence = opt$convergence == 0,
#'       message = opt$message,
#'       iterations = opt$iterations
#'     )
#'   } else { # optim methods
#'     if (!silent) message(paste("Optimizing with optim method:", method, "..."))
#'     opt <- tryCatch(
#'       stats::optim(
#'         par = obj$par,
#'         fn = obj$fn,
#'         gr = obj$gr,
#'         method = method,
#'         control = opt_control
#'       ),
#'       error = function(e) {
#'         stop(paste("Optimization with optim method", method, "failed:", e$message))
#'       }
#'     )
#'     fit_result <- list(
#'       coefficients = opt$par,
#'       loglik = -opt$value,
#'       convergence = opt$convergence == 0,
#'       message = if (opt$convergence == 0) "Successful convergence" else "Optimization failed",
#'       iterations = opt$counts[1]
#'     )
#'   }
#'
#'   # Format coefficient names with parameter mappings
#'   coef_names <- .format_coefficient_names(param_names, model_data, param_positions)
#'   names(fit_result$coefficients) <- coef_names
#'
#'   # Calculate standard errors and covariance matrix if requested
#'   if (hessian) {
#'     if (!silent) message("Computing standard errors...")
#'     tryCatch(
#'       {
#'         sd_report <- TMB::sdreport(obj, getJointPrecision = TRUE)
#'         fit_result$se <- as.vector(sd_report$sd)
#'         fit_result$vcov <- as.matrix(sd_report$cov)
#'
#'         # Add parameter names to standard errors
#'         names(fit_result$se) <- coef_names
#'
#'         # Add confidence intervals
#'         alpha <- 1 - conf.level
#'         z_value <- stats::qnorm(1 - alpha / 2)
#'         fit_result$ci_lower <- fit_result$coefficients - z_value * fit_result$se
#'         fit_result$ci_upper <- fit_result$coefficients + z_value * fit_result$se
#'       },
#'       error = function(e) {
#'         warning("Could not compute standard errors: ", e$message)
#'         fit_result$se <- rep(NA, length(fit_result$coefficients))
#'         fit_result$vcov <- matrix(NA, length(fit_result$coefficients), length(fit_result$coefficients))
#'         fit_result$ci_lower <- fit_result$ci_upper <- rep(NA, length(fit_result$coefficients))
#'         names(fit_result$se) <- coef_names
#'       }
#'     )
#'   }
#'
#'   # Extract TMB report
#'   tmb_report <- obj$report()
#'
#'   # Extract parameter means
#'   alpha_mean <- tmb_report$alpha_mean
#'   beta_mean <- tmb_report$beta_mean
#'   gamma_mean <- tmb_report$gamma_mean
#'   delta_mean <- tmb_report$delta_mean
#'   lambda_mean <- tmb_report$lambda_mean
#'
#'   # Extract parameter vectors for each observation
#'   alphaVec <- if ("alphaVec" %in% names(tmb_report)) tmb_report$alphaVec else rep(alpha_mean, length(y_var))
#'   betaVec <- if ("betaVec" %in% names(tmb_report)) tmb_report$betaVec else rep(beta_mean, length(y_var))
#'   gammaVec <- if ("gammaVec" %in% names(tmb_report)) tmb_report$gammaVec else rep(gamma_mean, length(y_var))
#'   deltaVec <- if ("deltaVec" %in% names(tmb_report)) tmb_report$deltaVec else rep(delta_mean, length(y_var))
#'   lambdaVec <- if ("lambdaVec" %in% names(tmb_report)) tmb_report$lambdaVec else rep(lambda_mean, length(y_var))
#'
#'   # Extract fitted values
#'   fitted_values <- if ("fitted" %in% names(tmb_report)) tmb_report$fitted else rep(NA, length(y_var))
#'
#'   # Calculate residuals
#'   response_residuals <- y_var - fitted_values
#'
#'   # Calculate fit statistics
#'   n_obs <- length(y_var)
#'   npar <- length(fit_result$coefficients)
#'
#'   # Deviance, AIC, BIC
#'   nll <- -fit_result$loglik
#'   deviance <- 2.0 * nll
#'   aic <- deviance + 2.0 * npar
#'   bic <- deviance + log(n_obs) * npar
#'
#'   # Residual degrees of freedom
#'   df.residual <- n_obs - npar
#'
#'   # Additional statistics
#'   rmse <- sqrt(mean(response_residuals^2, na.rm = TRUE))
#'   sse <- sum((y_var - fitted_values)^2, na.rm = TRUE)
#'   sst <- sum((y_var - mean(y_var))^2, na.rm = TRUE)
#'   efron_r2 <- if (sst > 0) 1 - sse / sst else NA
#'   mean_absolute_error <- mean(abs(response_residuals), na.rm = TRUE)
#'
#'   # Build final result with all necessary components
#'   result <- list(
#'     call = call,
#'     family = family,
#'     formula = formula,
#'     coefficients = fit_result$coefficients,
#'     fitted.values = as.vector(fitted_values),
#'     residuals = as.vector(response_residuals),
#'     fitted_parameters = list(
#'       alpha = alpha_mean,
#'       beta = beta_mean,
#'       gamma = gamma_mean,
#'       delta = delta_mean,
#'       lambda = lambda_mean
#'     ),
#'     parameter_vectors = list(
#'       alphaVec = alphaVec,
#'       betaVec = betaVec,
#'       gammaVec = gammaVec,
#'       deltaVec = deltaVec,
#'       lambdaVec = lambdaVec
#'     ),
#'     link = link_list,
#'     link_scale = link_scale_list,
#'     param_names = param_names,
#'     fixed_params = fixed_params,
#'     loglik = fit_result$loglik,
#'     aic = aic,
#'     bic = bic,
#'     deviance = deviance,
#'     df.residual = df.residual,
#'     nobs = n_obs,
#'     npar = npar,
#'     vcov = if (hessian) fit_result$vcov else NULL,
#'     se = if (hessian) fit_result$se else NULL,
#'     convergence = fit_result$convergence,
#'     message = fit_result$message,
#'     iterations = fit_result$iterations,
#'     rmse = rmse,
#'     efron_r2 = efron_r2,
#'     mean_absolute_error = mean_absolute_error,
#'     method = method
#'   )
#'
#'   # Add extra information if requested
#'   if (x) result$x <- model_data$matrices
#'   if (y) result$y <- y_var
#'   if (model) result$model <- model_data$model
#'
#'   # Store TMB object
#'   result$tmb_object <- obj
#'
#'   # Set class for S3 methods
#'   class(result) <- "gkwreg"
#'
#'   # Return the final result
#'   return(result)
#' }
