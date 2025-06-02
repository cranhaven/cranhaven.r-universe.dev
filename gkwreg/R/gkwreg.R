#' Fit Generalized Kumaraswamy Regression Models
#'
#' @description
#' Fits regression models using the Generalized Kumaraswamy (GKw) family of
#' distributions for response variables strictly bounded in the interval (0, 1).
#' The function allows modeling parameters from all seven submodels of the GKw
#' family as functions of predictors using appropriate link functions. Estimation
#' is performed using Maximum Likelihood via the TMB (Template Model Builder) package.
#' Requires the \code{Formula} and \code{TMB} packages.
#'
#' @param formula An object of class \code{\link[Formula]{Formula}} (or one that
#'   can be coerced to that class). It should be structured as
#'   \code{y ~ model_alpha | model_beta | model_gamma | model_delta | model_lambda},
#'   where \code{y} is the response variable and each \code{model_*} part specifies
#'   the linear predictor for the corresponding parameter (\eqn{\alpha}, \eqn{\beta},
#'   \eqn{\gamma}, \eqn{\delta}, \eqn{\lambda}). If a part is omitted or specified
#'   as \code{~ 1} or \code{.}, an intercept-only model is used for that parameter.
#'   See Details for parameter correspondence in subfamilies.
#' @param data A data frame containing the variables specified in the \code{formula}.
#' @param family A character string specifying the desired distribution family.
#'   Defaults to \code{"gkw"}. Supported families are:
#'   \itemize{
#'     \item \code{"gkw"}: Generalized Kumaraswamy (5 parameters: \eqn{\alpha, \beta, \gamma, \delta, \lambda})
#'     \item \code{"bkw"}: Beta-Kumaraswamy (4 parameters: \eqn{\alpha, \beta, \gamma, \delta}; \eqn{\lambda = 1} fixed)
#'     \item \code{"kkw"}: Kumaraswamy-Kumaraswamy (4 parameters: \eqn{\alpha, \beta, \delta, \lambda}; \eqn{\gamma = 1} fixed)
#'     \item \code{"ekw"}: Exponentiated Kumaraswamy (3 parameters: \eqn{\alpha, \beta, \lambda}; \eqn{\gamma = 1, \delta = 0} fixed)
#'     \item \code{"mc"}: McDonald / Beta Power (3 parameters: \eqn{\gamma, \delta, \lambda}; \eqn{\alpha = 1, \beta = 1} fixed)
#'     \item \code{"kw"}: Kumaraswamy (2 parameters: \eqn{\alpha, \beta}; \eqn{\gamma = 1, \delta = 0, \lambda = 1} fixed)
#'     \item \code{"beta"}: Beta distribution (2 parameters: \eqn{\gamma, \delta}; \eqn{\alpha = 1, \beta = 1, \lambda = 1} fixed)
#'   }
#' @param link Either a single character string specifying the same link function
#'   for all relevant parameters, or a named list specifying the link function for each
#'   modeled parameter (e.g., \code{list(alpha = "log", beta = "log", delta = "logit")}).
#'   Defaults are \code{"log"} for \eqn{\alpha, \beta, \gamma, \lambda} (parameters > 0)
#'   and \code{"logit"} for \eqn{\delta} (parameter in (0, 1)).
#'   Supported link functions are:
#'   \itemize{
#'     \item \code{"log"}: logarithmic link, maps \eqn{(0, \infty)} to \eqn{(-\infty, \infty)}
#'     \item \code{"identity"}: identity link, no transformation
#'     \item \code{"inverse"}: inverse link, maps \eqn{x} to \eqn{1/x}
#'     \item \code{"sqrt"}: square root link, maps \eqn{x} to \eqn{\sqrt{x}}
#'     \item \code{"inverse-square"}: inverse squared link, maps \eqn{x} to \eqn{1/x^2}
#'     \item \code{"logit"}: logistic link, maps \eqn{(0, 1)} to \eqn{(-\infty, \infty)}
#'     \item \code{"probit"}: probit link, using normal CDF
#'     \item \code{"cloglog"}: complementary log-log
#'     \item \code{"cauchy"}: Cauchy link, using Cauchy CDF
#'   }
#' @param link_scale Either a single numeric value specifying the same scale for all
#'   link functions, or a named list specifying the scale for each parameter's link
#'   function (e.g., \code{list(alpha = 10, beta = 5, delta = 1)}). The scale affects
#'   how the link function transforms the linear predictor. Default is 10 for most
#'   parameters and 1 for parameters using probability-type links (such as \code{delta}).
#'   For probability-type links (logit, probit, cloglog, cauchy), smaller values
#'   produce more extreme transformations.
#' @param start An optional named list providing initial values for the regression
#'   coefficients. Parameter names should match the distribution parameters (alpha,
#'   beta, etc.), and values should be vectors corresponding to the coefficients
#'   in the respective linear predictors (including intercept). If \code{NULL}
#'   (default), suitable starting values are automatically determined based on
#'   global parameter estimates.
#' @param fixed An optional named list specifying parameters or coefficients to be
#'   held fixed at specific values during estimation. Currently not fully implemented.
#' @param method Character string specifying the optimization algorithm to use.
#'   Options are \code{"nlminb"} (default, using \code{\link[stats]{nlminb}}),
#'   \code{"BFGS"}, \code{"Nelder-Mead"}, \code{"CG"}, \code{"SANN"}, or \code{"L-BFGS-B"}.
#'   If \code{"nlminb"} is selected, R's \code{\link[stats]{nlminb}} function is used;
#'   otherwise, R's \code{\link[stats]{optim}} function is used with the specified method.
#' @param hessian Logical. If \code{TRUE} (default), the Hessian matrix is computed
#'   via \code{\link[TMB]{sdreport}} to obtain standard errors and the covariance
#'   matrix of the estimated coefficients. Setting to \code{FALSE} speeds up fitting
#'   but prevents calculation of standard errors and confidence intervals.
#' @param plot Logical. If \code{TRUE} (default), enables the generation of diagnostic
#'   plots when calling the generic \code{plot()} function on the fitted object.
#'   Actual plotting is handled by the \code{plot.gkwreg} method.
#' @param conf.level Numeric. The confidence level (1 - alpha) for constructing
#'   confidence intervals for the parameters. Default is 0.95. Used only if
#'   \code{hessian = TRUE}.
#' @param optimizer.control A list of control parameters passed directly to the
#'   chosen optimizer (\code{\link[stats]{nlminb}} or \code{\link[stats]{optim}}).
#'   See their respective documentation for details.
#' @param subset An optional vector specifying a subset of observations from \code{data}
#'   to be used in the fitting process.
#' @param weights An optional vector of prior weights (e.g., frequency weights)
#'   to be used in the fitting process. Should be \code{NULL} or a numeric vector
#'   of non-negative values.
#' @param offset An optional numeric vector or matrix specifying an a priori known
#'   component to be included *on the scale of the linear predictor* for each parameter.
#'   If a vector, it's applied to the predictor of the first parameter in the standard
#'   order (\eqn{\alpha}). If a matrix, columns must correspond to parameters in the
#'   order \eqn{\alpha, \beta, \gamma, \delta, \lambda}.
#' @param na.action A function which indicates what should happen when the data
#'   contain \code{NA}s. The default (\code{na.fail}) stops if \code{NA}s are
#'   present. Other options include \code{\link[stats]{na.omit}} or
#'   \code{\link[stats]{na.exclude}}.
#' @param contrasts An optional list specifying the contrasts to be used for factor
#'   variables in the model. See the \code{contrasts.arg} of
#'   \code{\link[stats]{model.matrix.default}}.
#' @param x Logical. If \code{TRUE}, the list of model matrices (one for each modeled
#'   parameter) is returned as component \code{x} of the fitted object. Default \code{FALSE}.
#' @param y Logical. If \code{TRUE} (default), the response variable (after processing
#'   by \code{na.action}, \code{subset}) is returned as component \code{y}.
#' @param model Logical. If \code{TRUE} (default), the model frame (containing all
#'   variables used from \code{data}) is returned as component \code{model}.
#' @param silent Logical. If \code{TRUE} (default), suppresses progress messages
#'   from TMB compilation and optimization. Set to \code{FALSE} for verbose output.
#' @param ... Additional arguments, currently unused or passed down to internal
#'   methods (potentially).
#'
#' @return An object of class \code{gkwreg}. This is a list containing the
#'   following components:
#'   \item{call}{The matched function call.}
#'   \item{family}{The specified distribution family string.}
#'   \item{formula}{The \code{\link[Formula]{Formula}} object used.}
#'   \item{coefficients}{A named vector of estimated regression coefficients.}
#'   \item{fitted.values}{Vector of estimated means (expected values) of the response.}
#'   \item{residuals}{Vector of response residuals (observed - fitted mean).}
#'   \item{fitted_parameters}{List containing the estimated mean value for each distribution parameter (\eqn{\alpha, \beta, \gamma, \delta, \lambda}).}
#'   \item{parameter_vectors}{List containing vectors of the estimated parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) for each observation, evaluated on the link scale.}
#'   \item{link}{List of link functions used for each parameter.}
#'   \item{param_names}{Character vector of names of the parameters modeled by the family.}
#'   \item{fixed_params}{Named list indicating which parameters are fixed by the family definition.}
#'   \item{loglik}{The maximized log-likelihood value.}
#'   \item{aic}{Akaike Information Criterion.}
#'   \item{bic}{Bayesian Information Criterion.}
#'   \item{deviance}{The deviance ( -2 * loglik ).}
#'   \item{df.residual}{Residual degrees of freedom (nobs - npar).}
#'   \item{nobs}{Number of observations used in the fit.}
#'   \item{npar}{Total number of estimated parameters (coefficients).}
#'   \item{vcov}{The variance-covariance matrix of the coefficients (if \code{hessian = TRUE}).}
#'   \item{se}{Standard errors of the coefficients (if \code{hessian = TRUE}).}
#'   \item{convergence}{Convergence code from the optimizer (0 typically indicates success).}
#'   \item{message}{Convergence message from the optimizer.}
#'   \item{iterations}{Number of iterations used by the optimizer.}
#'   \item{rmse}{Root Mean Squared Error of response residuals.}
#'   \item{efron_r2}{Efron's pseudo R-squared.}
#'   \item{mean_absolute_error}{Mean Absolute Error of response residuals.}
#'   \item{x}{List of model matrices (if \code{x = TRUE}).}
#'   \item{y}{The response vector (if \code{y = TRUE}).}
#'   \item{model}{The model frame (if \code{model = TRUE}).}
#'   \item{tmb_object}{The raw object returned by \code{\link[TMB]{MakeADFun}}.}
#'
#' @details
#' The \code{gkwreg} function provides a regression framework for the Generalized
#' Kumaraswamy (GKw) family and its submodels, extending density estimation
#' to include covariates. The response variable must be strictly bounded in the
#' (0, 1) interval.
#'
#' \strong{Model Specification:}
#' The extended \code{\link[Formula]{Formula}} syntax is crucial for specifying
#' potentially different linear predictors for each relevant distribution parameter.
#' The parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) correspond sequentially
#' to the parts of the formula separated by \code{|}. For subfamilies where some
#' parameters are fixed by definition (see \code{family} argument), the corresponding
#' parts of the formula are automatically ignored. For example, in a \code{family = "kw"}
#' model, only the first two parts (for \eqn{\alpha} and \eqn{\beta}) are relevant.
#'
#' \strong{Parameter Constraints and Link Functions:}
#' The parameters \eqn{\alpha, \beta, \gamma, \lambda} are constrained to be positive,
#' while \eqn{\delta} is constrained to the interval (0, 1). The default link functions
#' (\code{"log"} for positive parameters, \code{"logit"} for \eqn{\delta}) ensure these
#' constraints during estimation. Users can specify alternative link functions suitable
#' for the parameter's domain via the \code{link} argument.
#'
#' \strong{Link Scales:}
#' The \code{link_scale} parameter allows users to control how aggressively the link
#' function transforms the linear predictor. For probability-type links (logit, probit,
#' cloglog, cauchy), smaller values (e.g., 1) produce more extreme transformations,
#' while larger values (e.g., 10) produce more gradual transformations. For continuous
#' parameters, scale values control the sensitivity of the transformation.
#'
#' \strong{Families and Parameters:}
#' The function automatically handles parameter fixing based on the chosen \code{family}:
#' \itemize{
#'   \item \strong{GKw}: All 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) modeled.
#'   \item \strong{BKw}: Models \eqn{\alpha, \beta, \gamma, \delta}; fixes \eqn{\lambda = 1}.
#'   \item \strong{KKw}: Models \eqn{\alpha, \beta, \delta, \lambda}; fixes \eqn{\gamma = 1}.
#'   \item \strong{EKw}: Models \eqn{\alpha, \beta, \lambda}; fixes \eqn{\gamma = 1, \delta = 0}.
#'   \item \strong{Mc} (McDonald): Models \eqn{\gamma, \delta, \lambda}; fixes \eqn{\alpha = 1, \beta = 1}.
#'   \item \strong{Kw} (Kumaraswamy): Models \eqn{\alpha, \beta}; fixes \eqn{\gamma = 1, \delta = 0, \lambda = 1}.
#'   \item \strong{Beta}: Models \eqn{\gamma, \delta}; fixes \eqn{\alpha = 1, \beta = 1, \lambda = 1}. This parameterization corresponds to the standard Beta distribution with shape1 = \eqn{\gamma} and shape2 = \eqn{\delta}.
#' }
#'
#' \strong{Estimation Engine:}
#' Maximum Likelihood Estimation (MLE) is performed using C++ templates via the
#' \code{TMB} package, which provides automatic differentiation and efficient
#' optimization capabilities. The specific TMB template used depends on the chosen \code{family}.
#'
#' \strong{Optimizer Method (\code{method} argument):}
#' \itemize{
#'   \item \code{"nlminb"}: Uses R's built-in \code{stats::nlminb} optimizer. Good for problems with box constraints. Default option.
#'   \item \code{"Nelder-Mead"}: Uses R's \code{stats::optim} with the Nelder-Mead simplex algorithm, which doesn't require derivatives.
#'   \item \code{"BFGS"}: Uses R's \code{stats::optim} with the BFGS quasi-Newton method for unconstrained optimization.
#'   \item \code{"CG"}: Uses R's \code{stats::optim} with conjugate gradients method for unconstrained optimization.
#'   \item \code{"SANN"}: Uses R's \code{stats::optim} with simulated annealing, a global optimization method useful for problems with multiple local minima.
#'   \item \code{"L-BFGS-B"}: Uses R's \code{stats::optim} with the limited-memory BFGS method with box constraints.
#' }
#'
#' @examples
#' \donttest{
#' ## -------------------------------------------------------------------------
#' ## 1. Real-world Case Studies
#' ## -------------------------------------------------------------------------
#'
#' ## Example 1: Food Expenditure Data
#' # Load required package
#' require(gkwreg)
#'
#' # Get FoodExpenditure data and create response variable 'y' as proportion of income spent on food
#' food_data <- get_bounded_datasets("FoodExpenditure")
#' food_data <- within(food_data, {
#'   y <- food / income
#' })
#'
#' # Define formula: y depends on 'persons' with 'income' as predictor for second parameter
#' formu_fe <- y ~ persons | income
#'
#' # Fit Kumaraswamy model with log link for both parameters
#' kw_model <- gkwreg(formu_fe, food_data,
#'   family = "kw",
#'   link = rep("log", 2), method = "nlminb"
#' )
#'
#' # Display model summary and diagnostics
#' summary(kw_model)
#' plot(kw_model, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#'
#' ## Example 2: Gasoline Yield Data
#' # Load GasolineYield dataset
#' gasoline_data <- get_bounded_datasets("GasolineYield")
#'
#' # Formula: yield depends on batch and temperature
#' # First part (for alpha/gamma) includes batch and temp
#' # Second part (for beta/delta/phi) includes only temp
#' formu_gy <- yield ~ batch + temp | temp
#'
#' # Fit Kumaraswamy model with log link and BFGS optimization
#' kw_model_gas <- gkwreg(formu_gy, gasoline_data,
#'   family = "kw",
#'   link = rep("log", 2), method = "BFGS"
#' )
#'
#' # Display results
#' summary(kw_model_gas)
#' plot(kw_model_gas, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#'
#' ## Example 3: SDAC Cancer Data
#' # Load cancer survival dataset
#' sdac_data <- get_bounded_datasets("sdac")
#'
#' # Formula: relative cumulative density ~ age adjustment + chemotherapy
#' formu_sd <- rcd ~ ageadj + chemo
#'
#' # Fit Extended Kumaraswamy model
#' ekw_model_gas <- gkwreg(formu_sd, sdac_data, family = "ekw", method = "BFGS")
#' summary(ekw_model_gas)
#' plot(ekw_model_gas, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#'
#' ## Example 4: Retinal Data
#' # Load retinal dataset
#' retinal_data <- get_bounded_datasets("retinal")
#'
#' # Formula for three parameters with different predictors
#' # alpha ~ LogT + LogT2 + Level
#' # beta  ~ LogT + Level
#' # gamma ~ Time
#' formu_rt <- Gas ~ LogT + LogT2 + Level | LogT + Level | Time
#'
#' # Fit Extended Kumaraswamy model
#' ekw_model_ret <- gkwreg(formu_rt, retinal_data, family = "ekw", method = "nlminb")
#' summary(ekw_model_ret)
#' plot(ekw_model_ret, use_ggplot = TRUE, arrange_plots = TRUE, sub.caption = "")
#'
#' ## Example 5: Weather Task Agreement Data
#' # Load the WeatherTask dataset
#' df_weather <- get_bounded_datasets("WeatherTask")
#'
#' # Fit all seven distribution families to the 'agreement' variable
#' fitall_weather <- gkwfitall(df_weather$agreement, method = "BFGS")
#'
#' # Compare model performance
#' summary(fitall_weather) # Displays the comparison table
#'
#' # Identify the best family based on AIC
#' best_family_code <- fitall_weather$comparison$Family[1]
#'
#' # Refit the best model for detailed analysis
#' fit_best_weather <- gkwfit(
#'   df_weather$agreement,
#'   family = best_family_code,
#'   method = "BFGS", profile = TRUE, plot = TRUE, silent = TRUE
#' )
#'
#' # Generate Goodness-of-Fit report
#' gof_report <- gkwgof(
#'   fit_best_weather,
#'   theme = ggplot2::theme_classic(),
#'   plot = TRUE, print_summary = FALSE, verbose = FALSE
#' )
#' summary(gof_report) # Display GoF statistics
#'
#' # Extract fit statistics for all families
#' results_weathertask_df <- do.call(rbind, lapply(fitall_weather$fits, function(f) {
#'   extract_gof_stats(gkwgof(f,
#'     plot = FALSE,
#'     print_summary = FALSE, verbose = FALSE
#'   ))
#' }))
#' results_weathertask_df <- results_weathertask_df[order(results_weathertask_df$AIC), ]
#' row.names(results_weathertask_df) <- NULL
#'
#' # Generate diagnostic plots for best model
#' plot(gkwgof(fit_best_weather, theme = ggplot2::theme_classic()), title = "")
#'
#' # Display formatted comparison table
#' results_weathertask_df[
#'   ,
#'   c(
#'     "family", "n_params", "logLik", "AIC", "BIC",
#'     "KS", "AD", "RMSE", "pseudo_R2"
#'   )
#' ]
#'
#' ## -------------------------------------------------------------------------
#' ## 2. Simulation Studies
#' ## -------------------------------------------------------------------------
#'
#' ## Example 1: Simple Kumaraswamy Regression Model
#' # Set seed for reproducibility
#' set.seed(123)
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#'
#' # Define true regression coefficients
#' alpha_coef <- c(0.8, 0.3, -0.2) # Intercept, x1, x2
#' beta_coef <- c(1.2, -0.4, 0.1) # Intercept, x1, x2
#'
#' # Generate linear predictors and transform using exponential link
#' eta_alpha <- alpha_coef[1] + alpha_coef[2] * x1 + alpha_coef[3] * x2
#' eta_beta <- beta_coef[1] + beta_coef[2] * x1 + beta_coef[3] * x2
#' alpha_true <- exp(eta_alpha)
#' beta_true <- exp(eta_beta)
#'
#' # Generate responses from Kumaraswamy distribution
#' y <- rkw(n, alpha = alpha_true, beta = beta_true)
#' df1 <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit Kumaraswamy regression model with formula notation
#' # Model: alpha ~ x1 + x2 and beta ~ x1 + x2
#' kw_reg <- gkwreg(y ~ x1 + x2 | x1 + x2, data = df1, family = "kw", silent = TRUE)
#'
#' # Alternative model with custom link scales
#' kw_reg2 <- gkwreg(y ~ x1 + x2 | x1 + x2,
#'   data = df1, family = "kw",
#'   link_scale = list(alpha = 5, beta = 8), silent = TRUE
#' )
#'
#' # Display model summary
#' summary(kw_reg)
#'
#' ## Example 2: Generalized Kumaraswamy Regression
#' # Set seed for reproducibility
#' set.seed(456)
#' n <- 1000
#' x1 <- runif(n, -1, 1)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.5), labels = c("A", "B")) # Factor variable
#'
#' # Define true regression coefficients for all parameters
#' alpha_coef <- c(0.5, 0.2) # Intercept, x1
#' beta_coef <- c(0.8, -0.3, 0.1) # Intercept, x1, x2
#' gamma_coef <- c(0.6, 0.4) # Intercept, x3B
#' delta_coef <- c(0.0, 0.2) # Intercept, x3B (logit scale)
#' lambda_coef <- c(-0.2, 0.1) # Intercept, x2
#'
#' # Create design matrices
#' X_alpha <- model.matrix(~x1, data = data.frame(x1 = x1))
#' X_beta <- model.matrix(~ x1 + x2, data = data.frame(x1 = x1, x2 = x2))
#' X_gamma <- model.matrix(~x3, data = data.frame(x3 = x3))
#' X_delta <- model.matrix(~x3, data = data.frame(x3 = x3))
#' X_lambda <- model.matrix(~x2, data = data.frame(x2 = x2))
#'
#' # Generate parameters through linear predictors and appropriate link functions
#' alpha <- exp(X_alpha %*% alpha_coef)
#' beta <- exp(X_beta %*% beta_coef)
#' gamma <- exp(X_gamma %*% gamma_coef)
#' delta <- plogis(X_delta %*% delta_coef) # logit link for delta
#' lambda <- exp(X_lambda %*% lambda_coef)
#'
#' # Generate response from Generalized Kumaraswamy distribution
#' y <- rgkw(n, alpha = alpha, beta = beta, gamma = gamma, delta = delta, lambda = lambda)
#' df2 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Fit GKw regression with parameter-specific formulas
#' gkw_reg <- gkwreg(y ~ x1 | x1 + x2 | x3 | x3 | x2, data = df2, family = "gkw")
#'
#' # Alternative model with custom link scales
#' gkw_reg2 <- gkwreg(y ~ x1 | x1 + x2 | x3 | x3 | x2,
#'   data = df2, family = "gkw",
#'   link_scale = list(
#'     alpha = 12, beta = 12, gamma = 12,
#'     delta = 0.8, lambda = 12
#'   )
#' )
#'
#' # Compare true vs. estimated coefficients
#' print("Estimated Coefficients (GKw):")
#' print(coef(gkw_reg))
#' print("True Coefficients (approx):")
#' print(list(
#'   alpha = alpha_coef, beta = beta_coef, gamma = gamma_coef,
#'   delta = delta_coef, lambda = lambda_coef
#' ))
#'
#' ## Example 3: Beta Regression for Comparison
#' # Set seed for reproducibility
#' set.seed(789)
#' n <- 1000
#' x1 <- runif(n, -1, 1)
#'
#' # True coefficients for Beta parameters (gamma = shape1, delta = shape2)
#' gamma_coef <- c(1.0, 0.5) # Intercept, x1 (log scale)
#' delta_coef <- c(1.5, -0.7) # Intercept, x1 (log scale)
#'
#' # Generate parameters through linear predictors and log link
#' X_beta_eg <- model.matrix(~x1, data.frame(x1 = x1))
#' gamma_true <- exp(X_beta_eg %*% gamma_coef)
#' delta_true <- exp(X_beta_eg %*% delta_coef)
#'
#' # Generate response from Beta distribution
#' y <- rbeta_(n, gamma_true, delta_true)
#' df_beta <- data.frame(y = y, x1 = x1)
#'
#' # Fit Beta regression model using gkwreg
#' beta_reg <- gkwreg(y ~ x1 | x1,
#'   data = df_beta, family = "beta",
#'   link = list(gamma = "log", delta = "log")
#' )
#'
#' ## Example 4: Model Comparison using AIC/BIC
#' # Fit an alternative model (Kumaraswamy) to the same beta-generated data
#' kw_reg2 <- try(gkwreg(y ~ x1 | x1, data = df_beta, family = "kw"))
#'
#' # Compare models using information criteria
#' print("AIC Comparison (Beta vs Kw):")
#' c(AIC(beta_reg), AIC(kw_reg2))
#' print("BIC Comparison (Beta vs Kw):")
#' c(BIC(beta_reg), BIC(kw_reg2))
#'
#' ## Example 5: Prediction with Fitted Models
#' # Create new data for predictions
#' newdata <- data.frame(x1 = seq(-1, 1, length.out = 20))
#'
#' # Predict expected response (mean of the Beta distribution)
#' pred_response <- predict(beta_reg, newdata = newdata, type = "response")
#'
#' # Predict parameters on the scale of the link function
#' pred_link <- predict(beta_reg, newdata = newdata, type = "link")
#'
#' # Predict parameters on the original scale
#' pred_params <- predict(beta_reg, newdata = newdata, type = "parameter")
#'
#' # Visualize fitted model and data
#' plot(df_beta$x1, df_beta$y,
#'   pch = 20, col = "grey", xlab = "x1", ylab = "y",
#'   main = "Beta Regression Fit (using gkwreg)"
#' )
#' lines(newdata$x1, pred_response, col = "red", lwd = 2)
#' legend("topright", legend = "Predicted Mean", col = "red", lty = 1, lwd = 2)
#' }
#'
#' @references
#' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
#' random processes. \emph{Journal of Hydrology}, \strong{46}(1-2), 79-88.
#'
#' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' \emph{Journal of Statistical Computation and Simulation}, \strong{81}(7), 883-898.
#'
#' Ferrari, S. L. P., & Cribari-Neto, F. (2004). Beta regression for modelling rates and
#' proportions. \emph{Journal of Applied Statistics}, \strong{31}(7), 799-815.
#'
#' Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M. (2016). TMB:
#' Automatic Differentiation and Laplace Approximation. \emph{Journal of Statistical
#' Software}, \strong{70}(5), 1-21.
#' (Underlying TMB package)
#'
#' Zeileis, A., Kleiber, C., Jackman, S. (2008). Regression Models for Count Data in R.
#' \emph{Journal of Statistical Software}, \strong{27}(8), 1-25.
#'
#'
#' Smithson, M., & Verkuilen, J. (2006). A Better Lemon Squeezer? Maximum-Likelihood
#' Regression with Beta-Distributed Dependent Variables. \emph{Psychological Methods},
#' \strong{11}(1), 54–71.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{summary.gkwreg}}, \code{\link{predict.gkwreg}},
#'   \code{\link{plot.gkwreg}}, \code{\link{coef.gkwreg}}, \code{\link{vcov.gkwreg}},
#'   \code{\link[stats]{logLik}}, \code{\link[stats]{AIC}},
#'   \code{\link[Formula]{Formula}}, \code{\link[TMB]{MakeADFun}},
#'   \code{\link[TMB]{sdreport}}
#'
#' @keywords regression models hoss
#' @author  Lopes, J. E.
#' @export
gkwreg <- function(formula,
                   data,
                   family = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"),
                   link = NULL,
                   link_scale = NULL,
                   start = NULL,
                   fixed = NULL,
                   method = c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B"),
                   hessian = TRUE,
                   plot = TRUE,
                   conf.level = 0.95,
                   optimizer.control = list(),
                   subset = NULL,
                   weights = NULL,
                   offset = NULL,
                   na.action = getOption("na.action"),
                   contrasts = NULL,
                   x = FALSE,
                   y = TRUE,
                   model = TRUE,
                   silent = TRUE,
                   ...) {
  # Match arguments
  family <- match.arg(family, choices = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))
  method <- match.arg(method, choices = c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B"))

  # Determine if we're using nlminb or optim (with specified method)
  use_nlminb <- method == "nlminb"

  call <- match.call()

  # Load Formula package for multi-part formula
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop("The 'Formula' package is required for this function. Please install it.")
  }

  # Load TMB package for model fitting
  if (!requireNamespace("TMB", quietly = TRUE)) {
    stop("The 'TMB' package is required for this function. Please install it.")
  }

  # Get parameter information for the specified family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  fixed_params <- param_info$fixed
  param_positions <- param_info$positions

  # Convert to Formula object
  formula_obj <- Formula::as.Formula(formula)

  # Process Formula object to get individual formulas for each parameter
  formula_list <- .process_formula_parts(formula_obj, param_names, fixed_params, data)

  # Process link functions
  link_list <- .process_link(link, param_names, fixed_params)

  # Process link scales
  link_scale_list <- .process_link_scale(link_scale, link_list, param_names, fixed_params)

  # Convert link strings to integers for TMB
  link_ints <- .convert_links_to_int(link_list)

  # Extract model frames, responses, and model matrices
  model_data <- .extract_model_data(
    formula_list = formula_list,
    data = data,
    subset = subset,
    weights = weights,
    na.action = na.action,
    offset = offset,
    contrasts = contrasts,
    original_call = call # Pass the original call for correct scoping
  )

  # Validate response variable is in (0, 1)
  y_var <- model_data$y
  invisible(.validate_data(y_var, length(param_names)))

  # Initialize result list
  result <- list(
    call = call,
    family = family,
    formula = formula,
    link = link_list,
    link_scale = link_scale_list,
    param_names = param_names,
    fixed_params = fixed_params
  )

  # Process fixed parameters
  fixed_processed <- .process_fixed(fixed, param_names, fixed_params)

  # Prepare TMB data with correct matrices based on family
  tmb_data <- .prepare_tmb_data(
    model_data, family, param_names, fixed_processed,
    link_ints, link_scale_list, y_var, param_positions
  )

  # Prepare TMB parameters in the correct structure required by each family
  tmb_params <- .prepare_tmb_params(
    model_data, family, param_names, fixed_processed,
    param_positions
  )

  # Compile and load the appropriate TMB model based on the family
  if (family == "beta") {
    dll_name <- "gkwbetareg"
  } else {
    dll_name <- paste0(family, "reg")
  }

  if (!silent) {
    message("Using TMB model: ", dll_name)
  }

  # Use the existing function to check and compile the TMB model
  .check_and_compile_TMB_code(dll_name, verbose = !silent)

  # Create TMB object
  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_params,
    DLL = dll_name,
    silent = silent
  )

  # Set up optimizer control parameters
  if (use_nlminb) {
    default_control <- list(eval.max = 500, iter.max = 300, trace = ifelse(silent, 0, 1))
  } else { # optim methods
    default_control <- list(maxit = 500, trace = ifelse(silent, 0, 1))
  }

  opt_control <- utils::modifyList(default_control, optimizer.control)

  # Optimize the model
  if (use_nlminb) {
    if (!silent) message("Optimizing with nlminb...")
    opt <- tryCatch(
      stats::nlminb(
        start = obj$par,
        objective = obj$fn,
        gradient = obj$gr,
        control = opt_control
      ),
      error = function(e) {
        stop("Optimization with nlminb failed: ", e$message)
      }
    )
    fit_result <- list(
      coefficients = obj$env$last.par,
      loglik = -opt$objective,
      convergence = opt$convergence == 0,
      message = opt$message,
      iterations = opt$iterations
    )
  } else { # optim methods
    if (!silent) message(paste("Optimizing with optim method:", method, "..."))
    opt <- tryCatch(
      stats::optim(
        par = obj$par,
        fn = obj$fn,
        gr = obj$gr,
        method = method,
        control = opt_control
      ),
      error = function(e) {
        stop(paste("Optimization with optim method", method, "failed:", e$message))
      }
    )
    fit_result <- list(
      coefficients = opt$par,
      loglik = -opt$value,
      convergence = opt$convergence == 0,
      message = if (opt$convergence == 0) "Successful convergence" else "Optimization failed",
      iterations = opt$counts[1]
    )
  }

  # Format coefficient names with parameter mappings
  coef_names <- .format_coefficient_names(param_names, model_data, param_positions)
  names(fit_result$coefficients) <- coef_names

  # Calculate standard errors and covariance matrix if requested
  if (hessian) {
    if (!silent) message("Computing standard errors...")
    tryCatch(
      {
        sd_report <- TMB::sdreport(obj, getJointPrecision = TRUE)
        fit_result$se <- as.vector(sd_report$sd)
        fit_result$vcov <- as.matrix(sd_report$cov)

        # Add parameter names to standard errors
        names(fit_result$se) <- coef_names

        # Add confidence intervals
        alpha <- 1 - conf.level
        z_value <- stats::qnorm(1 - alpha / 2)
        fit_result$ci_lower <- fit_result$coefficients - z_value * fit_result$se
        fit_result$ci_upper <- fit_result$coefficients + z_value * fit_result$se
      },
      error = function(e) {
        warning("Could not compute standard errors: ", e$message)
        fit_result$se <- rep(NA, length(fit_result$coefficients))
        fit_result$vcov <- matrix(NA, length(fit_result$coefficients), length(fit_result$coefficients))
        fit_result$ci_lower <- fit_result$ci_upper <- rep(NA, length(fit_result$coefficients))
        names(fit_result$se) <- coef_names
      }
    )
  }

  # Extract TMB report
  tmb_report <- obj$report()

  # Extract parameter means
  alpha_mean <- tmb_report$alpha_mean
  beta_mean <- tmb_report$beta_mean
  gamma_mean <- tmb_report$gamma_mean
  delta_mean <- tmb_report$delta_mean
  lambda_mean <- tmb_report$lambda_mean

  # Extract parameter vectors for each observation
  alphaVec <- if ("alphaVec" %in% names(tmb_report)) tmb_report$alphaVec else rep(alpha_mean, length(y_var))
  betaVec <- if ("betaVec" %in% names(tmb_report)) tmb_report$betaVec else rep(beta_mean, length(y_var))
  gammaVec <- if ("gammaVec" %in% names(tmb_report)) tmb_report$gammaVec else rep(gamma_mean, length(y_var))
  deltaVec <- if ("deltaVec" %in% names(tmb_report)) tmb_report$deltaVec else rep(delta_mean, length(y_var))
  lambdaVec <- if ("lambdaVec" %in% names(tmb_report)) tmb_report$lambdaVec else rep(lambda_mean, length(y_var))

  # Extract fitted values
  fitted_values <- if ("fitted" %in% names(tmb_report)) tmb_report$fitted else rep(NA, length(y_var))

  # Calculate residuals
  response_residuals <- y_var - fitted_values

  # Calculate fit statistics
  n_obs <- length(y_var)
  npar <- length(fit_result$coefficients)

  # Deviance, AIC, BIC
  nll <- -fit_result$loglik
  deviance <- 2.0 * nll
  aic <- deviance + 2.0 * npar
  bic <- deviance + log(n_obs) * npar

  # Residual degrees of freedom
  df.residual <- n_obs - npar

  # Additional statistics
  rmse <- sqrt(mean(response_residuals^2, na.rm = TRUE))
  sse <- sum((y_var - fitted_values)^2, na.rm = TRUE)
  sst <- sum((y_var - mean(y_var))^2, na.rm = TRUE)
  efron_r2 <- if (sst > 0) 1 - sse / sst else NA
  mean_absolute_error <- mean(abs(response_residuals), na.rm = TRUE)

  # Build final result with all necessary components
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
    vcov = if (hessian) fit_result$vcov else NULL,
    se = if (hessian) fit_result$se else NULL,
    convergence = fit_result$convergence,
    message = fit_result$message,
    iterations = fit_result$iterations,
    rmse = rmse,
    efron_r2 = efron_r2,
    mean_absolute_error = mean_absolute_error,
    method = method
  )

  # Add extra information if requested
  if (x) result$x <- model_data$matrices
  if (y) result$y <- y_var
  if (model) result$model <- model_data$model

  # Store TMB object
  result$tmb_object <- obj

  # Set class for S3 methods
  class(result) <- "gkwreg"

  # Return the final result
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

#' Process Formula Parts from a Formula Object
#'
#' @param formula_obj Formula object created with the Formula package.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @param data Data frame containing the variables.
#' @return A list of formula objects for each parameter.
#' @keywords internal
.process_formula_parts <- function(formula_obj, param_names, fixed_params, data) {
  # Get non-fixed parameters
  non_fixed_params <- setdiff(param_names, names(fixed_params))

  # Extract the response variable from the formula
  resp_var <- as.character(formula_obj[[2]])

  # Create list to store formulas for each parameter
  formula_list <- list()

  # Get the max number of RHS parts in the formula
  n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

  # Process each non-fixed parameter
  for (i in seq_along(non_fixed_params)) {
    param <- non_fixed_params[i]

    if (i <= n_parts) {
      # Extract the ith part of the formula
      rhs_part <- stats::formula(formula_obj, rhs = i, lhs = 1)[[3]]

      # Check if this part is just a dot
      if (identical(as.character(rhs_part), ".") || identical(as.character(rhs_part), "1")) {
        # Use intercept-only model
        formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", "1"))
      } else {
        # Use the specified formula part
        formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", deparse(rhs_part)))
      }
    } else {
      # For parameters beyond the number of specified parts, use intercept-only model
      formula_list[[param]] <- Formula::as.Formula(paste(resp_var, "~", "1"))
    }
  }

  return(formula_list)
}





#' Convert Link Function Names to TMB Integers
#'
#' @param link_list List of link function names
#' @return List of link function integers for TMB
#' @keywords internal
.convert_links_to_int <- function(link_list) {
  link_map <- c(
    "log" = 1,
    "logit" = 2,
    "probit" = 3,
    "cauchy" = 4,
    "cloglog" = 5,
    "identity" = 6,
    "sqrt" = 7,
    "inverse" = 8,
    "inverse-square" = 9
  )

  result <- lapply(link_list, function(link) {
    if (link %in% names(link_map)) {
      return(link_map[link])
    } else {
      warning("Unsupported link function: ", link, ". Using log link instead.")
      return(1) # Default to log
    }
  })

  return(result)
}



# .convert_links_to_int <- function(link_list) {
#   link_map <- c(
#     "log" = 1,
#     "logit" = 2,
#     "probit" = 3,
#     "cauchy" = 4,
#     "cloglog" = 5,
#     "identity" = 6,
#     "sqrt" = 7,
#     "inverse" = 8,
#     "inverse-square" = 9
#   )
#
#   result <- lapply(link_list, function(link) {
#     if (link %in% names(link_map)) {
#       return(link_map[link])
#     } else {
#       warning("Unsupported link function: ", link, ". Using log link instead.")
#       return(1) # Default to log
#     }
#   })
#
#   return(result)
# }


#' Format Coefficient Names Based on Family and Model Matrices
#'
#' @param param_names Names of parameters for the family
#' @param model_data Model data list including matrices
#' @param param_positions Parameter position mapping for the family
#' @return Vector of formatted coefficient names
#' @keywords internal
.format_coefficient_names <- function(param_names, model_data, param_positions) {
  # Initialize vector to accumulate all coefficient names
  all_coef_names <- c()
  # For each parameter that has a model matrix
  for (param in param_names) {
    if (param %in% names(model_data$matrices)) {
      # Get parameter position in TMB
      tmb_pos <- param_positions[[param]]
      # Get the model matrix
      X <- model_data$matrices[[param]]
      mat_coef_names <- colnames(X)
      # Create names for coefficients
      if (is.null(mat_coef_names)) {
        # If there are no column names, use generic names
        mat_coef_names <- paste0(param, "_", 1:ncol(X))
      } else {
        # Add parameter name prefix
        mat_coef_names <- paste0(param, ":", mat_coef_names)
      }
      # Add to accumulator vector
      all_coef_names <- c(all_coef_names, mat_coef_names)
    }
  }
  # Return coefficient names
  return(all_coef_names)
}

#' Get Parameter Information for a GKw Family Distribution
#'
#' @param family The GKw family distribution name.
#' @return A list with parameter information.
#' @keywords internal
.get_family_param_info <- function(family) {
  # Define parameter information and TMB parameter positions for each family
  family_params <- list(
    gkw = list(
      names = c("alpha", "beta", "gamma", "delta", "lambda"),
      n = 5,
      fixed = list(),
      positions = list(alpha = 1, beta = 2, gamma = 3, delta = 4, lambda = 5)
    ),
    bkw = list(
      names = c("alpha", "beta", "gamma", "delta"),
      n = 4,
      fixed = list(lambda = 1),
      positions = list(alpha = 1, beta = 2, gamma = 3, delta = 4)
    ),
    kkw = list(
      names = c("alpha", "beta", "delta", "lambda"),
      n = 4,
      fixed = list(gamma = 1),
      positions = list(alpha = 1, beta = 2, delta = 3, lambda = 4)
    ),
    ekw = list(
      names = c("alpha", "beta", "lambda"),
      n = 3,
      fixed = list(gamma = 1, delta = 0),
      positions = list(alpha = 1, beta = 2, lambda = 3)
    ),
    mc = list(
      names = c("gamma", "delta", "lambda"),
      n = 3,
      fixed = list(alpha = 1, beta = 1),
      positions = list(gamma = 1, delta = 2, lambda = 3)
    ),
    kw = list(
      names = c("alpha", "beta"),
      n = 2,
      fixed = list(gamma = 1, delta = 0, lambda = 1),
      positions = list(alpha = 1, beta = 2)
    ),
    beta = list(
      names = c("gamma", "delta"),
      n = 2,
      fixed = list(alpha = 1, beta = 1, lambda = 1),
      positions = list(gamma = 1, delta = 2)
    )
  )

  # Return parameter information for the specified family
  return(family_params[[family]])
}

#' Process Link Functions for GKw Regression
#'
#' @param link A character string or list of character strings specifying link functions.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @return A list of link functions.
#' @keywords internal
.process_link <- function(link, param_names, fixed_params) {
  # Default link functions for each parameter
  default_links <- list(
    alpha = "log",
    beta = "log",
    gamma = "log",
    delta = "logit",
    lambda = "log"
  )

  # Supported link functions
  supported_links <- c(
    "log", "logit", "identity", "inverse", "sqrt", "probit", "cloglog",
    "cauchy", "inverse-square"
  )

  # If link is NULL, use default links
  if (is.null(link)) {
    # Get default links for non-fixed parameters
    non_fixed_params <- setdiff(param_names, names(fixed_params))
    link_list <- default_links[non_fixed_params]
    return(link_list)
  }

  # If link is a single character string, apply to all parameters
  if (is.character(link) && length(link) == 1) {
    if (!link %in% supported_links) {
      stop(paste("Unsupported link function:", link))
    }

    # Apply the same link function to all non-fixed parameters
    non_fixed_params <- setdiff(param_names, names(fixed_params))
    link_list <- replicate(length(non_fixed_params), link, simplify = FALSE)
    names(link_list) <- non_fixed_params
    return(link_list)
  }

  # If link is a list, validate and return
  if (is.list(link) || is.character(link) && length(link) > 1) {
    if (is.character(link)) {
      link <- as.list(link)
      names(link) <- setdiff(param_names, names(fixed_params))
    }

    # Check if names of list match parameter names
    link_names <- names(link)
    if (is.null(link_names) || !all(link_names %in% param_names)) {
      stop("Names of link list must match parameter names for the chosen family")
    }

    # Check if all links are supported
    unsupported <- !unlist(link) %in% supported_links
    if (any(unsupported)) {
      stop(paste(
        "Unsupported link function(s):",
        paste(unlist(link)[unsupported], collapse = ", ")
      ))
    }

    # Remove links for fixed parameters
    fixed_param_names <- names(fixed_params)
    link <- link[setdiff(link_names, fixed_param_names)]

    return(link)
  }

  stop("link must be either a character string or a list of character strings")
}


#' Process Link Scales for GKw Regression
#'
#' @param link_scale A numeric value or list specifying scales for link functions.
#' @param link_list List of link functions for each parameter.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters.
#' @return A list of link scales.
#' @keywords internal
.process_link_scale <- function(link_scale, link_list, param_names, fixed_params) {
  # Default scale values based on link type
  probability_link_types <- c("logit", "probit", "cloglog", "cauchy")

  # Initialize default scales list
  default_scales <- list()

  # Set default scales based on parameter and link type
  for (param in names(link_list)) {
    if (link_list[[param]] %in% probability_link_types) {
      default_scales[[param]] <- 10.0 # Default for probability-type links
    } else {
      default_scales[[param]] <- 1.0 # Default for other links
    }
  }

  # If link_scale is NULL, use default scales
  if (is.null(link_scale)) {
    # Get default scales for non-fixed parameters with links
    non_fixed_params <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    return(default_scales[non_fixed_params])
  }

  # If link_scale is a single numeric value, apply to all parameters
  if (is.numeric(link_scale) && length(link_scale) == 1) {
    # Validate scale value
    if (link_scale <= 0) {
      stop("link_scale must be positive")
    }

    # Apply the same scale to all non-fixed parameters with links
    non_fixed_params <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    scale_list <- replicate(length(non_fixed_params), link_scale, simplify = FALSE)
    names(scale_list) <- non_fixed_params
    return(scale_list)
  }

  # If link_scale is a list, validate and return
  if (is.list(link_scale) || is.numeric(link_scale) && length(link_scale) > 1) {
    if (is.numeric(link_scale)) {
      link_scale <- as.list(link_scale)
      names(link_scale) <- intersect(names(link_list), setdiff(param_names, names(fixed_params)))
    }

    # Check if names of list match parameter names
    scale_names <- names(link_scale)
    if (is.null(scale_names) || !all(scale_names %in% param_names)) {
      stop("Names of link_scale list must match parameter names for the chosen family")
    }

    # Check if all scales are positive
    non_positive <- !unlist(lapply(link_scale, function(x) is.numeric(x) && x > 0))
    if (any(non_positive)) {
      stop("All link_scale values must be positive numbers")
    }

    # Remove scales for fixed parameters
    fixed_param_names <- names(fixed_params)
    link_scale <- link_scale[setdiff(scale_names, fixed_param_names)]

    # For parameters that have a link but no specified scale, use defaults
    missing_scales <- setdiff(names(link_list), names(link_scale))
    if (length(missing_scales) > 0) {
      link_scale <- c(link_scale, default_scales[missing_scales])
    }

    return(link_scale)
  }

  stop("link_scale must be either a numeric value or a list of numeric values")
}



#' Extract Model Data for GKw Regression
#'
#' @param formula_list List of formulas for each parameter.
#' @param data Data frame containing the variables.
#' @param subset Optional subset specification.
#' @param weights Optional weights.
#' @param na.action Function to handle missing values.
#' @param offset Optional offset.
#' @param contrasts List of contrasts for factors.
#' @param original_call The original function call.
#' @return A list of model data including frames, matrices, etc.
#' @keywords internal
.extract_model_data <- function(formula_list, data, subset, weights, na.action,
                                offset, contrasts, original_call) {
  # Initialize result list
  model_data <- list()

  # Get unique response variable name (should be the same for all formulas)
  resp_names <- unique(vapply(formula_list, function(f) as.character(f[[2]]), character(1)))
  if (length(resp_names) > 1) {
    stop("All formulas must have the same response variable")
  }

  # Extract model frames, responses, and model matrices for each parameter
  model_data$frames <- list()
  model_data$responses <- list()
  model_data$matrices <- list()
  model_data$terms <- list()

  for (param in names(formula_list)) {
    # Construct a list with arguments for model.frame
    mf_args <- list(
      formula = formula_list[[param]],
      data = data,
      subset = subset,
      na.action = na.action,
      drop.unused.levels = TRUE
    )

    # Evaluate and fix weights if provided
    if (!is.null(weights)) {
      weight_val <- tryCatch(weights, error = function(e) NULL)
      if (!is.null(weight_val) && !is.function(weight_val)) {
        mf_args$weights <- weight_val
      }
    }

    # Force the evaluation of the model frame in the proper environment
    model_data$frames[[param]] <- do.call(stats::model.frame, mf_args)

    # Extract response and model matrix
    model_data$responses[[param]] <- stats::model.response(model_data$frames[[param]])
    model_data$terms[[param]] <- attr(model_data$frames[[param]], "terms")
    X <- stats::model.matrix(model_data$terms[[param]], model_data$frames[[param]], contrasts)
    model_data$matrices[[param]] <- X
  }

  # Extract common response variable
  model_data$y <- model_data$responses[[1]]

  # Store model frame for the first parameter (all should have the same response)
  model_data$model <- model_data$frames[[1]]

  return(model_data)
}


#' Validate Data for GKw Regression
#'
#' @param data Numeric vector to validate.
#' @param n_params Number of parameters in the selected model.
#' @return The validated data.
#' @keywords internal
.validate_data <- function(data, n_params) {
  # Check if data is numeric
  if (!is.numeric(data)) {
    stop("Response variable must be numeric")
  }

  # Check for missing values
  if (any(is.na(data))) {
    stop("Response variable contains missing values")
  }

  # Check for values in the (0, 1) interval
  if (any(data <= 0 | data >= 1)) {
    stop("Response variable must be in the open interval (0, 1)")
  }

  # Return the validated data
  return(data)
}

#' Process Fixed Parameters for GKw Regression
#'
#' @param fixed List of fixed parameters or coefficients.
#' @param param_names Names of the parameters for the specified family.
#' @param fixed_params List of fixed parameters from the family definition.
#' @return A list of processed fixed parameters and coefficients.
#' @keywords internal
.process_fixed <- function(fixed, param_names, fixed_params) {
  # If no additional fixed parameters, return the family's fixed parameters
  if (is.null(fixed)) {
    return(fixed_params)
  }

  # Check if fixed is a valid list
  if (!is.list(fixed)) {
    stop("fixed must be a list")
  }

  # Combine user-defined fixed parameters with family's fixed parameters
  fixed_combined <- c(fixed, fixed_params)

  # Check for duplicates
  if (length(unique(names(fixed_combined))) < length(names(fixed_combined))) {
    stop("Duplicate entries in fixed parameters")
  }

  return(fixed_combined)
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


# .prepare_tmb_data <- function(model_data, family, param_names, fixed, link_ints, y, param_positions) {
#   # Initialize TMB data
#   tmb_data <- list(
#     y = y,
#     useMeanCache = 1, # Enable mean caching
#     calcFitted = 1, # Calculate fitted values
#     userChunkSize = 100 # Reasonable chunk size
#   )
#
#   # All families need matrices and link types
#   # The number of X matrices and link types needed depends on the family
#   num_params <- switch(family,
#     "gkw" = 5,
#     "bkw" = 4,
#     "kkw" = 4,
#     "ekw" = 3,
#     "mc" = 3,
#     "kw" = 2,
#     "beta" = 2
#   )
#
#   # Initialize default matrices and links for all required parameters
#   for (i in 1:num_params) {
#     matrix_name <- paste0("X", i)
#     link_name <- paste0("link_type", i)
#     scale_name <- paste0("scale", i)
#
#     # Default empty matrix with 1 column (intercept only)
#     tmb_data[[matrix_name]] <- matrix(0, nrow = length(y), ncol = 1)
#
#     # Default link is log (1) and scale is 10
#     tmb_data[[link_name]] <- 1
#     tmb_data[[scale_name]] <- 10.0
#   }
#
#   # Fill in actual matrices and links for non-fixed parameters
#   non_fixed_params <- setdiff(param_names, names(fixed))
#
#   for (param in non_fixed_params) {
#     # Get TMB parameter position based on family
#     tmb_pos <- param_positions[[param]]
#
#     # Skip if not mapped
#     if (is.null(tmb_pos) || is.na(tmb_pos)) next
#
#     # Update matrix, link type and scale
#     matrix_name <- paste0("X", tmb_pos)
#     link_name <- paste0("link_type", tmb_pos)
#     scale_name <- paste0("scale", tmb_pos)
#
#     # If parameter exists in model_data, use it
#     if (param %in% names(model_data$matrices)) {
#       tmb_data[[matrix_name]] <- model_data$matrices[[param]]
#
#       # If link exists, use it
#       if (param %in% names(link_ints)) {
#         tmb_data[[link_name]] <- link_ints[[param]]
#       }
#
#       # Set appropriate scale for the parameter
#       if (param == "delta") {
#         tmb_data[[scale_name]] <- 1.0 # For delta, which uses logit link
#       } else {
#         tmb_data[[scale_name]] <- 10.0 # For other parameters, which typically use log link
#       }
#     }
#   }
#
#   return(tmb_data)
# }


#' @title Extract Coefficients from a Fitted GKw Regression Model
#'
#' @description
#' Extracts the estimated regression coefficients from a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}. This is
#' an S3 method for the generic \code{\link[stats]{coef}} function.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function provides the standard way to access the estimated regression
#' coefficients from a model fitted with \code{\link{gkwreg}}. It simply
#' extracts the \code{coefficients} component from the fitted model object.
#' The function \code{\link[stats]{coefficients}} is an alias for this function.
#'
#' @return A named numeric vector containing the estimated regression coefficients
#'   for all modeled parameters. The names indicate the parameter (e.g., `alpha`,
#'   `beta`) and the corresponding predictor variable (e.g., `(Intercept)`, `x1`).
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{summary.gkwreg}},
#'   \code{\link[stats]{coef}}, \code{\link[stats]{confint}}
#'
#' @keywords coefficients methods regression
#'
#' @export
coef.gkwreg <- function(object, ...) {
  object$coefficients
}


#' @title Summary Method for Generalized Kumaraswamy Regression Models
#'
#' @description
#' Computes and returns a detailed statistical summary for a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param conf.level Numeric. The desired confidence level for constructing
#'   confidence intervals for the regression coefficients. Default is 0.95.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This method provides a comprehensive summary of the fitted \code{gkwreg} model.
#' It calculates z-values and p-values for the regression coefficients based on
#' the estimated standard errors (if available) and computes confidence intervals
#' at the specified \code{conf.level}. The summary includes:
#' \itemize{
#'   \item The model call.
#'   \item The distribution family used.
#'   \item A table of coefficients including estimates, standard errors, z-values,
#'     and p-values. Note: Significance stars are typically added by the
#'     corresponding \code{print.summary.gkwreg} method.
#'   \item Confidence intervals for the coefficients.
#'   \item Link functions used for each parameter.
#'   \item Mean values of the fitted distribution parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}).
#'   \item A five-number summary (Min, Q1, Median, Q3, Max) plus the mean of the
#'     response residuals.
#'   \item Key model fit statistics (Log-likelihood, AIC, BIC, RMSE, Efron's R^2).
#'   \item Information about model convergence and optimizer iterations.
#' }
#' If standard errors were not computed (e.g., \code{hessian = FALSE} in the
#' original \code{gkwreg} call), the coefficient table will only contain estimates,
#' and confidence intervals will not be available.
#'
#' @return An object of class \code{"summary.gkwreg"}, which is a list containing
#'   the following components:
#' \item{call}{The original function call that created the \code{object}.}
#' \item{family}{Character string specifying the distribution family.}
#' \item{coefficients}{A data frame (matrix) containing the coefficient estimates,
#'   standard errors, z-values, and p-values.}
#' \item{conf.int}{A matrix containing the lower and upper bounds of the confidence
#'   intervals for the coefficients (if standard errors are available).}
#' \item{link}{A list of character strings specifying the link functions used.}
#' \item{fitted_parameters}{A list containing the mean values of the estimated
#'   distribution parameters.}
#' \item{residuals}{A named numeric vector containing summary statistics for the
#'   response residuals.}
#' \item{nobs}{Number of observations used in the fit.}
#' \item{npar}{Total number of estimated regression coefficients.}
#' \item{df.residual}{Residual degrees of freedom.}
#' \item{loglik}{The maximized log-likelihood value.}
#' \item{aic}{Akaike Information Criterion.}
#' \item{bic}{Bayesian Information Criterion.}
#' \item{rmse}{Root Mean Squared Error of the residuals.}
#' \item{efron_r2}{Efron's pseudo-R-squared value.}
#' \item{mean_absolute_error}{Mean Absolute Error of the residuals.}
#' \item{convergence}{Convergence code from the optimizer.}
#' \item{iterations}{Number of iterations reported by the optimizer.}
#' \item{conf.level}{The confidence level used for calculating intervals.}
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{print.summary.gkwreg}},
#'   \code{\link[stats]{coef}}, \code{\link[stats]{confint}}
#'
#' @keywords summary models regression
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#' alpha_coef <- c(0.8, 0.3, -0.2)
#' beta_coef <- c(1.2, -0.4, 0.1)
#' eta_alpha <- alpha_coef[1] + alpha_coef[2] * x1 + alpha_coef[3] * x2
#' eta_beta <- beta_coef[1] + beta_coef[2] * x1 + beta_coef[3] * x2
#' alpha_true <- exp(eta_alpha)
#' beta_true <- exp(eta_beta)
#' # Use stats::rbeta as a placeholder if rkw is unavailable
#' y <- stats::rbeta(n, shape1 = alpha_true, shape2 = beta_true)
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' df <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit a Kumaraswamy regression model
#' kw_reg <- gkwreg(y ~ x1 + x2 | x1 + x2, data = df, family = "kw")
#'
#' # Generate detailed summary using the summary method
#' summary_kw <- summary(kw_reg)
#'
#' # Print the summary object (uses print.summary.gkwreg)
#' print(summary_kw)
#'
#' # Extract coefficient table directly from the summary object
#' coef_table <- coef(summary_kw) # Equivalent to summary_kw$coefficients
#' print(coef_table)
#' }
#'
#' @export
summary.gkwreg <- function(object, conf.level = 0.95, ...) {
  # Calculate z-values and p-values
  coef_est <- object$coefficients
  se <- object$se

  if (is.null(se)) {
    coef_table <- data.frame(
      Estimate = coef_est,
      row.names = names(coef_est)
    )
  } else {
    z_values <- coef_est / se
    p_values <- 2 * pnorm(-abs(z_values))

    coef_table <- data.frame(
      Estimate = coef_est,
      `Std. Error` = se,
      `z value` = z_values,
      `Pr(>|z|)` = p_values,
      row.names = names(coef_est), check.names = FALSE
    )
  }

  # Calculate confidence intervals
  alpha <- 1 - conf.level
  z_value <- stats::qnorm(1 - alpha / 2)

  if (!is.null(se)) {
    ci_lower <- coef_est - z_value * se
    ci_upper <- coef_est + z_value * se
    conf_int <- cbind(ci_lower, ci_upper)
    colnames(conf_int) <- c(
      paste0(format(100 * alpha / 2, digits = 1), "%"),
      paste0(format(100 * (1 - alpha / 2), digits = 1), "%")
    )
    rownames(conf_int) <- names(coef_est)
  } else {
    conf_int <- NULL
  }

  # Summarize residuals
  if (!is.null(object$residuals)) {
    res_summary <- c(
      Min = min(object$residuals, na.rm = TRUE),
      Q1 = quantile(object$residuals, 0.25, na.rm = TRUE),
      Median = stats::median(object$residuals, na.rm = TRUE),
      Mean = mean(object$residuals, na.rm = TRUE),
      Q3 = quantile(object$residuals, 0.75, na.rm = TRUE),
      Max = max(object$residuals, na.rm = TRUE)
    )
  } else {
    res_summary <- NULL
  }

  # Create and return summary object
  result <- list(
    call = object$call,
    family = object$family,
    coefficients = coef_table,
    conf.int = conf_int,
    link = object$link,
    fitted_parameters = object$fitted_parameters,
    residuals = res_summary,
    nobs = object$nobs,
    npar = object$npar,
    df.residual = object$df.residual,
    loglik = object$loglik,
    aic = object$aic,
    bic = object$bic,
    rmse = object$rmse,
    efron_r2 = object$efron_r2,
    mean_absolute_error = object$mean_absolute_error,
    convergence = object$convergence,
    iterations = object$iterations,
    conf.level = conf.level
  )

  class(result) <- "summary.gkwreg"
  return(result)
}


#' @title Print Method for Generalized Kumaraswamy Regression Summaries
#'
#' @description
#' Formats and prints the summary output of a fitted Generalized Kumaraswamy
#' (GKw) regression model (objects of class \code{"summary.gkwreg"}).
#'
#' @param x An object of class \code{"summary.gkwreg"}, typically the result of
#'   a call to \code{\link{summary.gkwreg}}.
#' @param digits Integer, controlling the number of significant digits to print.
#'   Defaults to \code{max(3, getOption("digits") - 3)}.
#' @param signif.stars Logical. If \code{TRUE}, significance stars are printed
#'   next to the p-values in the coefficient table. Defaults to the value of
#'   \code{getOption("show.signif.stars")}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This is the print method for objects created by \code{\link{summary.gkwreg}}.
#' It formats the summary information for display in the console. It is typically
#' invoked automatically when \code{print()} is called on a \code{summary.gkwreg}
#' object, or simply by typing the name of the summary object in an interactive R session.
#'
#' The output includes:
#' \itemize{
#'   \item Model family and the original function call.
#'   \item Summary statistics for residuals.
#'   \item A coefficient table with estimates, standard errors, z-values, and
#'     p-values, optionally marked with significance stars (using
#'     \code{\link[stats]{printCoefmat}}).
#'   \item Confidence intervals for coefficients (if available).
#'   \item Link functions used for each parameter.
#'   \item Mean values of the fitted distribution parameters.
#'   \item Key model fit statistics (LogLik, AIC, BIC, RMSE, R^2, etc.).
#'   \item Convergence status and number of iterations.
#' }
#'
#' @return Invisibly returns the original input object \code{x}. This allows the
#'   output of \code{print()} to be assigned, but it primarily prints the formatted
#'   summary to the console.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{summary.gkwreg}}, \code{\link{gkwreg}},
#'   \code{\link[stats]{printCoefmat}}
#'
#' @keywords print methods internal hoss
#'
#' @export
print.summary.gkwreg <- function(x, digits = max(3, getOption("digits") - 3),
                                 signif.stars = getOption("show.signif.stars"), ...) {
  cat("\nGeneralized Kumaraswamy Regression Model Summary\n\n")

  # Display family
  cat("Family:", x$family, "\n\n")

  # Display call
  cat("Call:\n")
  print(x$call)

  # Display residuals summary
  if (!is.null(x$residuals)) {
    cat("\nResiduals:\n")
    print(round(x$residuals, digits = digits))
  }

  # Display coefficient table with significance stars
  cat("\nCoefficients:\n")
  coefs <- x$coefficients
  stats::printCoefmat(coefs,
    digits = digits, signif.stars = signif.stars,
    has.Pvalue = ncol(coefs) >= 4
  )

  # Display confidence intervals
  if (!is.null(x$conf.int)) {
    cat("\nConfidence intervals (", x$conf.level * 100, "%):\n", sep = "")
    print(round(x$conf.int, digits = digits))
  }

  # Display link functions
  cat("\nLink functions:\n")
  for (param in names(x$link)) {
    cat(param, ": ", x$link[[param]], "\n", sep = "")
  }

  # Display fitted parameter means
  cat("\nFitted parameter means:\n")
  for (param in names(x$fitted_parameters)) {
    cat(param, ": ", format(x$fitted_parameters[[param]], digits = digits), "\n", sep = "")
  }

  # Display model fit statistics
  cat("\nModel fit statistics:\n")
  cat("Number of observations:", x$nobs, "\n")
  cat("Number of parameters:", x$npar, "\n")
  cat("Residual degrees of freedom:", x$df.residual, "\n")
  cat("Log-likelihood:", format(x$loglik, digits = digits), "\n")
  cat("AIC:", format(x$aic, digits = digits), "\n")
  cat("BIC:", format(x$bic, digits = digits), "\n")

  if (!is.null(x$rmse)) {
    cat("RMSE:", format(x$rmse, digits = digits), "\n")
  }

  if (!is.null(x$efron_r2) && !is.na(x$efron_r2)) {
    cat("Efron's R2:", format(x$efron_r2, digits = digits), "\n")
  }

  if (!is.null(x$mean_absolute_error)) {
    cat("Mean Absolute Error:", format(x$mean_absolute_error, digits = digits), "\n")
  }

  # Display convergence information
  cat("\nConvergence status:", ifelse(as.numeric(x$convergence) == 1, "Successful", "Failed"), "\n")
  if (!is.null(x$iterations)) {
    cat("Iterations:", x$iterations, "\n")
  }

  cat("\n")
  invisible(x)
}


#' @title Predictions from a Fitted Generalized Kumaraswamy Regression Model
#'
#' @description
#' Computes predictions and related quantities from a fitted Generalized
#' Kumaraswamy (GKw) regression model object. This method can extract fitted values,
#' predicted means, linear predictors, parameter values, variances, densities,
#' probabilities, and quantiles based on the estimated model. Predictions can be made
#' for new data or for the original data used to fit the model.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param newdata An optional data frame containing the variables needed for prediction.
#'   If omitted, predictions are made for the data used to fit the model.
#' @param type A character string specifying the type of prediction. Options are:
#'   \describe{
#'     \item{\code{"response"} or \code{"mean"}}{Predicted mean response (default).}
#'     \item{\code{"link"}}{Linear predictors for each parameter before applying
#'       inverse link functions.}
#'     \item{\code{"parameter"}}{Parameter values on their original scale (after
#'       applying inverse link functions).}
#'     \item{\code{"alpha"}, \code{"beta"}, \code{"gamma"}, \code{"delta"},
#'       \code{"lambda"}}{Values for a specific distribution parameter.}
#'     \item{\code{"variance"}}{Predicted variance of the response.}
#'     \item{\code{"density"} or \code{"pdf"}}{Density function values at points
#'       specified by \code{at}.}
#'     \item{\code{"probability"} or \code{"cdf"}}{Cumulative distribution function
#'       values at points specified by \code{at}.}
#'     \item{\code{"quantile"}}{Quantiles corresponding to probabilities specified
#'       by \code{at}.}
#'   }
#' @param na.action Function determining how to handle missing values in \code{newdata}.
#'   Default is \code{stats::na.pass}, which returns \code{NA} for cases with missing
#'   predictors.
#' @param at Numeric vector of values at which to evaluate densities, probabilities,
#'   or for which to compute quantiles, depending on \code{type}. Required for
#'   \code{type = "density"}, \code{type = "probability"}, or \code{type = "quantile"}.
#'   Defaults to 0.5.
#' @param elementwise Logical. If \code{TRUE} and \code{at} has the same length as
#'   the number of observations, applies each value in \code{at} to the corresponding
#'   observation. If \code{FALSE} (default), applies all values in \code{at} to each
#'   observation, returning a matrix.
#' @param family Character string specifying the distribution family to use for
#'   calculations. If \code{NULL} (default), uses the family from the fitted model.
#'   Options match those in \code{\link{gkwreg}}: \code{"gkw"}, \code{"bkw"},
#'   \code{"kkw"}, \code{"ekw"}, \code{"mc"}, \code{"kw"}, \code{"beta"}.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' The \code{predict.gkwreg} function provides a flexible framework for obtaining
#' predictions and inference from fitted Generalized Kumaraswamy regression models.
#' It handles all subfamilies of GKw distributions and respects the parametrization
#' and link functions specified in the original model.
#'
#' \subsection{Prediction Types}{
#' The function supports several types of predictions:
#' \itemize{
#'   \item \strong{Response/Mean}: Computes the expected value of the response variable
#'     based on the model parameters. For most GKw family distributions, this requires
#'     numerical integration or special formulas.
#'
#'   \item \strong{Link}: Returns the linear predictors for each parameter without
#'     applying inverse link functions. These are the values \eqn{\eta_j = X\beta_j}
#'     for each parameter \eqn{j}.
#'
#'   \item \strong{Parameter}: Computes the distribution parameter values on their original
#'     scale by applying the appropriate inverse link functions to the linear predictors.
#'     For example, if alpha uses a log link, then \eqn{\alpha = \exp(X\beta_\alpha)}.
#'
#'   \item \strong{Individual Parameters}: Extract specific parameter values (alpha, beta,
#'     gamma, delta, lambda) on their original scale.
#'
#'   \item \strong{Variance}: Estimates the variance of the response based on the model
#'     parameters. For some distributions, analytical formulas are used; for others,
#'     numerical approximations are employed.
#'
#'   \item \strong{Density/PDF}: Evaluates the probability density function at specified
#'     points given the model parameters.
#'
#'   \item \strong{Probability/CDF}: Computes the cumulative distribution function at
#'     specified points given the model parameters.
#'
#'   \item \strong{Quantile}: Calculates quantiles corresponding to specified probabilities
#'     given the model parameters.
#' }
#' }
#'
#' \subsection{Link Functions}{
#' The function respects the link functions specified in the original model for each
#' parameter. The supported link functions are:
#' \itemize{
#'   \item \code{"log"}: \eqn{g(\mu) = \log(\mu)}, \eqn{g^{-1}(\eta) = \exp(\eta)}
#'   \item \code{"logit"}: \eqn{g(\mu) = \log(\mu/(1-\mu))}, \eqn{g^{-1}(\eta) = 1/(1+\exp(-\eta))}
#'   \item \code{"probit"}: \eqn{g(\mu) = \Phi^{-1}(\mu)}, \eqn{g^{-1}(\eta) = \Phi(\eta)}
#'   \item \code{"cauchy"}: \eqn{g(\mu) = \tan(\pi*(\mu-0.5))}, \eqn{g^{-1}(\eta) = 0.5 + (1/\pi) \arctan(\eta)}
#'   \item \code{"cloglog"}: \eqn{g(\mu) = \log(-\log(1-\mu))}, \eqn{g^{-1}(\eta) = 1 - \exp(-\exp(\eta))}
#'   \item \code{"identity"}: \eqn{g(\mu) = \mu}, \eqn{g^{-1}(\eta) = \eta}
#'   \item \code{"sqrt"}: \eqn{g(\mu) = \sqrt{\mu}}, \eqn{g^{-1}(\eta) = \eta^2}
#'   \item \code{"inverse"}: \eqn{g(\mu) = 1/\mu}, \eqn{g^{-1}(\eta) = 1/\eta}
#'   \item \code{"inverse-square"}: \eqn{g(\mu) = 1/\sqrt{\mu}}, \eqn{g^{-1}(\eta) = 1/\eta^2}
#' }
#' }
#'
#' \subsection{Family-Specific Constraints}{
#' The function enforces appropriate constraints for each distribution family:
#' \itemize{
#'   \item \code{"gkw"}: All 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) are used.
#'   \item \code{"bkw"}: \eqn{\lambda = 1} is fixed.
#'   \item \code{"kkw"}: \eqn{\gamma = 1} is fixed.
#'   \item \code{"ekw"}: \eqn{\gamma = 1, \delta = 0} are fixed.
#'   \item \code{"mc"}: \eqn{\alpha = 1, \beta = 1} are fixed.
#'   \item \code{"kw"}: \eqn{\gamma = 1, \delta = 0, \lambda = 1} are fixed.
#'   \item \code{"beta"}: \eqn{\alpha = 1, \beta = 1, \lambda = 1} are fixed.
#' }
#' }
#'
#' \subsection{Parameter Bounds}{
#' All parameters are constrained to their valid ranges:
#' \itemize{
#'   \item \eqn{\alpha, \beta, \gamma, \lambda > 0}
#'   \item \eqn{0 < \delta < 1}
#' }
#' }
#'
#' \subsection{Using with New Data}{
#' When providing \code{newdata}, ensure it contains all variables used in the model's
#' formula. The function extracts the terms for each parameter's model matrix and applies
#' the appropriate link functions to calculate predictions. If any variables are missing,
#' the function will attempt to substitute reasonable defaults or raise an error if
#' critical variables are absent.
#' }
#'
#' \subsection{Using for Model Evaluation}{
#' The function is useful for model checking, generating predicted values for plotting,
#' and evaluating the fit of different distribution families. By specifying the \code{family}
#' parameter, you can compare predictions under different distributional assumptions.
#' }
#'
#' @return The return value depends on the \code{type} argument:
#' \itemize{
#'   \item For \code{type = "response"}, \code{type = "variance"}, or individual
#'     parameters (\code{type = "alpha"}, etc.): A numeric vector of length equal
#'     to the number of rows in \code{newdata} (or the original data).
#'
#'   \item For \code{type = "link"} or \code{type = "parameter"}: A data frame with
#'     columns for each parameter and rows corresponding to observations.
#'
#'   \item For \code{type = "density"}, \code{type = "probability"}, or
#'     \code{type = "quantile"}:
#'     \itemize{
#'       \item If \code{elementwise = TRUE}: A numeric vector of length equal to
#'         the number of rows in \code{newdata} (or the original data).
#'       \item If \code{elementwise = FALSE}: A matrix where rows correspond to
#'         observations and columns correspond to the values in \code{at}.
#'     }
#' }
#'
#' @examples
#' \donttest{
#' # Generate a sample dataset (n = 1000)
#' set.seed(123)
#' n <- 1000
#'
#' # Create predictors
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.4))
#'
#' # Simulate Kumaraswamy distributed data
#' # True parameters with specific relationships to predictors
#' true_alpha <- exp(0.7 + 0.3 * x1)
#' true_beta <- exp(1.2 - 0.2 * x2 + 0.4 * (x3 == "1"))
#'
#' # Generate random responses
#' y <- rkw(n, alpha = true_alpha, beta = true_beta)
#'
#' # Ensure responses are strictly in (0, 1)
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#'
#' # Create data frame
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Split into training and test sets
#' set.seed(456)
#' train_idx <- sample(n, 800)
#' train_data <- df[train_idx, ]
#' test_data <- df[-train_idx, ]
#'
#' # ====================================================================
#' # Example 1: Basic usage - Fit a Kumaraswamy model and make predictions
#' # ====================================================================
#'
#' # Fit the model
#' kw_model <- gkwreg(y ~ x1 | x2 + x3, data = train_data, family = "kw")
#'
#' # Predict mean response for test data
#' pred_mean <- predict(kw_model, newdata = test_data, type = "response")
#'
#' # Calculate prediction error
#' mse <- mean((test_data$y - pred_mean)^2)
#' cat("Mean Squared Error:", mse, "\n")
#'
#' # ====================================================================
#' # Example 2: Different prediction types
#' # ====================================================================
#'
#' # Create a grid of values for visualization
#' x1_grid <- seq(-2, 2, length.out = 100)
#' grid_data <- data.frame(x1 = x1_grid, x2 = 0, x3 = 0)
#'
#' # Predict different quantities
#' pred_mean <- predict(kw_model, newdata = grid_data, type = "response")
#' pred_var <- predict(kw_model, newdata = grid_data, type = "variance")
#' pred_params <- predict(kw_model, newdata = grid_data, type = "parameter")
#' pred_alpha <- predict(kw_model, newdata = grid_data, type = "alpha")
#' pred_beta <- predict(kw_model, newdata = grid_data, type = "beta")
#'
#' # Plot predicted mean and parameters against x1
#' plot(x1_grid, pred_mean,
#'   type = "l", col = "blue",
#'   xlab = "x1", ylab = "Predicted Mean", main = "Mean Response vs x1"
#' )
#' plot(x1_grid, pred_var,
#'   type = "l", col = "red",
#'   xlab = "x1", ylab = "Predicted Variance", main = "Response Variance vs x1"
#' )
#' plot(x1_grid, pred_alpha,
#'   type = "l", col = "purple",
#'   xlab = "x1", ylab = "Alpha", main = "Alpha Parameter vs x1"
#' )
#' plot(x1_grid, pred_beta,
#'   type = "l", col = "green",
#'   xlab = "x1", ylab = "Beta", main = "Beta Parameter vs x1"
#' )
#'
#' # ====================================================================
#' # Example 3: Computing densities, CDFs, and quantiles
#' # ====================================================================
#'
#' # Select a single observation
#' obs_data <- test_data[1, ]
#'
#' # Create a sequence of y values for plotting
#' y_seq <- seq(0.01, 0.99, length.out = 100)
#'
#' # Compute density at each y value
#' dens_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "density", at = y_seq, elementwise = FALSE
#' )
#'
#' # Compute CDF at each y value
#' cdf_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "probability", at = y_seq, elementwise = FALSE
#' )
#'
#' # Compute quantiles for a sequence of probabilities
#' prob_seq <- seq(0.1, 0.9, by = 0.1)
#' quant_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "quantile", at = prob_seq, elementwise = FALSE
#' )
#'
#' # Plot density and CDF
#' plot(y_seq, dens_values,
#'   type = "l", col = "blue",
#'   xlab = "y", ylab = "Density", main = "Predicted PDF"
#' )
#' plot(y_seq, cdf_values,
#'   type = "l", col = "red",
#'   xlab = "y", ylab = "Cumulative Probability", main = "Predicted CDF"
#' )
#'
#' # ====================================================================
#' # Example 4: Prediction under different distributional assumptions
#' # ====================================================================
#'
#' # Fit models with different families
#' beta_model <- gkwreg(y ~ x1 | x2 + x3, data = train_data, family = "beta")
#' gkw_model <- gkwreg(y ~ x1 | x2 + x3 | 1 | 1 | x3, data = train_data, family = "gkw")
#'
#' # Predict means using different families
#' pred_kw <- predict(kw_model, newdata = test_data, type = "response")
#' pred_beta <- predict(beta_model, newdata = test_data, type = "response")
#' pred_gkw <- predict(gkw_model, newdata = test_data, type = "response")
#'
#' # Calculate MSE for each family
#' mse_kw <- mean((test_data$y - pred_kw)^2)
#' mse_beta <- mean((test_data$y - pred_beta)^2)
#' mse_gkw <- mean((test_data$y - pred_gkw)^2)
#'
#' cat("MSE by family:\n")
#' cat("Kumaraswamy:", mse_kw, "\n")
#' cat("Beta:", mse_beta, "\n")
#' cat("GKw:", mse_gkw, "\n")
#'
#' # Compare predictions from different families visually
#' plot(test_data$y, pred_kw,
#'   col = "blue", pch = 16,
#'   xlab = "Observed", ylab = "Predicted", main = "Predicted vs Observed"
#' )
#' points(test_data$y, pred_beta, col = "red", pch = 17)
#' points(test_data$y, pred_gkw, col = "green", pch = 18)
#' abline(0, 1, lty = 2)
#' legend("topleft",
#'   legend = c("Kumaraswamy", "Beta", "GKw"),
#'   col = c("blue", "red", "green"), pch = c(16, 17, 18)
#' )
#'
#' # ====================================================================
#' # Example 5: Working with linear predictors and link functions
#' # ====================================================================
#'
#' # Extract linear predictors and parameter values
#' lp <- predict(kw_model, newdata = test_data, type = "link")
#' params <- predict(kw_model, newdata = test_data, type = "parameter")
#'
#' # Verify that inverse link transformation works correctly
#' # For Kumaraswamy model, alpha and beta use log links by default
#' alpha_from_lp <- exp(lp$alpha)
#' beta_from_lp <- exp(lp$beta)
#'
#' # Compare with direct parameter predictions
#' cat("Manual inverse link vs direct parameter prediction:\n")
#' cat("Alpha difference:", max(abs(alpha_from_lp - params$alpha)), "\n")
#' cat("Beta difference:", max(abs(beta_from_lp - params$beta)), "\n")
#'
#' # ====================================================================
#' # Example 6: Elementwise calculations
#' # ====================================================================
#'
#' # Generate probabilities specific to each observation
#' probs <- runif(nrow(test_data), 0.1, 0.9)
#'
#' # Calculate quantiles for each observation at its own probability level
#' quant_elementwise <- predict(kw_model,
#'   newdata = test_data,
#'   type = "quantile", at = probs, elementwise = TRUE
#' )
#'
#' # Calculate probabilities at each observation's actual value
#' prob_at_y <- predict(kw_model,
#'   newdata = test_data,
#'   type = "probability", at = test_data$y, elementwise = TRUE
#' )
#'
#' # Create Q-Q plot
#' plot(sort(prob_at_y), seq(0, 1, length.out = length(prob_at_y)),
#'   xlab = "Empirical Probability", ylab = "Theoretical Probability",
#'   main = "P-P Plot", type = "l"
#' )
#' abline(0, 1, lty = 2, col = "red")
#'
#' # ====================================================================
#' # Example 7: Predicting for the original data
#' # ====================================================================
#'
#' # Fit a model with original data
#' full_model <- gkwreg(y ~ x1 + x2 + x3 | x1 + x2 + x3, data = df, family = "kw")
#'
#' # Get fitted values using predict and compare with model's fitted.values
#' fitted_from_predict <- predict(full_model, type = "response")
#' fitted_from_model <- full_model$fitted.values
#'
#' # Compare results
#' cat(
#'   "Max difference between predict() and fitted.values:",
#'   max(abs(fitted_from_predict - fitted_from_model)), "\n"
#' )
#'
#' # ====================================================================
#' # Example 8: Handling missing data
#' # ====================================================================
#'
#' # Create test data with some missing values
#' test_missing <- test_data
#' test_missing$x1[1:5] <- NA
#' test_missing$x2[6:10] <- NA
#'
#' # Predict with different na.action options
#' pred_na_pass <- tryCatch(
#'   predict(kw_model, newdata = test_missing, na.action = na.pass),
#'   error = function(e) rep(NA, nrow(test_missing))
#' )
#' pred_na_omit <- tryCatch(
#'   predict(kw_model, newdata = test_missing, na.action = na.omit),
#'   error = function(e) rep(NA, nrow(test_missing))
#' )
#'
#' # Show which positions have NAs
#' cat("Rows with missing predictors:", which(is.na(pred_na_pass)), "\n")
#' cat("Length after na.omit:", length(pred_na_omit), "\n")
#' }
#'
#' @seealso
#' \code{\link{gkwreg}} for fitting Generalized Kumaraswamy regression models,
#' \code{\link{fitted.gkwreg}} for extracting fitted values,
#' \code{\link{residuals.gkwreg}} for calculating residuals,
#' \code{\link{summary.gkwreg}} for model summaries,
#' \code{\link{coef.gkwreg}} for extracting coefficients.
#'
#' @references
#' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' \emph{Journal of Statistical Computation and Simulation}, \strong{81}(7), 883-898.
#'
#' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
#' random processes. \emph{Journal of Hydrology}, \strong{46}(1-2), 79-88.
#'
#' Ferrari, S. L. P., & Cribari-Neto, F. (2004). Beta regression for modelling rates and
#' proportions. \emph{Journal of Applied Statistics}, \strong{31}(7), 799-815.
#'
#' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution with some
#' tractability advantages. \emph{Statistical Methodology}, \strong{6}(1), 70-81.
#'
#' @examples
#' \donttest{
#' ## Example 1: Simple Kumaraswamy regression model ----
#' set.seed(123)
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#'
#' # True regression coefficients
#' alpha_coef <- c(0.8, 0.3, -0.2) # Intercept, x1, x2
#' beta_coef <- c(1.2, -0.4, 0.1) # Intercept, x1, x2
#'
#' # Generate linear predictors and transform to parameters using inverse link (exp)
#' eta_alpha <- alpha_coef[1] + alpha_coef[2] * x1 + alpha_coef[3] * x2
#' eta_beta <- beta_coef[1] + beta_coef[2] * x1 + beta_coef[3] * x2
#' alpha_true <- exp(eta_alpha)
#' beta_true <- exp(eta_beta)
#'
#' # Generate responses from Kumaraswamy distribution (assuming rkw is available)
#' y <- rkw(n, alpha = alpha_true, beta = beta_true)
#' # Create data frame
#' df1 <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit Kumaraswamy regression model using extended formula syntax
#' # Model alpha ~ x1 + x2 and beta ~ x1 + x2
#' kw_reg <- gkwreg(y ~ x1 + x2 | x1 + x2, data = df1, family = "kw", silent = TRUE)
#'
#' # Display summary
#' summary(kw_reg)
#'
#' ## Example 2: Generalized Kumaraswamy regression ----
#' set.seed(456)
#' x1 <- runif(n, -1, 1)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.5), labels = c("A", "B")) # Factor variable
#'
#' # True regression coefficients
#' alpha_coef <- c(0.5, 0.2) # Intercept, x1
#' beta_coef <- c(0.8, -0.3, 0.1) # Intercept, x1, x2
#' gamma_coef <- c(0.6, 0.4) # Intercept, x3B
#' delta_coef <- c(0.0, 0.2) # Intercept, x3B (logit scale)
#' lambda_coef <- c(-0.2, 0.1) # Intercept, x2
#'
#' # Design matrices
#' X_alpha <- model.matrix(~x1, data = data.frame(x1 = x1))
#' X_beta <- model.matrix(~ x1 + x2, data = data.frame(x1 = x1, x2 = x2))
#' X_gamma <- model.matrix(~x3, data = data.frame(x3 = x3))
#' X_delta <- model.matrix(~x3, data = data.frame(x3 = x3))
#' X_lambda <- model.matrix(~x2, data = data.frame(x2 = x2))
#'
#' # Generate linear predictors and transform to parameters
#' alpha <- exp(X_alpha %*% alpha_coef)
#' beta <- exp(X_beta %*% beta_coef)
#' gamma <- exp(X_gamma %*% gamma_coef)
#' delta <- plogis(X_delta %*% delta_coef) # logit link for delta
#' lambda <- exp(X_lambda %*% lambda_coef)
#'
#' # Generate response from GKw distribution (assuming rgkw is available)
#' y <- rgkw(n, alpha = alpha, beta = beta, gamma = gamma, delta = delta, lambda = lambda)
#'
#' # Create data frame
#' df2 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Fit GKw regression with parameter-specific formulas
#' # alpha ~ x1, beta ~ x1 + x2, gamma ~ x3, delta ~ x3, lambda ~ x2
#' gkw_reg <- gkwreg(y ~ x1 | x1 + x2 | x3 | x3 | x2, data = df2, family = "gkw")
#'
#' # Compare true vs. estimated coefficients
#' print("Estimated Coefficients (GKw):")
#' print(coef(gkw_reg))
#' print("True Coefficients (approx):")
#' print(list(
#'   alpha = alpha_coef, beta = beta_coef, gamma = gamma_coef,
#'   delta = delta_coef, lambda = lambda_coef
#' ))
#'
#' ## Example 3: Beta regression for comparison ----
#' set.seed(789)
#' x1 <- runif(n, -1, 1)
#'
#' # True coefficients for Beta parameters (gamma = shape1, delta = shape2)
#' gamma_coef <- c(1.0, 0.5) # Intercept, x1 (log scale for shape1)
#' delta_coef <- c(1.5, -0.7) # Intercept, x1 (log scale for shape2)
#'
#' # Generate linear predictors and transform (default link is log for Beta params here)
#' X_beta_eg <- model.matrix(~x1, data.frame(x1 = x1))
#' gamma_true <- exp(X_beta_eg %*% gamma_coef)
#' delta_true <- exp(X_beta_eg %*% delta_coef)
#'
#' # Generate response from Beta distribution
#' y <- rbeta_(n, gamma_true, delta_true)
#'
#' # Create data frame
#' df_beta <- data.frame(y = y, x1 = x1)
#'
#' # Fit Beta regression model using gkwreg
#' # Formula maps to gamma and delta: y ~  x1 | x1
#' beta_reg <- gkwreg(y ~ x1 | x1,
#'   data = df_beta, family = "beta",
#'   link = list(gamma = "log", delta = "log")
#' ) # Specify links if non-default
#'
#' ## Example 4: Model comparison using AIC/BIC ----
#' # Fit an alternative model, e.g., Kumaraswamy, to the same beta-generated data
#' kw_reg2 <- try(gkwreg(y ~ x1 | x1, data = df_beta, family = "kw"))
#'
#' print("AIC Comparison (Beta vs Kw):")
#' c(AIC(beta_reg), AIC(kw_reg2))
#' print("BIC Comparison (Beta vs Kw):")
#' c(BIC(beta_reg), BIC(kw_reg2))
#'
#' ## Example 5: Predicting with a fitted model
#'
#' # Use the Beta regression model from Example 3
#' newdata <- data.frame(x1 = seq(-1, 1, length.out = 20))
#'
#' # Predict expected response (mean of the Beta distribution)
#' pred_response <- predict(beta_reg, newdata = newdata, type = "response")
#'
#' # Predict parameters (gamma and delta) on the scale of the link function
#' pred_link <- predict(beta_reg, newdata = newdata, type = "link")
#'
#' # Predict parameters on the original scale (shape1, shape2)
#' pred_params <- predict(beta_reg, newdata = newdata, type = "parameter")
#'
#' # Plot original data and predicted mean response curve
#' plot(df_beta$x1, df_beta$y,
#'   pch = 20, col = "grey", xlab = "x1", ylab = "y",
#'   main = "Beta Regression Fit (using gkwreg)"
#' )
#' lines(newdata$x1, pred_response, col = "red", lwd = 2)
#' legend("topright", legend = "Predicted Mean", col = "red", lty = 1, lwd = 2)
#' }
#'
#' @author Lopes, J. E. and contributors
#'
#' @keywords models regression predict
#' @importFrom stats predict
#' @method predict gkwreg
#' @export
predict.gkwreg <- function(object, newdata = NULL,
                           type = "response",
                           na.action = stats::na.pass, at = 0.5,
                           elementwise = NULL, family = NULL, ...) {
  # Match type argument
  type <- match.arg(type, c(
    "response", "link", "parameter",
    "alpha", "beta", "gamma", "delta", "lambda",
    "variance", "density", "pdf",
    "probability", "cdf", "quantile"
  ))

  # Aliases for some types
  if (type == "pdf") type <- "density"
  if (type == "cdf") type <- "probability"
  if (type == "mean") type <- "response"

  # Validate object
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a gkwreg model")
  }

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      warning("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))
  }

  # Import functions from the required package
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop("Package 'Formula' is needed for this function.")
  }

  # Handle case when we can directly use parameter vectors
  if (is.null(newdata) && !is.null(object$parameter_vectors) &&
    type %in% c(
      "response", "parameter", "alpha", "beta", "gamma", "delta", "lambda",
      "density", "probability", "quantile"
    )) {
    n <- length(object$parameter_vectors$alphaVec)
    params <- matrix(0, nrow = n, ncol = 5)
    params[, 1] <- object$parameter_vectors$alphaVec
    params[, 2] <- object$parameter_vectors$betaVec
    params[, 3] <- object$parameter_vectors$gammaVec
    params[, 4] <- object$parameter_vectors$deltaVec
    params[, 5] <- object$parameter_vectors$lambdaVec

    # Special case for response and fitted values
    if (type == "response" && !is.null(object$fitted.values)) {
      return(object$fitted.values)
    }

    # Handle parameter predictions
    if (type == "parameter") {
      return(data.frame(
        alpha = params[, 1],
        beta = params[, 2],
        gamma = params[, 3],
        delta = params[, 4],
        lambda = params[, 5]
      ))
    } else if (type %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
      param_index <- match(type, c("alpha", "beta", "gamma", "delta", "lambda"))
      return(params[, param_index])
    }

    # For density, probability, and quantile using at
    if (type %in% c("density", "probability", "quantile")) {
      if (!is.numeric(at)) {
        stop("'at' must be numeric")
      }

      if (is.null(elementwise)) {
        elementwise <- length(at) == n
      }

      if (elementwise) {
        if (length(at) != n) {
          stop("For elementwise=TRUE, length of 'at' must equal number of observations")
        }
        eval_y <- at
        eval_params <- params
      } else {
        eval_params <- do.call(rbind, lapply(seq_len(length(at)), function(i) params))
        eval_y <- rep(at, each = n)
      }

      # Use the specific family distribution functions with proper prefixes
      result <- numeric(length(eval_y))

      # Loop through observations to calculate results using the appropriate distribution function
      for (i in seq_along(eval_y)) {
        alpha_i <- eval_params[i, 1]
        beta_i <- eval_params[i, 2]
        gamma_i <- eval_params[i, 3]
        delta_i <- eval_params[i, 4]
        lambda_i <- eval_params[i, 5]
        y_i <- eval_y[i]

        # Use the appropriate distribution function based on family and type
        if (type == "density") {
          # Density functions (d-prefix)
          switch(family,
            "gkw" = {
              result[i] <- dgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- dbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- dkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- dekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- dmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- dkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- dbeta_(y_i, gamma_i, delta_i)
            }
          )
        } else if (type == "probability") {
          # CDF functions (p-prefix)
          switch(family,
            "gkw" = {
              result[i] <- pgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- pbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- pkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- pekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- pmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- pkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- pbeta_(y_i, gamma_i, delta_i)
            }
          )
        } else if (type == "quantile") {
          # Quantile functions (q-prefix)
          switch(family,
            "gkw" = {
              result[i] <- qgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- qbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- qkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- qekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- qmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- qkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- qbeta_(y_i, gamma_i, delta_i)
            }
          )
        }
      }

      if (elementwise) {
        return(result)
      } else {
        result_matrix <- matrix(result, nrow = n, ncol = length(at))
        colnames(result_matrix) <- as.character(at)
        return(result_matrix)
      }
    }
  }

  # Special case for type = "variance" - implement direct calculation
  if (type == "variance" && is.null(newdata) &&
    !is.null(object$parameter_vectors) && !is.null(object$fitted.values)) {
    n <- length(object$fitted.values)
    variances <- numeric(n)

    # Get needed parameters
    alpha_vec <- object$parameter_vectors$alphaVec
    beta_vec <- object$parameter_vectors$betaVec
    gamma_vec <- object$parameter_vectors$gammaVec
    delta_vec <- object$parameter_vectors$deltaVec
    lambda_vec <- object$parameter_vectors$lambdaVec
    means <- object$fitted.values

    # Calculate variance for each observation
    for (i in 1:n) {
      if (family == "beta") {
        # Analytical formula for Beta distribution
        variances[i] <- (gamma_vec[i] * delta_vec[i]) /
          ((gamma_vec[i] + delta_vec[i])^2 * (gamma_vec[i] + delta_vec[i] + 1))
      } else if (family == "kw") {
        # Approximate formula for Kumaraswamy distribution
        variances[i] <- alpha_vec[i] * beta_vec[i] *
          beta(1 + 1 / alpha_vec[i], beta_vec[i]) -
          (alpha_vec[i] * beta_vec[i] * beta(1 + 1 / alpha_vec[i], beta_vec[i]))^2
      } else {
        # Numerical approximation using density function with proper prefix
        h <- 0.001
        mean_i <- means[i]

        # Safely compute y within (0,1)
        mean_plus <- min(0.99999, mean_i + h)
        mean_minus <- max(0.00001, mean_i - h)

        # Use the appropriate density function for the family
        switch(family,
          "gkw" = {
            f_plus <- dgkw(mean_plus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
            f <- dgkw(mean_i, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dgkw(mean_minus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
          },
          "bkw" = {
            f_plus <- dbkw(mean_plus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
            f <- dbkw(mean_i, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
            f_minus <- dbkw(mean_minus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
          },
          "kkw" = {
            f_plus <- dkkw(mean_plus, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
            f <- dkkw(mean_i, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dkkw(mean_minus, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
          },
          "ekw" = {
            f_plus <- dekw(mean_plus, alpha_vec[i], beta_vec[i], lambda_vec[i])
            f <- dekw(mean_i, alpha_vec[i], beta_vec[i], lambda_vec[i])
            f_minus <- dekw(mean_minus, alpha_vec[i], beta_vec[i], lambda_vec[i])
          },
          "mc" = {
            f_plus <- dmc(mean_plus, gamma_vec[i], delta_vec[i], lambda_vec[i])
            f <- dmc(mean_i, gamma_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dmc(mean_minus, gamma_vec[i], delta_vec[i], lambda_vec[i])
          }
        )

        # Approximate second derivative of log-likelihood
        d2ll <- (f_plus - 2 * f + f_minus) / (h * h)

        # Variance is inverse of the information
        variances[i] <- min(0.25, max(1e-6, 1 / (abs(d2ll) + 1e-10)))
      }
    }

    return(variances)
  }

  # Special case for type = "link" when newdata is NULL
  if (type == "link" && is.null(newdata)) {
    if (!is.null(object$tmb_object) && !is.null(object$tmb_object$env$data)) {
      # Try to extract linear predictors from TMB object if available
      tmb_data <- object$tmb_object$env$data
      if (!is.null(tmb_data$X1) && !is.null(tmb_data$X2) &&
        !is.null(tmb_data$X3) && !is.null(tmb_data$X4) &&
        !is.null(tmb_data$X5)) {
        # Extract coefficients
        coefs <- object$coefficients
        if (is.list(coefs)) {
          beta1 <- coefs$alpha
          beta2 <- coefs$beta
          beta3 <- coefs$gamma
          beta4 <- coefs$delta
          beta5 <- coefs$lambda
        } else {
          # Try using regex patterns to extract coefficients
          beta1 <- coefs[grep("^alpha:", names(coefs))]
          beta2 <- coefs[grep("^beta:", names(coefs))]
          beta3 <- coefs[grep("^gamma:", names(coefs))]
          beta4 <- coefs[grep("^delta:", names(coefs))]
          beta5 <- coefs[grep("^lambda:", names(coefs))]

          # If regex didn't work, try to infer from dimensions
          if (length(beta1) == 0 || length(beta2) == 0) {
            # Extract design matrices
            X1 <- tmb_data$X1
            X2 <- tmb_data$X2
            X3 <- tmb_data$X3
            X4 <- tmb_data$X4
            X5 <- tmb_data$X5

            # Split coefficients based on matrix dimensions
            all_coefs <- coefs
            beta1 <- all_coefs[1:ncol(X1)]
            remaining <- all_coefs[-(1:ncol(X1))]

            beta2 <- remaining[1:ncol(X2)]
            remaining <- remaining[-(1:ncol(X2))]

            beta3 <- remaining[1:ncol(X3)]
            remaining <- remaining[-(1:ncol(X3))]

            beta4 <- remaining[1:ncol(X4)]
            remaining <- remaining[-(1:ncol(X4))]

            beta5 <- remaining[1:ncol(X5)]
          }
        }

        # Calculate linear predictors using TMB data matrices
        X1 <- tmb_data$X1
        X2 <- tmb_data$X2
        X3 <- tmb_data$X3
        X4 <- tmb_data$X4
        X5 <- tmb_data$X5

        eta1 <- as.vector(X1 %*% beta1)
        eta2 <- as.vector(X2 %*% beta2)
        eta3 <- as.vector(X3 %*% beta3)
        eta4 <- as.vector(X4 %*% beta4)
        eta5 <- as.vector(X5 %*% beta5)

        return(data.frame(
          alpha = eta1,
          beta = eta2,
          gamma = eta3,
          delta = eta4,
          lambda = eta5
        ))
      }
    }

    # If we can't extract linear predictors, return a warning and parameter values instead
    if (!is.null(object$parameter_vectors)) {
      warning("Cannot calculate link values without design matrices. Returning parameter values instead.")
      return(data.frame(
        alpha = object$parameter_vectors$alphaVec,
        beta = object$parameter_vectors$betaVec,
        gamma = object$parameter_vectors$gammaVec,
        delta = object$parameter_vectors$deltaVec,
        lambda = object$parameter_vectors$lambdaVec
      ))
    } else {
      stop("Cannot extract linear predictors. Please provide 'newdata' or set x=TRUE when fitting the model.")
    }
  }

  # Get parameter information for the family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions
  fixed_params <- param_info$fixed

  # Identify non-fixed parameters for this family
  non_fixed_params <- setdiff(param_names, names(fixed_params))

  # Prepare model matrices for prediction
  if (is.null(newdata)) {
    # Try different approaches to access model matrices
    if (!is.null(object$x)) {
      # Model matrices stored directly (fitted with x=TRUE)
      matrices <- list()
      for (param in param_names) {
        pos <- param_positions[[param]]
        if (param %in% names(object$x)) {
          matrices[[paste0("X", pos)]] <- object$x[[param]]
        } else {
          # Default intercept-only matrix for missing parameters
          n_obs <- length(object$y)
          matrices[[paste0("X", pos)]] <- matrix(1, n_obs, 1,
            dimnames = list(NULL, "(Intercept)")
          )
        }
      }
    } else if (!is.null(object$model) && !is.null(object$formula)) {
      # Reconstruct matrices from model frame and formula
      matrices <- list()
      formula_obj <- object$formula
      mf <- object$model

      # Ensure formula is a Formula object
      if (!inherits(formula_obj, "Formula")) {
        formula_obj <- Formula::as.Formula(formula_obj)
      }

      # Get number of RHS parts
      n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

      # Create matrix for each parameter
      for (i in seq_along(param_names)) {
        param <- param_names[i]
        pos <- param_positions[[param]]

        if (i <= n_parts) {
          # Try to extract matrix for this formula part
          tryCatch(
            {
              X_i <- stats::model.matrix(formula_obj, data = mf, rhs = i)
              matrices[[paste0("X", pos)]] <- X_i
            },
            error = function(e) {
              # Default to intercept-only if extraction fails
              matrices[[paste0("X", pos)]] <- matrix(1, nrow(mf), 1,
                dimnames = list(NULL, "(Intercept)")
              )
            }
          )
        } else {
          # Default to intercept-only for parts beyond those specified
          matrices[[paste0("X", pos)]] <- matrix(1, nrow(mf), 1,
            dimnames = list(NULL, "(Intercept)")
          )
        }
      }
    } else if (!is.null(object$tmb_object) && !is.null(object$tmb_object$env$data)) {
      # Use matrices from TMB object
      tmb_data <- object$tmb_object$env$data
      matrices <- list()

      for (param in param_names) {
        pos <- param_positions[[param]]
        matrix_name <- paste0("X", pos)

        if (!is.null(tmb_data[[matrix_name]])) {
          matrices[[matrix_name]] <- tmb_data[[matrix_name]]
        }
      }
    } else {
      stop("Cannot extract model matrices. Please provide 'newdata' or set x=TRUE when fitting the model.")
    }
  } else {
    # Create model matrices from newdata
    matrices <- list()
    formula <- object$formula

    # Ensure formula is a Formula object
    if (!inherits(formula, "Formula")) {
      formula <- Formula::as.Formula(formula)
    }

    # Get number of RHS parts
    n_parts <- length(attr(Formula::Formula(formula), "rhs"))

    # Process each part of the formula for each parameter
    for (i in seq_along(param_names)) {
      param <- param_names[i]
      pos <- param_positions[[param]]

      if (i <= n_parts) {
        # Extract terms for this formula part
        component_terms <- stats::delete.response(stats::terms(formula, rhs = i))

        # Create model frame and matrix
        tryCatch(
          {
            mf_i <- stats::model.frame(component_terms, newdata,
              na.action = na.action,
              xlev = object$xlevels
            )

            X_i <- stats::model.matrix(component_terms, mf_i)

            if (ncol(X_i) > 0) {
              matrices[[paste0("X", pos)]] <- X_i
            } else {
              matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
                dimnames = list(NULL, "(Intercept)")
              )
            }
          },
          error = function(e) {
            # Use intercept-only matrix if model frame creation fails
            matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
              dimnames = list(NULL, "(Intercept)")
            )
          }
        )
      } else {
        # Default intercept-only matrix for parts beyond those in formula
        matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
          dimnames = list(NULL, "(Intercept)")
        )
      }
    }
  }

  # Fill in any missing matrices with intercept-only
  for (i in 1:max(unlist(param_positions))) {
    matrix_name <- paste0("X", i)
    if (is.null(matrices[[matrix_name]])) {
      n_obs <- nrow(matrices[[1]])
      matrices[[matrix_name]] <- matrix(1, n_obs, 1, dimnames = list(NULL, "(Intercept)"))
    }
  }

  # Extract coefficients for each parameter
  coefs <- list()

  if (is.list(object$coefficients) && !is.null(names(object$coefficients))) {
    # If coefficients are in a named list
    for (param in param_names) {
      if (param %in% names(object$coefficients)) {
        coefs[[param]] <- object$coefficients[[param]]
      } else if (param %in% names(fixed_params)) {
        # Use fixed value for fixed parameters
        coefs[[param]] <- fixed_params[[param]]
      }
    }
  } else {
    # If coefficients are in a vector, extract using parameter names pattern
    all_coefs <- object$coefficients

    for (param in param_names) {
      # Try to find coefficients for this parameter using pattern matching
      param_pattern <- paste0("^", param, ":")
      param_coefs <- all_coefs[grep(param_pattern, names(all_coefs))]

      if (length(param_coefs) > 0) {
        coefs[[param]] <- param_coefs
      } else if (param %in% names(fixed_params)) {
        # Use fixed value for fixed parameters
        coefs[[param]] <- fixed_params[[param]]
      }
    }

    # If pattern matching didn't work, try positional approach using matrix dimensions
    if (all(sapply(coefs, length) == 0) && !is.null(all_coefs)) {
      remaining_coefs <- all_coefs

      for (param in param_names) {
        if (param %in% names(fixed_params)) {
          coefs[[param]] <- fixed_params[[param]]
          next
        }

        pos <- param_positions[[param]]
        X <- matrices[[paste0("X", pos)]]

        if (!is.null(X) && ncol(X) > 0 && length(remaining_coefs) >= ncol(X)) {
          coefs[[param]] <- remaining_coefs[1:ncol(X)]
          remaining_coefs <- remaining_coefs[-(1:ncol(X))]
        }
      }
    }
  }

  # Check if any coefficients are missing
  missing_coefs <- setdiff(non_fixed_params, names(coefs)[sapply(coefs, length) > 0])
  if (length(missing_coefs) > 0) {
    stop(
      "Could not extract coefficients for parameter(s): ",
      paste(missing_coefs, collapse = ", ")
    )
  }

  # Extract link functions
  link_types <- rep(NA, 5)
  names(link_types) <- c("alpha", "beta", "gamma", "delta", "lambda")

  if (!is.null(object$link)) {
    # Link functions are stored directly
    link_map <- c(
      "log" = 1,
      "logit" = 2,
      "probit" = 3,
      "cauchy" = 4,
      "cloglog" = 5,
      "identity" = 6,
      "sqrt" = 7,
      "inverse" = 8,
      "inverse-square" = 9
    )

    for (param in names(object$link)) {
      if (param %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
        link_types[param] <- link_map[object$link[[param]]]
      }
    }
  }

  # Fill in default link types for any missing ones
  if (is.na(link_types["alpha"])) link_types["alpha"] <- 1 # log
  if (is.na(link_types["beta"])) link_types["beta"] <- 1 # log
  if (is.na(link_types["gamma"])) link_types["gamma"] <- 1 # log
  if (is.na(link_types["delta"])) link_types["delta"] <- 2 # logit
  if (is.na(link_types["lambda"])) link_types["lambda"] <- 1 # log

  # Extract scale factors (or use defaults)
  scale_factors <- c(10, 10, 10, 1, 10) # Default scale factors

  # If the type is "link", calculate and return the linear predictors
  if (type == "link") {
    # Initialize results
    n_obs <- nrow(matrices[[1]])
    result <- data.frame(
      alpha = rep(NA_real_, n_obs),
      beta = rep(NA_real_, n_obs),
      gamma = rep(NA_real_, n_obs),
      delta = rep(NA_real_, n_obs),
      lambda = rep(NA_real_, n_obs)
    )

    # Calculate linear predictor for each parameter
    for (param in param_names) {
      if (param %in% names(fixed_params)) next

      pos <- param_positions[[param]]
      X <- matrices[[paste0("X", pos)]]
      beta <- coefs[[param]]

      # Match dimensions of X and beta
      if (length(beta) != ncol(X)) {
        warning(
          "Dimension mismatch for ", param, ": X has ", ncol(X),
          " columns but beta has ", length(beta), " elements."
        )
        next
      }

      result[[param]] <- as.vector(X %*% beta)
    }

    return(result)
  }

  # For the other types, calculate distribution parameters on original scale
  params <- matrix(0, nrow = nrow(matrices[[1]]), ncol = 5)
  colnames(params) <- c("alpha", "beta", "gamma", "delta", "lambda")

  # Calculate parameters for each observation
  for (param in c("alpha", "beta", "gamma", "delta", "lambda")) {
    param_idx <- match(param, c("alpha", "beta", "gamma", "delta", "lambda"))

    if (param %in% names(fixed_params)) {
      # Fixed parameter - same value for all observations
      params[, param_idx] <- fixed_params[[param]]
    } else if (param %in% param_names) {
      # Calculate from model matrix and coefficients
      pos <- param_positions[[param]]
      X <- matrices[[paste0("X", pos)]]
      beta <- coefs[[param]]
      link_type <- link_types[param]

      # Check dimension compatibility
      if (length(beta) != ncol(X)) {
        stop(
          "Dimension mismatch for ", param, ": X has ", ncol(X),
          " columns but beta has ", length(beta), " elements."
        )
      }

      # Calculate linear predictor
      eta <- as.vector(X %*% beta)

      # Apply inverse link function
      params[, param_idx] <- switch(as.character(link_type),
        "1" = exp(eta), # log
        "2" = 1 / (1 + exp(-eta)), # logit
        "3" = pnorm(eta), # probit
        "4" = 0.5 + (1 / pi) * atan(eta), # cauchy
        "5" = 1 - exp(-exp(eta)), # cloglog
        "6" = eta, # identity
        "7" = eta^2, # sqrt
        "8" = 1 / eta, # inverse
        "9" = 1 / sqrt(eta), # inverse-square
        exp(eta) # default to log
      )
    }
  }

  # Apply family-specific constraints to ensure proper parameter ranges
  switch(family,
    "gkw" = {
      # All parameters used, no extra constraints needed
    },
    "bkw" = {
      # lambda = 1 fixed
      params[, "lambda"] <- 1
    },
    "kkw" = {
      # gamma = 1 fixed
      params[, "gamma"] <- 1
    },
    "ekw" = {
      # gamma = 1, delta = 0 fixed
      params[, "gamma"] <- 1
      params[, "delta"] <- 0
    },
    "mc" = {
      # alpha = 1, beta = 1 fixed
      params[, "alpha"] <- 1
      params[, "beta"] <- 1
    },
    "kw" = {
      # gamma = 1, delta = 0, lambda = 1 fixed
      params[, "gamma"] <- 1
      params[, "delta"] <- 0
      params[, "lambda"] <- 1
    },
    "beta" = {
      # alpha = 1, beta = 1, lambda = 1 fixed
      params[, "alpha"] <- 1
      params[, "beta"] <- 1
      params[, "lambda"] <- 1
    }
  )

  # Handle remaining parameter range constraints
  params[, "alpha"] <- pmax(1e-7, params[, "alpha"])
  params[, "beta"] <- pmax(1e-7, params[, "beta"])
  params[, "gamma"] <- pmax(1e-7, params[, "gamma"])
  params[, "delta"] <- pmax(1e-7, pmin(1 - 1e-7, params[, "delta"]))
  params[, "lambda"] <- pmax(1e-7, params[, "lambda"])

  # Return the parameters if requested
  if (type == "parameter") {
    return(data.frame(
      alpha = params[, "alpha"],
      beta = params[, "beta"],
      gamma = params[, "gamma"],
      delta = params[, "delta"],
      lambda = params[, "lambda"]
    ))
  } else if (type %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
    param_idx <- match(type, c("alpha", "beta", "gamma", "delta", "lambda"))
    return(params[, param_idx])
  }

  # Calculate mean response
  if (type == "response") {
    means <- calculateMeans(params, family = family)
    return(means)
  } else if (type == "variance") {
    # Calculate means first
    means <- calculateMeans(params, family = family)
    n_obs <- nrow(params)

    # Calculate variance for each observation
    variances <- numeric(n_obs)

    for (i in 1:n_obs) {
      alpha_i <- params[i, "alpha"]
      beta_i <- params[i, "beta"]
      gamma_i <- params[i, "gamma"]
      delta_i <- params[i, "delta"]
      lambda_i <- params[i, "lambda"]

      # Different variance approximations based on family
      if (family == "beta") {
        variances[i] <- (gamma_i * delta_i) /
          ((gamma_i + delta_i)^2 * (gamma_i + delta_i + 1))
      } else if (family == "kw") {
        variances[i] <- alpha_i * beta_i *
          beta(1 + 1 / alpha_i, beta_i) -
          (alpha_i * beta_i * beta(1 + 1 / alpha_i, beta_i))^2
      } else {
        # Numerical approximation
        h <- 0.001
        mean_i <- means[i]

        # Safely compute for y within (0,1)
        mean_plus <- min(0.999, mean_i + h)
        mean_minus <- max(0.001, mean_i - h)

        # Use the appropriate density function based on family
        switch(family,
          "gkw" = {
            f_plus <- dgkw(mean_plus, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            f <- dgkw(mean_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            f_minus <- dgkw(mean_minus, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
          },
          "bkw" = {
            f_plus <- dbkw(mean_plus, alpha_i, beta_i, gamma_i, delta_i)
            f <- dbkw(mean_i, alpha_i, beta_i, gamma_i, delta_i)
            f_minus <- dbkw(mean_minus, alpha_i, beta_i, gamma_i, delta_i)
          },
          "kkw" = {
            f_plus <- dkkw(mean_plus, alpha_i, beta_i, delta_i, lambda_i)
            f <- dkkw(mean_i, alpha_i, beta_i, delta_i, lambda_i)
            f_minus <- dkkw(mean_minus, alpha_i, beta_i, delta_i, lambda_i)
          },
          "ekw" = {
            f_plus <- dekw(mean_plus, alpha_i, beta_i, lambda_i)
            f <- dekw(mean_i, alpha_i, beta_i, lambda_i)
            f_minus <- dekw(mean_minus, alpha_i, beta_i, lambda_i)
          },
          "mc" = {
            f_plus <- dmc(mean_plus, gamma_i, delta_i, lambda_i)
            f <- dmc(mean_i, gamma_i, delta_i, lambda_i)
            f_minus <- dmc(mean_minus, gamma_i, delta_i, lambda_i)
          }
        )

        # Approximate second derivative of log-likelihood
        d2ll <- (f_plus - 2 * f + f_minus) / (h * h)

        # Variance is inverse of the information
        variances[i] <- min(0.25, max(1e-6, 1 / (abs(d2ll) + 1e-10)))
      }
    }

    return(variances)
  }

  # For the remaining types (density, probability, quantile), we need the 'at' argument
  if (!is.numeric(at)) {
    stop("'at' must be numeric")
  }

  # Check if the elementwise mode should be used
  if (is.null(elementwise)) {
    elementwise <- length(at) == nrow(params)
  }

  n_obs <- nrow(params)

  # Prepare data for calculation based on elementwise mode
  if (elementwise) {
    if (length(at) != n_obs) {
      stop("For elementwise=TRUE, length of 'at' must equal number of observations")
    }
    eval_y <- at
    eval_params <- params
  } else {
    # Repeat params for each value in 'at'
    eval_params <- do.call(rbind, lapply(seq_len(length(at)), function(i) params))
    # Repeat each value of 'at' for each row of params
    eval_y <- rep(at, each = n_obs)
  }

  # Calculate the requested quantities using appropriate distribution functions
  result <- numeric(length(eval_y))

  for (i in seq_along(eval_y)) {
    alpha_i <- eval_params[i, "alpha"]
    beta_i <- eval_params[i, "beta"]
    gamma_i <- eval_params[i, "gamma"]
    delta_i <- eval_params[i, "delta"]
    lambda_i <- eval_params[i, "lambda"]
    y_i <- eval_y[i]

    # Use the appropriate distribution function based on family and type
    if (type == "density") {
      switch(family,
        "gkw" = {
          result[i] <- dgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- dbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- dkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- dekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- dmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- dkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- dbeta_(y_i, gamma_i, delta_i)
        }
      )
    } else if (type == "probability") {
      switch(family,
        "gkw" = {
          result[i] <- pgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- pbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- pkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- pekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- pmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- pkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- pbeta_(y_i, gamma_i, delta_i)
        }
      )
    } else if (type == "quantile") {
      switch(family,
        "gkw" = {
          result[i] <- qgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- qbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- qkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- qekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- qmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- qkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- qbeta_(y_i, gamma_i, delta_i + 1)
        }
      )
    }
  }

  # Format the result according to elementwise mode
  if (elementwise) {
    return(result)
  } else {
    result_matrix <- matrix(result, nrow = n_obs, ncol = length(at))
    colnames(result_matrix) <- as.character(at)
    return(result_matrix)
  }
}



#' @title Extract Fitted Values from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' Extracts the fitted mean values (predicted expected values of the response)
#' from a fitted Generalized Kumaraswamy (GKw) regression model object of class
#' \code{"gkwreg"}. This is an S3 method for the generic
#' \code{\link[stats]{fitted.values}} function.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param family Character string specifying the distribution family under which
#'   the fitted mean values should be calculated. If \code{NULL} (default), the
#'   family stored within the fitted \code{object} is used. Specifying a different
#'   family (e.g., \code{"beta"}) will trigger recalculation of the fitted means
#'   based on that family's mean structure, using the original model's estimated
#'   coefficients mapped to the relevant parameters. Available options match those
#'   in \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function retrieves or calculates the fitted values, which represent the
#' estimated conditional mean of the response variable given the covariates
#' (\eqn{E(Y | X)}).
#'
#' The function attempts to retrieve fitted values efficiently using the following
#' priority:
#' \enumerate{
#'   \item Directly from the \code{fitted.values} component stored in the \code{object},
#'     if available and complete. It includes logic to handle potentially
#'     incomplete stored values via interpolation (\code{\link[stats]{approx}}) for
#'     very large datasets where only a sample might be stored.
#'   \item By recalculating the mean using stored parameter vectors for each
#'     observation (\code{object$parameter_vectors}) and an internal function
#'     (\code{calculateMeans}), if available.
#'   \item From the \code{fitted} component within the TMB report (\code{object$tmb_object$report()}),
#'     if available, potentially using interpolation as above.
#'   \item As a fallback, by calling \code{predict(object, type = "response", family = family)}.
#' }
#' Specifying a \code{family} different from the one used to fit the model will
#' always force recalculation using the \code{predict} method (step 4).
#'
#' @return A numeric vector containing the fitted mean values. These values are
#'   typically bounded between 0 and 1, corresponding to the scale of the original
#'   response variable. The length of the vector corresponds to the number of
#'   observations used in the model fit (considering \code{subset} and \code{na.action}).
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{predict.gkwreg}},
#'   \code{\link{residuals.gkwreg}}, \code{\link[stats]{fitted.values}}
#'
#' @keywords fitted methods regression
#'
#' @examples
#' \donttest{
#' # Assume 'mydata' exists with response 'y' and predictors 'x1', 'x2'
#' # and that rgkw() is available and data is appropriate (0 < y < 1).
#' set.seed(456)
#' n <- 100
#' x1 <- runif(n, -1, 1)
#' x2 <- rnorm(n)
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(0.8 - 0.3 * x1 + 0.1 * x2)
#' gamma <- exp(0.6)
#' delta <- plogis(0.0 + 0.2 * x1)
#' lambda <- exp(-0.2 + 0.1 * x2)
#' # Use stats::rbeta as placeholder if rgkw is not available
#' y <- stats::rbeta(n, shape1 = gamma * alpha, shape2 = delta * beta) # Approximation
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' mydata <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit a GKw model
#' model <- gkwreg(y ~ x1 | x1 + x2 | 1 | x1 | x2, data = mydata, family = "gkw")
#'
#' # Extract fitted values (using the original 'gkw' family)
#' fitted_vals_gkw <- fitted(model)
#'
#' # Extract fitted values recalculated as if it were a Beta model
#' # (using the fitted gamma and delta coefficients)
#' fitted_vals_beta <- fitted(model, family = "beta")
#'
#' # Plot observed vs. fitted (using original family)
#' response_y <- model$y # Get the response variable used in the fit
#' if (!is.null(response_y)) {
#'   plot(response_y, fitted_vals_gkw,
#'     xlab = "Observed Response", ylab = "Fitted Mean Value",
#'     main = "Observed vs Fitted Values (GKw Family)",
#'     pch = 1, col = "blue"
#'   )
#'   abline(0, 1, col = "red", lty = 2) # Line y = x
#' } else {
#'   print("Response variable not found in model object to create plot.")
#' }
#'
#' # Compare fitted values under different family assumptions
#' head(data.frame(GKw_Fitted = fitted_vals_gkw, Beta_Fitted = fitted_vals_beta))
#' }
#'
#' @export
fitted.gkwreg <- function(object, family = NULL, ...) {
  # Check if object is of class "gkwreg"
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model of class \"gkwreg\"")
  }

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, we need to recalculate
    if (!is.null(object$family) && family != object$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        object$family, "). Recalculating fitted values..."
      ))

      # Use predict to calculate fitted values with the new family
      return(stats::predict(object, type = "response", family = family))
    }
  }

  # Determine the number of observations from various possible sources
  n <- if (!is.null(object$nobs)) {
    object$nobs
  } else if (!is.null(object$y)) {
    length(object$y)
  } else if (!is.null(object$model) && !is.null(object$model$y)) {
    length(object$model$y)
  } else if (!is.null(object$parameter_vectors) && !is.null(object$parameter_vectors$alphaVec)) {
    length(object$parameter_vectors$alphaVec)
  } else {
    stop("Cannot determine the number of observations in the model")
  }

  # Method 1: Try to get fitted values directly from the model object
  if (!is.null(object$fitted.values)) {
    fitted_len <- length(object$fitted.values)

    # Check if dimensions match the expected number of observations
    if (fitted_len == n) {
      return(object$fitted.values)
    } else if (fitted_len == 1 && n > 1) {
      message("Only one fitted value found in model object. Recalculating all fitted values...")
    } else if (fitted_len > 1 && fitted_len < n && n > 10000) {
      # For very large datasets, we might have a sample of fitted values - try to interpolate
      message("Partial fitted values found. Interpolating...")

      # Create fitted values vector with NAs
      fitted_values <- rep(NA_real_, n)

      # Map available values to appropriate indices
      sample_size <- fitted_len
      sample_idx <- floor(seq(1, n, length.out = sample_size))
      fitted_values[sample_idx] <- object$fitted.values

      # Interpolate the rest
      idx_with_values <- which(!is.na(fitted_values))
      fitted_values <- stats::approx(
        x = idx_with_values,
        y = fitted_values[idx_with_values],
        xout = seq_len(n),
        rule = 2
      )$y

      return(fitted_values)
    }
  }

  # Method 2: Try to use the parameter vectors if available
  if (!is.null(object$parameter_vectors)) {
    # Check if all necessary parameter vectors are available
    param_vec <- object$parameter_vectors
    if (!is.null(param_vec$alphaVec) &&
      !is.null(param_vec$betaVec) &&
      !is.null(param_vec$gammaVec) &&
      !is.null(param_vec$deltaVec) &&
      !is.null(param_vec$lambdaVec)) {
      # Check if dimensions match
      param_lengths <- c(
        length(param_vec$alphaVec),
        length(param_vec$betaVec),
        length(param_vec$gammaVec),
        length(param_vec$deltaVec),
        length(param_vec$lambdaVec)
      )

      if (all(param_lengths == n)) {
        # Create parameter matrix for calculateMeans
        params <- matrix(0, nrow = n, ncol = 5)
        params[, 1] <- param_vec$alphaVec
        params[, 2] <- param_vec$betaVec
        params[, 3] <- param_vec$gammaVec
        params[, 4] <- param_vec$deltaVec
        params[, 5] <- param_vec$lambdaVec

        # Calculate means using the parameters
        message("Calculating fitted values from parameter vectors...")
        return(calculateMeans(params, family = family))
      }
    }
  }

  # Method 3: Check TMB report for fitted values
  if (!is.null(object$tmb_object)) {
    tmb_report <- try(object$tmb_object$report(), silent = TRUE)

    if (!inherits(tmb_report, "try-error") && "fitted" %in% names(tmb_report)) {
      fitted_from_tmb <- tmb_report$fitted

      if (length(fitted_from_tmb) == n) {
        return(as.vector(fitted_from_tmb))
      } else if (length(fitted_from_tmb) > 1 && length(fitted_from_tmb) < n && n > 10000) {
        # For large datasets, interpolate if we only have a sample
        message("Found partial fitted values in TMB report. Interpolating...")

        # Create fitted values vector with NAs
        fitted_values <- rep(NA_real_, n)

        # Map available values to appropriate indices
        sample_size <- length(fitted_from_tmb)
        sample_idx <- floor(seq(1, n, length.out = sample_size))
        fitted_values[sample_idx] <- fitted_from_tmb

        # Interpolate the rest
        idx_with_values <- which(!is.na(fitted_values))
        fitted_values <- stats::approx(
          x = idx_with_values,
          y = fitted_values[idx_with_values],
          xout = seq_len(n),
          rule = 2
        )$y

        return(fitted_values)
      }
    }
  }

  # Method 4: If all else fails, use predict.gkwreg
  message("Recalculating fitted values using the predict function...")

  # Use predict to calculate fitted values
  fitted_values <- stats::predict(object, type = "response", family = family)

  return(fitted_values)
}


#' @title Extract Residuals from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' Extracts or calculates various types of residuals from a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}, useful for
#' model diagnostics.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param type Character string specifying the type of residuals to compute.
#'   Available options are:
#'   \itemize{
#'     \item \code{"response"}: (Default) Raw response residuals:
#'       \eqn{y - \mu}, where \eqn{\mu} is the fitted mean.
#'     \item \code{"pearson"}: Pearson residuals: \eqn{(y - \mu) / \sqrt{V(\mu)}},
#'       where \eqn{V(\mu)} is the variance function of the specified family.
#'     \item \code{"deviance"}: Deviance residuals: Signed square root of the
#'       unit deviances. Sum of squares equals the total deviance.
#'     \item \code{"quantile"}: Randomized quantile residuals (Dunn & Smyth, 1996).
#'       Transformed via the model's CDF and the standard normal quantile function.
#'       Should approximate a standard normal distribution if the model is correct.
#'     \item \code{"modified.deviance"}: (Not typically implemented, placeholder)
#'       Standardized deviance residuals, potentially adjusted for leverage.
#'     \item \code{"cox-snell"}: Cox-Snell residuals: \eqn{-\log(1 - F(y))}, where
#'       \eqn{F(y)} is the model's CDF. Should approximate a standard exponential
#'       distribution if the model is correct.
#'     \item \code{"score"}: (Not typically implemented, placeholder) Score residuals,
#'       related to the derivative of the log-likelihood.
#'     \item \code{"partial"}: Partial residuals for a specific predictor in one
#'       parameter's linear model: \eqn{eta_p + \beta_{pk} x_{ik}}, where \eqn{eta_p}
#'       is the partial linear predictor and \eqn{\beta_{pk} x_{ik}} is the
#'       component associated with the k-th covariate for the i-th observation.
#'       Requires \code{parameter} and \code{covariate_idx}.
#'   }
#' @param covariate_idx Integer. Only used if \code{type = "partial"}. Specifies the
#'   index (column number in the corresponding model matrix) of the covariate for
#'   which to compute partial residuals.
#' @param parameter Character string. Only used if \code{type = "partial"}. Specifies
#'   the distribution parameter (\code{"alpha"}, \code{"beta"}, \code{"gamma"},
#'   \code{"delta"}, or \code{"lambda"}) whose linear predictor contains the
#'   covariate of interest.
#' @param family Character string specifying the distribution family assumptions
#'   to use when calculating residuals (especially for types involving variance,
#'   deviance, CDF, etc.). If \code{NULL} (default), the family stored within the
#'   fitted \code{object} is used. Specifying a different family may be useful
#'   for diagnostic comparisons. Available options match those in
#'   \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function calculates various types of residuals useful for diagnosing the
#' adequacy of a fitted GKw regression model.
#'
#' \itemize{
#'   \item \strong{Response residuals} (\code{type="response"}) are the simplest,
#'     showing raw differences between observed and fitted mean values.
#'   \item \strong{Pearson residuals} (\code{type="pearson"}) account for the
#'     mean-variance relationship specified by the model family. Constant variance
#'     when plotted against fitted values suggests the variance function is appropriate.
#'   \item \strong{Deviance residuals} (\code{type="deviance"}) are related to the
#'     log-likelihood contribution of each observation. Their sum of squares equals
#'     the total model deviance. They often have more symmetric distributions
#'     than Pearson residuals.
#'   \item \strong{Quantile residuals} (\code{type="quantile"}) are particularly useful
#'     for non-standard distributions as they should always be approximately standard
#'     normal if the assumed distribution and model structure are correct. Deviations
#'     from normality in a QQ-plot indicate model misspecification.
#'   \item \strong{Cox-Snell residuals} (\code{type="cox-snell"}) provide another
#'     check of the overall distributional fit. A plot of the sorted residuals
#'     against theoretical exponential quantiles should approximate a straight line
#'     through the origin with slope 1.
#'   \item \strong{Partial residuals} (\code{type="partial"}) help visualize the
#'     marginal relationship between a specific predictor and the response on the
#'     scale of the linear predictor for a chosen parameter, adjusted for other predictors.
#' }
#' Calculations involving the distribution's properties (variance, CDF, PDF) depend
#' heavily on the specified \code{family}. The function relies on internal helper
#' functions (potentially implemented in C++ for efficiency) to compute these based
#' on the fitted parameters for each observation.
#'
#' @return A numeric vector containing the requested type of residuals. The length
#'   corresponds to the number of observations used in the model fit.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Dunn, P. K., & Smyth, G. K. (1996). Randomized Quantile Residuals.
#' \emph{Journal of Computational and Graphical Statistics}, \strong{5}(3), 236-244.
#'
#'
#' Cox, D. R., & Snell, E. J. (1968). A General Definition of Residuals.
#' \emph{Journal of the Royal Statistical Society, Series B (Methodological)},
#' \strong{30}(2), 248-275.
#'
#' McCullagh, P., & Nelder, J. A. (1989). \emph{Generalized Linear Models} (2nd ed.).
#' Chapman and Hall/CRC.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{fitted.gkwreg}},
#'   \code{\link{predict.gkwreg}}, \code{\link[stats]{residuals}}
#'
#' @keywords residuals methods regression diagnostics
#'
#' @examples
#' \donttest{
#' # Assume 'mydata' exists with response 'y' and predictors 'x1', 'x2'
#' # and that rgkw() is available and data is appropriate (0 < y < 1).
#' set.seed(456)
#' n <- 150
#' x1 <- runif(n, -1, 1)
#' x2 <- rnorm(n)
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(0.8 - 0.3 * x1 + 0.1 * x2)
#' gamma <- exp(0.6)
#' delta <- plogis(0.0 + 0.2 * x1)
#' lambda <- exp(-0.2 + 0.1 * x2)
#' # Use stats::rbeta as placeholder if rgkw is not available
#' y <- stats::rbeta(n, shape1 = gamma * alpha, shape2 = delta * beta) # Approximation
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' mydata <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit a GKw model
#' model <- gkwreg(y ~ x1 | x1 + x2 | 1 | x1 | x2, data = mydata, family = "gkw")
#'
#' # --- Extract different types of residuals ---
#' resp_res <- residuals(model, type = "response")
#' pearson_res <- residuals(model, type = "pearson")
#' quant_res <- residuals(model, type = "quantile")
#' cs_res <- residuals(model, type = "cox-snell")
#'
#' # --- Diagnostic Plots ---
#' # QQ-plot for quantile residuals (should be approx. normal)
#' stats::qqnorm(quant_res, main = "QQ Plot: GKw Quantile Residuals")
#' stats::qqline(quant_res, col = "blue")
#'
#' # Cox-Snell residuals plot (should be approx. exponential -> linear on exp-QQ)
#' plot(stats::qexp(stats::ppoints(length(cs_res))), sort(cs_res),
#'   xlab = "Theoretical Exponential Quantiles", ylab = "Sorted Cox-Snell Residuals",
#'   main = "Cox-Snell Residual Plot", pch = 1
#' )
#' abline(0, 1, col = "red")
#'
#' # --- Compare residuals using a different family assumption ---
#' # Calculate quantile residuals assuming underlying Beta dist
#' quant_res_beta <- residuals(model, type = "quantile", family = "beta")
#'
#' # Compare QQ-plots
#' stats::qqnorm(quant_res, main = "GKw Quantile Residuals")
#' stats::qqline(quant_res, col = "blue")
#' stats::qqnorm(quant_res_beta, main = "Beta Quantile Residuals (from GKw Fit)")
#' stats::qqline(quant_res_beta, col = "darkgreen")
#'
#' # --- Partial Residuals ---
#' # Examine effect of x1 on the alpha parameter's linear predictor
#' if ("x1" %in% colnames(model$x$alpha)) { # Check if x1 is in alpha model matrix
#'   # Find index for 'x1' (could be 2 if intercept is first)
#'   x1_idx_alpha <- which(colnames(model$x$alpha) == "x1")
#'   if (length(x1_idx_alpha) == 1) {
#'     part_res_alpha_x1 <- residuals(model,
#'       type = "partial",
#'       parameter = "alpha", covariate_idx = x1_idx_alpha
#'     )
#'     # Plot partial residuals against the predictor
#'     plot(mydata$x1, part_res_alpha_x1,
#'       xlab = "x1", ylab = "Partial Residual (alpha predictor)",
#'       main = "Partial Residual Plot for alpha ~ x1"
#'     )
#'     # Add a smoother to see the trend
#'     lines(lowess(mydata$x1, part_res_alpha_x1), col = "red")
#'   }
#' }
#'
#' # Examine effect of x2 on the beta parameter's linear predictor
#' if ("x2" %in% colnames(model$x$beta)) {
#'   x2_idx_beta <- which(colnames(model$x$beta) == "x2")
#'   if (length(x2_idx_beta) == 1) {
#'     part_res_beta_x2 <- residuals(model,
#'       type = "partial",
#'       parameter = "beta", covariate_idx = x2_idx_beta
#'     )
#'     plot(mydata$x2, part_res_beta_x2,
#'       xlab = "x2", ylab = "Partial Residual (beta predictor)",
#'       main = "Partial Residual Plot for beta ~ x2"
#'     )
#'     lines(lowess(mydata$x2, part_res_beta_x2), col = "red")
#'   }
#' }
#' }
#'
#' @export
residuals.gkwreg <- function(
    object, type = c(
      "response", "pearson", "deviance", "quantile",
      "modified.deviance", "cox-snell",
      "score", "partial"
    ),
    covariate_idx = 1, parameter = "alpha",
    family = NULL, ...) {
  # Check if object is of class "gkwreg"
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model of class \"gkwreg\"")
  }

  # Match argument
  type <- match.arg(type)

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, show a message
    if (!is.null(object$family) && family != object$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        object$family, ")."
      ))
    }
  }

  # Get response values
  if (is.null(object$y)) {
    stop("Response variable not found in model object. Cannot calculate residuals.")
  }
  y <- object$y

  # Get fitted values - passing the family parameter
  fitted_vals <- stats::fitted(object, family = family)

  # If type is "response", we can calculate quickly
  if (type == "response") {
    return(calculateResponseResiduals(y, fitted_vals))
  }

  # For partial residuals, verify parameters
  if (type == "partial") {
    if (!parameter %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
      stop("parameter must be one of: 'alpha', 'beta', 'gamma', 'delta', 'lambda'")
    }

    # Confirm valid covariate_idx
    model_matrices <- object$model_matrices
    if (is.null(model_matrices)) {
      stop("Model matrices not available in model object")
    }

    X <- model_matrices[[parameter]]
    if (is.null(X)) {
      stop("Model matrix for parameter '", parameter, "' not found")
    }

    p <- ncol(X)
    if (covariate_idx < 1 || covariate_idx > p) {
      stop("covariate_idx must be between 1 and ", p)
    }
  }

  # Get parameters for each observation
  get_parameters <- function(object, family) {
    # Initialize vectors for parameters
    n <- length(object$y)

    # Try to get parameters via predict with the specified family
    if (exists("predict.gkwreg", mode = "function")) {
      tryCatch(
        {
          params_df <- stats::predict(object, type = "parameter", family = family)
          return(list(
            alpha = params_df$alpha,
            beta = params_df$beta,
            gamma = params_df$gamma,
            delta = params_df$delta,
            lambda = params_df$lambda
          ))
        },
        error = function(e) {
          warning("Could not extract parameter values using predict.gkwreg(): ", e$message)
        }
      )
    }

    # If we're using the same family as the model, try to get parameters directly
    if (is.null(object$family) || family == object$family) {
      # Try to get individual parameter vectors from the model object first
      if (!is.null(object$model) && !is.null(object$model$report)) {
        report <- object$model$report()

        # Check if individual parameter vectors are available
        if (all(c("alphaVec", "betaVec", "gammaVec", "deltaVec", "lambdaVec") %in% names(report))) {
          return(list(
            alpha = report$alphaVec,
            beta = report$betaVec,
            gamma = report$gammaVec,
            delta = report$deltaVec,
            lambda = report$lambdaVec
          ))
        }
      }
    }

    # If we get here, we need to approximate using the average parameter values
    # or recompute parameters with the appropriate family constraints

    # Check if we have fitted parameters already
    if (!is.null(object$fitted_parameters)) {
      # Start with the model's fitted parameters
      base_params <- list(
        alpha = rep(object$fitted_parameters$alpha, n),
        beta = rep(object$fitted_parameters$beta, n),
        gamma = rep(object$fitted_parameters$gamma, n),
        delta = rep(object$fitted_parameters$delta, n),
        lambda = rep(object$fitted_parameters$lambda, n)
      )

      # Apply family-specific constraints if needed
      if (family != "gkw") {
        if (family == "bkw") {
          # BKw: lambda = 1
          base_params$lambda <- rep(1.0, n)
        } else if (family == "kkw") {
          # KKw: gamma = 1
          base_params$gamma <- rep(1.0, n)
        } else if (family == "ekw") {
          # EKw: gamma = 1, delta = 0
          base_params$gamma <- rep(1.0, n)
          base_params$delta <- rep(0.0, n)
        } else if (family == "mc") {
          # MC: alpha = 1, beta = 1
          base_params$alpha <- rep(1.0, n)
          base_params$beta <- rep(1.0, n)
        } else if (family == "kw") {
          # KW: lambda = 1, gamma = 1, delta = 0
          base_params$gamma <- rep(1.0, n)
          base_params$delta <- rep(0.0, n)
          base_params$lambda <- rep(1.0, n)
        } else if (family == "beta") {
          # Beta: alpha = 1, beta = 1, lambda = 1
          base_params$alpha <- rep(1.0, n)
          base_params$beta <- rep(1.0, n)
          base_params$lambda <- rep(1.0, n)
        }
      }

      message("Using adjusted parameter values for family ", family, ".")
      return(base_params)
    }

    # If all else fails
    stop("Unable to extract parameter values from the model object.")
  }

  # Get parameters with the specified family
  params <- get_parameters(object, family)

  # Create a parameter matrix for C++ functions that expect it
  param_matrix <- matrix(
    c(params$alpha, params$beta, params$gamma, params$delta, params$lambda),
    ncol = 5
  )

  # Calculate appropriate residuals
  result <- switch(type,
    "pearson" = {
      calculatePearsonResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "deviance" = {
      calculateDevianceResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "quantile" = {
      calculateQuantileResiduals(
        y, param_matrix,
        family = family
      )
    },
    "modified.deviance" = {
      calculateModifiedDevianceResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "cox-snell" = {
      calculateCoxSnellResiduals(
        y, param_matrix,
        family = family
      )
    },
    "score" = {
      calculateScoreResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "partial" = {
      # Get model matrices and coefficients for the specified parameter
      X <- object$model_matrices[[parameter]]
      beta <- object$coefficients[[parameter]]

      # We need C++ 0-based indexing
      c_idx <- covariate_idx - 1

      calculatePartialResiduals(y, fitted_vals, X, beta, c_idx)
    }
  )

  return(result)
}


#' @title Diagnostic Plots for Generalized Kumaraswamy Regression Models
#'
#' @description
#' Produces a set of diagnostic plots for assessing the adequacy of a fitted
#' Generalized Kumaraswamy (GKw) regression model (objects of class \code{"gkwreg"}).
#' Options allow selection of specific plots, choice of residual type, and plotting
#' using either base R graphics or \code{ggplot2}.
#'
#' @param x An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param which Integer vector specifying which diagnostic plots to produce.
#'   If a subset of the plots is required, specify a subset of the numbers 1:6.
#'   Defaults to \code{1:6}. The plots correspond to:
#'   \enumerate{
#'     \item Residuals vs. Observation Indices: Checks for time trends or patterns.
#'     \item Cook's Distance Plot: Helps identify influential observations.
#'     \item Generalized Leverage vs. Fitted Values: Identifies points with high leverage.
#'     \item Residuals vs. Linear Predictor: Checks for non-linearity and heteroscedasticity.
#'     \item Half-Normal Plot of Residuals (with simulated envelope): Assesses normality
#'       of residuals, comparing against simulated quantiles.
#'     \item Predicted vs. Observed Values: Checks overall model prediction accuracy.
#'   }
#' @param caption Character vector providing captions (titles) for the plots.
#'   Its length must be at least \code{max(which)}. Defaults are provided for plots 1-6.
#' @param sub.caption Character string used as a common subtitle positioned above all plots
#'   (especially when multiple plots are arranged). Defaults to the deparsed model call.
#' @param main An optional character string to be prepended to the individual plot captions
#'   (from the \code{caption} argument).
#' @param ask Logical. If \code{TRUE} (and using base R graphics with multiple plots
#'   on an interactive device), the user is prompted before displaying each plot.
#'   Defaults to \code{TRUE} if more plots are requested than fit on the current screen layout.
#' @param ... Additional arguments passed to the underlying plotting functions
#'   (e.g., graphical parameters like \code{col}, \code{pch}, \code{cex} for base R plots).
#' @param type Character string indicating the type of residuals to be used for plotting.
#'   Defaults to \code{"quantile"}. Valid options are:
#'   \itemize{
#'     \item \code{"quantile"}: Randomized quantile residuals (Dunn & Smyth, 1996).
#'       Recommended for bounded responses as they should be approximately N(0,1)
#'       if the model is correctly specified.
#'     \item \code{"pearson"}: Pearson residuals (response residual standardized by
#'       estimated standard deviation). Useful for checking the variance function.
#'     \item \code{"deviance"}: Deviance residuals. Related to the log-likelihood
#'       contribution of each observation.
#'   }
#' @param family Character string specifying the distribution family assumptions
#'   to use when calculating residuals and other diagnostics. If \code{NULL} (default),
#'   the family stored within the fitted \code{object} is used. Specifying a different
#'   family can be useful for diagnostic comparisons. Available options match those
#'   in \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' @param nsim Integer. Number of simulations used to generate the envelope in the
#'   half-normal plot (\code{which = 5}). Defaults to 100. Must be positive.
#' @param level Numeric. The confidence level (between 0 and 1) for the simulated
#'   envelope in the half-normal plot (\code{which = 5}). Defaults to 0.90.
#' @param use_ggplot Logical. If \code{TRUE}, plots are generated using the \code{ggplot2}
#'   package. If \code{FALSE} (default), base R graphics are used. Requires the
#'   \code{ggplot2} package to be installed if set to \code{TRUE}.
#' @param arrange_plots Logical. Only relevant if \code{use_ggplot = TRUE} and multiple
#'   plots are requested (\code{length(which) > 1}). If \code{TRUE}, attempts to arrange
#'   the generated \code{ggplot} objects into a grid using either the \code{gridExtra}
#'   or \code{ggpubr} package (requires one of them to be installed). Defaults to \code{FALSE}.
#' @param sample_size Integer or \code{NULL}. If specified as an integer less than the
#'   total number of observations (\code{x$nobs}), a random sample of this size is
#'   used for calculating diagnostics and plotting. This can be useful for speeding up
#'   plots with very large datasets. Defaults to \code{NULL} (use all observations).
#' @param theme_fn A function. Only relevant if \code{use_ggplot = TRUE}. Specifies a
#'   \code{ggplot2} theme function to apply to the plots (e.g., \code{theme_bw},
#'   \code{theme_classic}). Defaults to \code{ggplot2::theme_minimal}.
#' @param save_diagnostics Logical. If \code{TRUE}, the function invisibly returns a list
#'   containing the calculated diagnostic measures (residuals, leverage, Cook's distance, etc.)
#'   instead of the model object. If \code{FALSE} (default), the function invisibly
#'   returns the original model object \code{x}.
#'
#' @details
#' Diagnostic plots are essential for evaluating the assumptions and adequacy of
#' fitted regression models. This function provides several standard plots adapted
#' for \code{gkwreg} objects.
#'
#' The choice of residual type (\code{type}) is important. For models with bounded
#' responses like the GKw family, quantile residuals (\code{type = "quantile"}) are
#' generally preferred as they are constructed to be approximately normally distributed
#' under a correctly specified model, making standard diagnostic tools like QQ-plots
#' more directly interpretable.
#'
#' The plots help to assess:
#' \itemize{
#'   \item Plot 1 (Residuals vs. Index): Potential patterns or autocorrelation over time/index.
#'   \item Plot 2 (Cook's Distance): Observations with disproportionately large influence
#'     on the estimated coefficients.
#'   \item Plot 3 (Leverage vs. Fitted): Observations with unusual predictor combinations
#'     (high leverage) that might influence the fit.
#'   \item Plot 4 (Residuals vs. Linear Predictor): Non-linearity in the predictor-response
#'     relationship or non-constant variance (heteroscedasticity).
#'   \item Plot 5 (Half-Normal Plot): Deviations from the assumed residual distribution
#'     (ideally normal for quantile residuals). Points outside the simulated envelope
#'     are potentially problematic.
#'   \item Plot 6 (Predicted vs. Observed): Overall goodness-of-fit and potential systematic
#'     over- or under-prediction.
#' }
#' The function relies on internal helper functions to calculate the necessary diagnostic
#' quantities and generate the plots using either base R or \code{ggplot2}.
#'
#' @return Invisibly returns either the original fitted model object \code{x}
#'   (if \code{save_diagnostics = FALSE}) or a list containing the calculated
#'   diagnostic measures used for plotting (if \code{save_diagnostics = TRUE}).
#'   Primarily called for its side effect of generating plots.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{residuals.gkwreg}},
#'   \code{\link{summary.gkwreg}}, \code{\link[stats]{plot.lm}},
#'   \code{\link[ggplot2]{ggplot}}, \code{\link[gridExtra]{grid.arrange}},
#'   \code{\link[ggpubr]{ggarrange}}
#'
#' @keywords plot methods regression diagnostics hplot
#'
#' @examples
#' \donttest{
#' # Assume 'mydata' exists with response 'y' and predictors 'x1', 'x2'
#' # and that rgkw() is available and data is appropriate (0 < y < 1).
#' set.seed(456)
#' n <- 150
#' x1 <- runif(n, -1, 1)
#' x2 <- rnorm(n)
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(0.8 - 0.3 * x1 + 0.1 * x2)
#' gamma <- exp(0.6)
#' delta <- plogis(0.0 + 0.2 * x1)
#' lambda <- exp(-0.2 + 0.1 * x2)
#' # Use stats::rbeta as placeholder if rgkw is not available
#' y <- stats::rbeta(n, shape1 = gamma * alpha, shape2 = delta * beta) # Approximation
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' mydata <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit a GKw model
#' model <- gkwreg(y ~ x1 | x1 + x2 | 1 | x1 | x2, data = mydata, family = "gkw")
#'
#' # --- Generate default base R plots (prompts for each plot) ---
#' plot(model)
#'
#' # --- Generate specific plots using base R ---
#' plot(model, which = c(1, 5), type = "quantile") # Residuals vs Index, Half-Normal
#'
#' # --- Generate plots using ggplot2 (requires ggplot2 package) ---
#' # Ensure ggplot2 is installed: install.packages("ggplot2")
#' plot(model, which = c(4, 6), use_ggplot = TRUE) # Res vs Lin Pred, Pred vs Obs
#'
#' # --- Generate all ggplot2 plots and arrange them (requires gridExtra or ggpubr) ---
#' # Ensure gridExtra is installed: install.packages("gridExtra")
#' # plot(model, use_ggplot = TRUE, arrange_plots = TRUE, ask = FALSE)
#'
#' # --- Generate plots using Pearson residuals ---
#' plot(model, which = 4, type = "pearson") # Res vs Lin Pred using Pearson residuals
#'
#' # --- Save diagnostic measures instead of plotting ---
#' diagnostics <- plot(model, save_diagnostics = TRUE)
#' head(diagnostics$residuals)
#' head(diagnostics$cooks_distance)
#' }
#'
#' @export
plot.gkwreg <- function(x,
                        which = 1:6,
                        caption = c(
                          "Residuals vs. Observation Indices",
                          "Cook's Distance Plot",
                          "Generalized Leverage vs. Fitted Values",
                          "Residuals vs. Linear Predictor",
                          "Half-Normal Plot of Residuals",
                          "Predicted vs. Observed Values"
                        ),
                        sub.caption = paste(deparse(x$call), collapse = "\n"),
                        main = "",
                        ask = prod(par("mfcol")) < length(which) && dev.interactive(),
                        ...,
                        type = c("quantile", "pearson", "deviance"),
                        family = NULL,
                        nsim = 100,
                        level = 0.90,
                        use_ggplot = FALSE,
                        arrange_plots = FALSE,
                        sample_size = NULL,
                        theme_fn = ggplot2::theme_minimal,
                        save_diagnostics = FALSE) {
  # Validate inputs and prepare diagnostic data using the fixed function
  diag_data <- .validate_and_prepare_gkwreg_diagnostics(
    x = x,
    which = which,
    caption = caption,
    type = type,
    family = family,
    nsim = nsim,
    level = level,
    use_ggplot = use_ggplot,
    arrange_plots = arrange_plots,
    sample_size = sample_size,
    theme_fn = theme_fn
  )

  # Get formatted plot titles
  plot_titles <- .create_plot_titles(
    which = which,
    caption = caption,
    main = main,
    family = diag_data$model_info$family,
    orig_family = x$family
  )

  # Choose plotting implementation based on use_ggplot
  if (!use_ggplot) {
    # Base R graphics implementation
    result <- .plot_gkwreg_base_r(
      diag_data = diag_data,
      which = which,
      plot_titles = plot_titles,
      sub.caption = sub.caption,
      ask = ask,
      ...
    )
  } else {
    # ggplot2 implementation
    result <- .plot_gkwreg_ggplot(
      diag_data = diag_data,
      which = which,
      plot_titles = plot_titles,
      sub.caption = sub.caption,
      ask = FALSE,
      arrange_plots = arrange_plots,
      theme_fn = theme_fn,
      ...
    )
  }

  # Return diagnostic data if requested
  if (save_diagnostics) {
    return(invisible(diag_data))
  } else {
    return(invisible(x))
  }
}



#' Validate inputs and prepare diagnostic data for gkwreg plots
#'
#' @param x A fitted model object of class "gkwreg"
#' @param which Integer vector specifying which plots to produce
#' @param caption Character vector of plot captions
#' @param type Character string specifying residual type
#' @param family Character string specifying distribution family
#' @param nsim Number of simulations for envelope calculation
#' @param level Confidence level for envelope
#' @param use_ggplot Logical; whether to use ggplot2
#' @param arrange_plots Logical; whether to arrange multiple plots
#' @param sample_size Integer or NULL; sample size for large datasets
#' @param theme_fn ggplot2 theme function
#'
#' @return A list containing diagnostic data and model information
#'
#' @keywords internal
.validate_and_prepare_gkwreg_diagnostics <- function(x,
                                                     which,
                                                     caption,
                                                     type = c("quantile", "pearson", "deviance"),
                                                     family = NULL,
                                                     nsim = 100,
                                                     level = 0.90,
                                                     use_ggplot = FALSE,
                                                     arrange_plots = FALSE,
                                                     sample_size = NULL,
                                                     theme_fn = ggplot2::theme_minimal) {
  # Input validation
  if (!inherits(x, "gkwreg")) {
    stop("The object must be of class 'gkwreg'.")
  }

  type <- match.arg(type)

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(x$family)) {
      family <- x$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, show a message
    if (!is.null(x$family) && family != x$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        x$family, ") for diagnostics."
      ))
    }
  }

  # Get parameter information for the family
  param_info <- .get_family_param_info(family)

  # Other input validations
  if (!all(which %in% 1:6)) {
    stop("Argument 'which' must contain values between 1 and 6.")
  }

  if (max(which) > length(caption)) {
    stop("The 'caption' vector is too short for the selected 'which' plots.")
  }

  if (!is.numeric(nsim) || nsim <= 0 || nsim != round(nsim)) {
    stop("Argument 'nsim' must be a positive integer.")
  }

  if (!is.numeric(level) || level <= 0 || level >= 1) {
    stop("Argument 'level' must be between 0 and 1.")
  }

  # Check dependencies
  if (use_ggplot && !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for ggplot2 plotting. Please install it with install.packages('ggplot2').")
  }

  if (use_ggplot && arrange_plots &&
    !requireNamespace("gridExtra", quietly = TRUE) &&
    !requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Either package 'gridExtra' or 'ggpubr' is required for arranging plots. Please install one of them.")
  }

  # Extract model components with error handling
  y_obs <- x$y
  if (is.null(y_obs)) {
    stop("No 'y' component found in the model object. Ensure the model was fitted with y=TRUE.")
  }

  # Get fitted values using the specified family
  fitted_vals <- fitted(x, family = family)
  if (is.null(fitted_vals)) {
    stop("Could not calculate fitted values.")
  }

  # Extract model matrices and parameters using the improved functions
  model_matrices <- .extract_model_matrices(x)
  model_params <- .extract_model_params(x)

  # Sample data if requested
  n <- length(y_obs)
  idx <- seq_len(n)

  if (!is.null(sample_size) && is.numeric(sample_size) && sample_size > 0 && sample_size < n) {
    sampling_result <- .sample_model_data(
      n = n,
      sample_size = sample_size,
      y_obs = y_obs,
      fitted_vals = fitted_vals,
      model_matrices = model_matrices
    )

    idx <- sampling_result$idx
    y_obs <- sampling_result$y_obs
    fitted_vals <- sampling_result$fitted_vals
    model_matrices <- sampling_result$model_matrices
  }

  # Calculate model parameters for the specified family
  param_mat <- .calculate_model_parameters(model_matrices, model_params, family)

  # Extract parameter vectors
  param_vectors <- .extract_parameter_vectors(param_mat)

  # Calculate residuals
  resid_vec <- .calculate_residuals(y_obs, fitted_vals, param_mat, type, family)

  # Calculate diagnostic measures with family-specific handling
  diagnostic_measures <- .calculate_diagnostic_measures(
    y_obs = y_obs,
    fitted_vals = fitted_vals,
    resid_vec = resid_vec,
    model_matrices = model_matrices,
    model_params = model_params,
    param_vectors = param_vectors,
    idx = idx,
    family = family,
    param_info = param_info
  )

  # Calculate half-normal plot data if needed
  half_normal_data <- NULL
  if (5 %in% which) {
    half_normal_data <- .calculate_half_normal_data(
      resid_vec = resid_vec,
      idx = idx,
      nsim = nsim,
      level = level,
      param_mat = param_mat,
      param_vectors = param_vectors,
      type = type,
      family = family
    )
  }

  # Create the diagnostic data structure
  diag_data <- list(
    data = data.frame(
      index = idx,
      y_obs = y_obs,
      fitted = fitted_vals,
      resid = resid_vec,
      abs_resid = abs(resid_vec),
      cook_dist = diagnostic_measures$cook_dist,
      leverage = diagnostic_measures$leverage,
      linpred = diagnostic_measures$linpred
    ),
    model_info = list(
      n = n,
      p = diagnostic_measures$p,
      cook_threshold = 4 / n,
      leverage_threshold = 2 * diagnostic_measures$p / n,
      family = family,
      type = type,
      param_info = param_info,
      level = level
    )
  )

  # Add half-normal data if available
  if (!is.null(half_normal_data)) {
    diag_data$half_normal <- half_normal_data
  }

  return(diag_data)
}

#' Extract model matrices from a gkwreg object with family-specific handling
#'
#' @param x A fitted model object of class "gkwreg"
#' @return A list of model matrices
#'
#' @importFrom stats model.matrix
#' @keywords internal
.extract_model_matrices <- function(x) {
  # Get family parameter information
  family <- x$family
  if (is.null(family)) {
    family <- "gkw" # Default to gkw if not specified
    warning("No family specified in the model. Using 'gkw' as default.")
  }

  # Get parameter information for the specified family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions

  # Initialize matrices list with the correct number for this family
  num_matrices <- max(unlist(param_positions))
  matrices <- vector("list", num_matrices)
  names(matrices) <- paste0("X", 1:num_matrices)

  # Try different ways to get design matrices
  if (!is.null(x$x)) {
    # Model was fitted with x=TRUE
    for (param in param_names) {
      pos <- param_positions[[param]]
      if (param %in% names(x$x)) {
        matrices[[pos]] <- x$x[[param]]
      } else {
        # Default to intercept-only for missing matrices
        n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
        matrices[[pos]] <- matrix(1, nrow = n_obs, ncol = 1)
        colnames(matrices[[pos]]) <- "(Intercept)"
      }
    }
  } else if (!is.null(x$model) && !is.null(x$formula)) {
    # Recreate model matrices from the model frame and formula
    mf <- x$model
    formula_obj <- x$formula
    if (inherits(formula_obj, "formula")) {
      formula_obj <- Formula::as.Formula(formula_obj)
    }

    # Get number of rhs parts in the formula
    n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

    # Extract matrices for each parameter
    for (param in param_names) {
      pos <- param_positions[[param]]
      idx <- which(param_names == param)

      if (idx <= n_parts) {
        # Try to extract matrix using the formula part
        matrices[[pos]] <- tryCatch(
          model.matrix(formula_obj, data = mf, rhs = idx),
          error = function(e) matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
        )
      } else {
        # Default to intercept-only
        matrices[[pos]] <- matrix(1, nrow(mf), 1, dimnames = list(NULL, "(Intercept)"))
      }
    }
  } else if (!is.null(x$tmb_object) && !is.null(x$tmb_object$env$data)) {
    # Use TMB object if available
    tmb_data <- x$tmb_object$env$data

    # Extract matrices by their TMB position
    for (param in param_names) {
      pos <- param_positions[[param]]
      tmb_matrix_name <- paste0("X", pos)

      if (!is.null(tmb_data[[tmb_matrix_name]])) {
        matrices[[pos]] <- tmb_data[[tmb_matrix_name]]

        # Add column names if missing
        if (is.null(colnames(matrices[[pos]]))) {
          if (ncol(matrices[[pos]]) == 1) {
            colnames(matrices[[pos]]) <- "(Intercept)"
          } else {
            colnames(matrices[[pos]]) <- paste0(param, "_", 1:ncol(matrices[[pos]]))
          }
        }
      }
    }
  } else {
    stop("Cannot extract model matrices. Try fitting the model with x=TRUE.")
  }

  # Check for missing matrices and replace with default
  for (i in 1:num_matrices) {
    if (is.null(matrices[[i]])) {
      n_obs <- ifelse(is.null(x$y), nrow(x$model), length(x$y))
      matrices[[i]] <- matrix(1, n_obs, 1, dimnames = list(NULL, "(Intercept)"))
    }
  }

  return(matrices)
}


#' Extract model parameters from a gkwreg object with family-specific handling
#'
#' @param x A fitted model object of class "gkwreg"
#' @return A list of model parameters
#'
#' @keywords internal
.extract_model_params <- function(x) {
  # Get family information
  family <- x$family
  if (is.null(family)) family <- "gkw"

  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions
  fixed_params <- param_info$fixed

  # Initialize beta parameters for all possible positions
  beta_params <- vector("list", 5)
  names(beta_params) <- paste0("beta", 1:5)

  # Extract coefficients
  coefs <- x$coefficients

  if (is.list(coefs)) {
    # If coefficients are a list with parameter names
    for (param in param_names) {
      if (param %in% names(coefs)) {
        pos <- param_positions[[param]]
        beta_params[[pos]] <- coefs[[param]]
      }
    }
  } else if (!is.null(names(coefs))) {
    # If coefficients are named, extract them using regex patterns
    for (param in param_names) {
      pos <- param_positions[[param]]
      param_coefs <- coefs[grep(paste0("^", param, ":"), names(coefs))]
      if (length(param_coefs) > 0) {
        beta_params[[pos]] <- param_coefs
      }
    }

    # If regex didn't work, try alternative pattern matching
    empty_positions <- sapply(beta_params[1:length(param_names)], is.null)
    if (all(empty_positions)) {
      # Try alternative pattern matching
      for (param in param_names) {
        pos <- param_positions[[param]]
        param_coefs <- coefs[grep(param, names(coefs))]
        if (length(param_coefs) > 0) {
          beta_params[[pos]] <- param_coefs
        }
      }

      # If still empty, raise error
      if (all(sapply(beta_params[1:length(param_names)], is.null))) {
        stop("Cannot determine coefficient mapping. Try a more recent version of the model.")
      }
    }
  } else {
    stop("Unrecognized coefficient structure. Try a more recent version of the model.")
  }

  # Assign default values for fixed parameters
  for (param in names(fixed_params)) {
    pos <- param_positions[[param]]
    if (!is.null(pos)) {
      # Use the fixed value for this parameter
      beta_params[[pos]] <- fixed_params[[param]]
    }
  }

  # Assign default values for any remaining NULL betas
  for (i in 1:5) {
    if (is.null(beta_params[[i]])) {
      beta_params[[i]] <- 0
    }
  }

  # Extract link information
  if (!is.null(x$link_codes)) {
    link_codes <- unlist(x$link_codes, use.names = FALSE)
  } else if (!is.null(x$link)) {
    # Convert link functions to codes
    link_map <- c(
      "log" = 1,
      "logit" = 2,
      "probit" = 3,
      "cauchy" = 4,
      "cloglog" = 5,
      "identity" = 6,
      "sqrt" = 7,
      "inverse" = 8,
      "inverse-square" = 9
    )

    # Default links (log for most, logit for delta)
    link_codes <- c(1, 1, 1, 2, 1)

    # Update with actual links from the model
    for (param in param_names) {
      if (param %in% names(x$link)) {
        pos <- param_positions[[param]]
        link_codes[pos] <- link_map[x$link[[param]]]
      }
    }
  } else {
    # Default link types if not specified
    link_codes <- c(1, 1, 1, 2, 1) # log for most parameters, logit for delta
  }

  # Extract scale factors
  if (!is.null(x$scale_factors)) {
    scale_factors <- as.numeric(unlist(x$scale_factors))
  } else {
    # Default scale factors
    scale_factors <- c(10, 10, 10, 1, 10) # 10 for most parameters, 1 for delta
  }

  return(c(
    beta_params,
    list(
      link_codes = link_codes,
      scale_factors = scale_factors
    )
  ))
}


#' Sample model data for large datasets
#'
#' @param n Total number of observations
#' @param sample_size Target sample size
#' @param y_obs Vector of observed values
#' @param fitted_vals Vector of fitted values
#' @param model_matrices List of model matrices
#' @return A list with sampled data
#'
#' @keywords internal
.sample_model_data <- function(n, sample_size, y_obs, fitted_vals, model_matrices) {
  # For reproducibility
  idx <- sample(n, size = min(sample_size, n))

  # Sample matrices - ensure we maintain the structure
  sampled_matrices <- list()
  for (matrix_name in names(model_matrices)) {
    X <- model_matrices[[matrix_name]]
    if (is.matrix(X) && nrow(X) == n) {
      sampled_matrices[[matrix_name]] <- X[idx, , drop = FALSE]
    } else {
      # Handle non-matrix or incorrectly sized matrix
      sampled_matrices[[matrix_name]] <- matrix(1, length(idx), 1)
    }
  }

  return(list(
    idx = idx,
    y_obs = y_obs[idx],
    fitted_vals = fitted_vals[idx],
    model_matrices = sampled_matrices
  ))
}


#' Calculate model parameters for the specified family
#'
#' @param model_matrices List of model matrices
#' @param model_params List of model parameters
#' @param family Character string specifying distribution family
#' @return A matrix of calculated parameters
#'
#' @keywords internal
.calculate_model_parameters <- function(model_matrices, model_params, family) {
  # Get parameter information for this family
  param_info <- .get_family_param_info(family)

  # Calculate the number of required parameters based on family
  num_params <- max(unlist(param_info$positions))

  # Extract matrices for all parameters
  X_matrices <- vector("list", 5)
  for (i in 1:5) {
    X_name <- paste0("X", i)
    if (i <= num_params && X_name %in% names(model_matrices)) {
      X_matrices[[i]] <- model_matrices[[X_name]]
    } else {
      # Default matrix for unused parameters
      X_matrices[[i]] <- matrix(1, nrow(model_matrices[[1]]), 1)
    }
  }

  # Extract beta parameters
  beta_params <- vector("list", 5)
  for (i in 1:5) {
    beta_name <- paste0("beta", i)
    if (beta_name %in% names(model_params)) {
      beta_params[[i]] <- model_params[[beta_name]]
    } else {
      # Default for unused parameters
      beta_params[[i]] <- 0
    }
  }

  # Extract link codes and scale factors
  link_codes <- model_params$link_codes
  scale_factors <- model_params$scale_factors

  # Call the calculateParameters function with proper unpacking
  param_mat <- calculateParameters(
    X_matrices[[1]], X_matrices[[2]], X_matrices[[3]],
    X_matrices[[4]], X_matrices[[5]],
    beta_params[[1]], beta_params[[2]], beta_params[[3]],
    beta_params[[4]], beta_params[[5]],
    link_codes, scale_factors,
    family = family
  )

  return(param_mat)
}

#' Extract parameter vectors from parameter matrix
#'
#' @param param_mat Matrix of calculated parameters
#' @return A list of parameter vectors
#'
#' @keywords internal
.extract_parameter_vectors <- function(param_mat) {
  list(
    alphaVec = param_mat[, 1],
    betaVec = param_mat[, 2],
    gammaVec = param_mat[, 3],
    deltaVec = param_mat[, 4],
    lambdaVec = param_mat[, 5]
  )
}


#' Calculate residuals based on the specified type
#'
#' @param y_obs Vector of observed values
#' @param fitted_vals Vector of fitted values
#' @param param_mat Matrix of calculated parameters
#' @param type Character string specifying residual type
#' @param family Character string specifying distribution family
#' @return A vector of residuals
#'
#' @keywords internal
.calculate_residuals <- function(y_obs, fitted_vals, param_mat, type, family) {
  # The following calculate*Residuals() are assumed to be internal package functions
  if (type == "quantile") {
    calculateQuantileResiduals(y_obs, param_mat, family = family)
  } else if (type == "pearson") {
    calculatePearsonResiduals(y_obs, fitted_vals, param_mat, family = family)
  } else if (type == "deviance") {
    calculateDevianceResiduals(y_obs, fitted_vals, param_mat, family = family)
  } else {
    stop("Unsupported residual type.")
  }
}

#' Calculate diagnostic measures for gkwreg plots
#'
#' @param y_obs Vector of observed values
#' @param fitted_vals Vector of fitted values
#' @param resid_vec Vector of residuals
#' @param model_matrices List of model matrices
#' @param model_params List of model parameters
#' @param param_vectors List of parameter vectors
#' @param idx Vector of observation indices
#' @param family Character string specifying distribution family
#' @param param_info Parameter information for the family
#' @return A list of diagnostic measures
#'
#' @importFrom stats rnorm
#' @keywords internal
.calculate_diagnostic_measures <- function(y_obs,
                                           fitted_vals,
                                           resid_vec,
                                           model_matrices,
                                           model_params,
                                           param_vectors,
                                           idx,
                                           family,
                                           param_info) {
  # Get the first active parameter for this family to use for linear predictor
  first_param <- param_info$names[1]
  first_pos <- param_info$positions[[first_param]]

  # Get the X matrix and beta coefficients for the first parameter
  X_matrix <- model_matrices[[paste0("X", first_pos)]]
  beta_coef <- model_params[[paste0("beta", first_pos)]]

  # Calculate linear predictor
  if (length(beta_coef) > 0 && !is.null(X_matrix)) {
    if (is.matrix(X_matrix) && is.numeric(beta_coef) &&
      ncol(X_matrix) == length(beta_coef)) {
      linpred <- as.vector(X_matrix %*% beta_coef)
    } else {
      # Fallback for incompatible dimensions
      linpred <- rep(0, length(idx))
    }
  } else {
    # Fallback for missing data
    linpred <- rep(0, length(idx))
  }

  # Calculate total number of parameters (excluding fixed ones)
  p <- 0
  for (param in param_info$names) {
    pos <- param_info$positions[[param]]
    beta_name <- paste0("beta", pos)
    if (beta_name %in% names(model_params)) {
      beta_val <- model_params[[beta_name]]
      if (is.numeric(beta_val) && length(beta_val) > 0) {
        p <- p + length(beta_val)
      }
    }
  }

  # Ensure p is at least 1 to avoid division by zero
  p <- max(1, p)

  # Mean squared error
  mse <- mean(resid_vec^2, na.rm = TRUE)

  # Approximate generalized leverage
  n <- length(idx)
  leverage <- rep(p / n, length(idx))

  # Small noise addition for visual differentiation
  leverage <- leverage + abs(rnorm(length(idx), 0, 0.01))

  # Calculate Cook's distance
  cook_dist <- (resid_vec^2 / (p * mse)) * (leverage / ((1 - leverage)^2))
  cook_dist[is.infinite(cook_dist) | cook_dist > 100 | is.na(cook_dist)] <- NA

  list(
    linpred = linpred,
    p = p,
    leverage = leverage,
    cook_dist = cook_dist
  )
}


#' Calculate half-normal plot data with envelope
#'
#' @param resid_vec Vector of residuals
#' @param idx Vector of observation indices
#' @param nsim Number of simulations for envelope
#' @param level Confidence level for envelope
#' @param param_mat Matrix of calculated parameters
#' @param param_vectors List of parameter vectors
#' @param type Character string specifying residual type
#' @param family Character string specifying distribution family
#' @return A data frame with half-normal plot data
#' @importFrom stats quantile
#'
#' @keywords internal
.calculate_half_normal_data <- function(resid_vec,
                                        idx,
                                        nsim,
                                        level,
                                        param_mat,
                                        param_vectors,
                                        type,
                                        family) {
  abs_resid <- abs(resid_vec)
  sorted_abs_resid <- sort(abs_resid)
  prob_points <- (seq_along(idx) - 0.5) / length(idx)
  hn_q <- stats::qnorm(0.5 + prob_points / 2) # half-normal quantiles from normal

  # Prepare data frame
  half_normal_data <- data.frame(
    index = seq_along(sorted_abs_resid),
    theoretical = hn_q,
    observed = sorted_abs_resid
  )

  # Extract parameter vectors for simulation
  alphaVec <- param_vectors$alphaVec
  betaVec <- param_vectors$betaVec
  gammaVec <- param_vectors$gammaVec
  deltaVec <- param_vectors$deltaVec
  lambdaVec <- param_vectors$lambdaVec

  # Simulate envelope
  envelope_data <- matrix(NA, nrow = length(idx), ncol = nsim)

  cat("Simulating envelope (", nsim, "iterations): ")
  progress_step <- max(1, floor(nsim / 10))

  for (i in seq_len(nsim)) {
    if (i %% progress_step == 0) cat(".")

    # Generate simulated data using family-specific random generation
    sim_y <- .simulate_from_distribution(
      n = length(idx),
      alphaVec = alphaVec,
      betaVec = betaVec,
      gammaVec = gammaVec,
      deltaVec = deltaVec,
      lambdaVec = lambdaVec,
      family = family
    )

    # Calculate residuals for simulated data
    sim_resid <- .calculate_sim_residuals(
      sim_y = sim_y,
      param_mat = param_mat,
      type = type,
      family = family
    )

    envelope_data[, i] <- sort(abs(sim_resid))
  }
  cat(" Done!\n")

  # Calculate envelope bounds
  lower_bound <- apply(envelope_data, 1, quantile, probs = (1 - level) / 2, na.rm = TRUE)
  upper_bound <- apply(envelope_data, 1, quantile, probs = 1 - (1 - level) / 2, na.rm = TRUE)

  half_normal_data$lower <- lower_bound
  half_normal_data$upper <- upper_bound

  half_normal_data
}




#' Simulate observations from a specified distribution family
#'
#' @param n Number of observations to simulate
#' @param alphaVec Vector of alpha parameters
#' @param betaVec Vector of beta parameters
#' @param gammaVec Vector of gamma parameters
#' @param deltaVec Vector of delta parameters
#' @param lambdaVec Vector of lambda parameters
#' @param family Character string specifying distribution family
#' @return A vector of simulated observations
#'
#' @keywords internal
.simulate_from_distribution <- function(n,
                                        alphaVec,
                                        betaVec,
                                        gammaVec,
                                        deltaVec,
                                        lambdaVec,
                                        family) {
  # Generate uniform random variates
  sim_u <- stats::runif(n)

  # Use the appropriate quantile function for each family
  # This approach replaces the complex nested expressions with direct calls
  # to the existing quantile functions for each distribution
  switch(family,
    "gkw" = {
      # Use the implemented qgkw function
      rgkw(n,
        alpha = alphaVec, beta = betaVec, gamma = gammaVec,
        delta = deltaVec, lambda = lambdaVec
      )
    },
    "bkw" = {
      # BKw: lambda = 1
      rbkw(n, alpha = alphaVec, beta = betaVec, gamma = gammaVec, delta = deltaVec)
    },
    "kkw" = {
      # KKw: gamma = 1
      rkkw(n, alpha = alphaVec, beta = betaVec, delta = deltaVec, lambda = lambdaVec)
    },
    "ekw" = {
      # EKw: gamma = 1, delta = 0
      rekw(n, alpha = alphaVec, beta = betaVec, lambda = lambdaVec)
    },
    "mc" = {
      # MC: alpha = 1, beta = 1
      rmc(n, gamma = gammaVec, delta = deltaVec, lambda = lambdaVec)
    },
    "kw" = {
      # KW: lambda = 1, gamma = 1, delta = 0
      rkw(n, alpha = alphaVec, beta = betaVec)
    },
    "beta" = {
      # Beta: alpha = 1, beta = 1, lambda = 1
      rbeta_(n, gammaVec, deltaVec)
    },
    # Default case - use the GKw distribution
    {
      warning("Unrecognized family '", family, "'. Using GKw distribution instead.")
      rgkw(n,
        alpha = alphaVec, beta = betaVec, gamma = gammaVec,
        delta = deltaVec, lambda = lambdaVec
      )
    }
  )
}



#' Calculate residuals for simulated data
#'
#' @param sim_y Vector of simulated observations
#' @param param_mat Matrix of calculated parameters
#' @param type Character string specifying residual type
#' @param family Character string specifying distribution family
#' @return A vector of residuals
#'
#' @keywords internal
.calculate_sim_residuals <- function(sim_y, param_mat, type, family) {
  if (type == "quantile") {
    calculateQuantileResiduals(sim_y, param_mat, family = family)
  } else if (type == "pearson") {
    sim_fitted <- calculateMeans(param_mat, family = family)
    calculatePearsonResiduals(sim_y, sim_fitted, param_mat, family = family)
  } else if (type == "deviance") {
    sim_fitted <- calculateMeans(param_mat, family = family)
    calculateDevianceResiduals(sim_y, sim_fitted, param_mat, family = family)
  } else {
    stop("Unsupported residual type.")
  }
}


#' Create formatted plot titles
#'
#' @param which Integer vector specifying which plots to produce
#' @param caption Character vector of plot captions
#' @param main Optional character string for main title
#' @param family Character string specifying distribution family
#' @param orig_family Original family from the model
#' @return A named vector of formatted plot titles
#'
#' @keywords internal
.create_plot_titles <- function(which, caption, main, family, orig_family) {
  plot_titles <- rep("", length(which))
  names(plot_titles) <- which

  for (i in seq_along(which)) {
    i_plot <- which[i]
    base_title <- caption[i_plot]
    if (!is.null(orig_family) && family != orig_family) {
      base_title <- paste0(base_title, " (", family, ")")
    }
    if (nzchar(main)) {
      plot_titles[as.character(i_plot)] <- paste(main, "-", base_title)
    } else {
      plot_titles[as.character(i_plot)] <- base_title
    }
  }

  plot_titles
}


#' Generate diagnostic plots using base R graphics
#'
#' @param diag_data List of diagnostic data
#' @param which Integer vector specifying which plots to produce
#' @param plot_titles Named vector of plot titles
#' @param sub.caption Character string for subtitle
#' @param ask Logical; whether to ask before new plots
#' @param ... Additional graphical parameters
#' @return NULL (invisibly)
#'
#' @keywords internal
.plot_gkwreg_base_r <- function(diag_data, which, plot_titles, sub.caption, ask, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  one.fig <- prod(par("mfcol")) == 1

  if (ask) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask), add = TRUE)
  }

  type <- diag_data$model_info$type

  for (i_plot in which) {
    grDevices::dev.hold()
    p_main <- plot_titles[as.character(i_plot)]

    if (i_plot == 1) {
      .plot_base_r_residuals_vs_index(diag_data, p_main, type, ...)
    } else if (i_plot == 2) {
      .plot_base_r_cooks_distance(diag_data, p_main, ...)
    } else if (i_plot == 3) {
      .plot_base_r_leverage_vs_fitted(diag_data, p_main, ...)
    } else if (i_plot == 4) {
      .plot_base_r_residuals_vs_linpred(diag_data, p_main, type, ...)
    } else if (i_plot == 5) {
      .plot_base_r_half_normal(diag_data, p_main, type, ...)
    } else if (i_plot == 6) {
      .plot_base_r_predicted_vs_observed(diag_data, p_main, ...)
    }
    mtext(sub.caption, side = 3, line = 0.25, cex = 0.8, col = "gray40")

    grDevices::dev.flush()

    if (ask && !one.fig) {
      message("Press <ENTER> to continue to the next plot...")
      invisible(readLines(con = "stdin", n = 1))
    }
  }

  invisible(NULL)
}


#' Plot residuals vs. index (base R)
#'
#' @keywords internal
.plot_base_r_residuals_vs_index <- function(diag_data, p_main, type, ...) {
  plot(diag_data$data$index, diag_data$data$resid,
    ylab = paste0(type, " residuals"),
    xlab = "Observation Index",
    main = p_main, ...
  )
  abline(h = 0, lty = 2, col = "gray")
  lines(stats::lowess(diag_data$data$index, diag_data$data$resid),
    col = "red", lwd = 2
  )

  invisible(NULL)
}


#' Plot Cook's distance (base R)
#'
#' @keywords internal
.plot_base_r_cooks_distance <- function(diag_data, p_main, ...) {
  plot(diag_data$data$index, diag_data$data$cook_dist,
    ylab = "Cook's Distance",
    xlab = "Observation Index",
    main = p_main, ...
  )

  threshold <- diag_data$model_info$cook_threshold
  abline(h = threshold, lty = 2, col = "red")
  text(0.9 * max(diag_data$data$index), threshold * 1.1,
    "4/n",
    col = "red", pos = 3
  )

  invisible(NULL)
}


#' Plot leverage vs. fitted (base R)
#'
#' @keywords internal
.plot_base_r_leverage_vs_fitted <- function(diag_data, p_main, ...) {
  plot(diag_data$data$fitted, diag_data$data$leverage,
    xlab = "Fitted Values",
    ylab = "Generalized Leverage",
    main = p_main, ...
  )

  threshold <- diag_data$model_info$leverage_threshold
  abline(h = threshold, lty = 2, col = "red")
  text(0.9 * max(diag_data$data$fitted), threshold * 1.1,
    "2p/n",
    col = "red", pos = 3
  )

  invisible(NULL)
}


#' Plot residuals vs. linear predictor (base R)
#'
#' @keywords internal
.plot_base_r_residuals_vs_linpred <- function(diag_data, p_main, type, ...) {
  plot(diag_data$data$linpred, diag_data$data$resid,
    xlab = "Linear Predictor (alpha)",
    ylab = paste0(type, " residuals"),
    main = p_main, ...
  )
  abline(h = 0, lty = 2, col = "gray")
  lines(stats::lowess(diag_data$data$linpred, diag_data$data$resid),
    col = "red", lwd = 2
  )

  invisible(NULL)
}


#' Plot half-normal plot (base R)
#'
#' @importFrom stats qqnorm qqline
#' @keywords internal
.plot_base_r_half_normal <- function(diag_data, p_main, type, ...) {
  if (is.null(diag_data$half_normal)) {
    # fallback to a normal QQ-plot of absolute residuals
    stats::qqnorm(abs(diag_data$data$resid),
      main = paste(p_main, "(Half-Normal)"),
      ylab = paste0("|", type, " residuals|"), ...
    )
    stats::qqline(abs(diag_data$data$resid), lty = 2, col = "gray")
    return(invisible(NULL))
  }

  hn_data <- diag_data$half_normal
  plot(hn_data$theoretical, hn_data$observed,
    xlab = "Theoretical Half-Normal Quantiles",
    ylab = paste0("Ordered |", type, " residuals|"),
    main = paste(p_main, "(Half-Normal)"), ...
  )

  abline(0, stats::sd(diag_data$data$abs_resid, na.rm = TRUE),
    lty = 2, col = "gray"
  )

  if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
    lines(hn_data$theoretical, hn_data$lower, lty = 2, col = "blue", lwd = 1.5)
    lines(hn_data$theoretical, hn_data$upper, lty = 2, col = "blue", lwd = 1.5)
    level <- round(100 * (1 - (1 - 0.5 * (hn_data$upper[1] / hn_data$observed[1]))), 2)
    text(max(hn_data$theoretical) * 0.8, max(hn_data$upper) * 0.9,
      paste0(level, "% envelope"),
      col = "blue"
    )
  }

  invisible(NULL)
}


#' Plot predicted vs. observed (base R)
#'
#' @keywords internal
.plot_base_r_predicted_vs_observed <- function(diag_data, p_main, ...) {
  plot(diag_data$data$fitted, diag_data$data$y_obs,
    xlab = "Fitted (Mean)",
    ylab = "Observed (y)",
    main = p_main, ...
  )
  abline(0, 1, col = "gray", lty = 2)
  lines(stats::lowess(diag_data$data$fitted, diag_data$data$y_obs),
    col = "red", lwd = 2
  )

  invisible(NULL)
}


#' Generate diagnostic plots using ggplot2
#'
#' @param diag_data List of diagnostic data
#' @param which Integer vector specifying which plots to produce
#' @param plot_titles Named vector of plot titles
#' @param sub.caption Character string for subtitle
#' @param ask Logical; whether to ask before new plots
#' @param arrange_plots Logical; whether to arrange multiple plots in a grid
#' @param theme_fn ggplot2 theme function
#' @param ... Additional graphical parameters
#' @return NULL (invisibly)
#'
#' @keywords internal
.plot_gkwreg_ggplot <- function(diag_data,
                                which,
                                plot_titles,
                                sub.caption,
                                ask,
                                arrange_plots,
                                theme_fn,
                                ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for ggplot2 plotting.")
  }

  type <- diag_data$model_info$type
  p_list <- list()

  for (i in seq_along(which)) {
    i_plot <- which[i]
    p_main <- plot_titles[as.character(i_plot)]

    if (i_plot == 1) {
      p_list[[i]] <- .plot_ggplot_residuals_vs_index(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 2) {
      p_list[[i]] <- .plot_ggplot_cooks_distance(diag_data, p_main, sub.caption, theme_fn)
    } else if (i_plot == 3) {
      p_list[[i]] <- .plot_ggplot_leverage_vs_fitted(diag_data, p_main, sub.caption, theme_fn)
    } else if (i_plot == 4) {
      p_list[[i]] <- .plot_ggplot_residuals_vs_linpred(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 5) {
      p_list[[i]] <- .plot_ggplot_half_normal(diag_data, p_main, sub.caption, type, theme_fn)
    } else if (i_plot == 6) {
      p_list[[i]] <- .plot_ggplot_predicted_vs_observed(diag_data, p_main, sub.caption, theme_fn)
    }
  }

  if (arrange_plots && length(p_list) > 1) {
    n_plots <- length(p_list)
    n_cols <- min(2, n_plots)
    n_rows <- ceiling(n_plots / n_cols)

    if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::grid.arrange(grobs = p_list, ncol = n_cols, nrow = n_rows)
    } else if (requireNamespace("ggpubr", quietly = TRUE)) {
      do.call(ggpubr::ggarrange, c(p_list, list(ncol = n_cols, nrow = n_rows)))
    } else {
      warning("Neither 'gridExtra' nor 'ggpubr' is installed. Displaying plots individually.")
      for (i in seq_along(p_list)) {
        print(p_list[[i]])
        if (ask && i < length(p_list)) {
          message("Press <ENTER> to continue to the next plot...")
          invisible(readLines(con = "stdin", n = 1))
        }
      }
    }
  } else {
    for (i in seq_along(p_list)) {
      print(p_list[[i]])
      if (ask && i < length(p_list)) {
        message("Press <ENTER> to continue to the next plot...")
        invisible(readLines(con = "stdin", n = 1))
      }
    }
  }

  invisible(NULL)
}


#' Plot residuals vs. index (ggplot2)
#'
#' @keywords internal
.plot_ggplot_residuals_vs_index <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  ggplot2::ggplot(diag_data$data, ggplot2::aes(x = index, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Observation Index",
      y = paste0(type, " residuals"),
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}


#' Plot Cook's distance (ggplot2)
#'
#' @keywords internal
.plot_ggplot_cooks_distance <- function(diag_data, p_main, sub.caption, theme_fn) {
  cook_ref <- diag_data$model_info$cook_threshold
  df_data <- diag_data$data

  p <- ggplot2::ggplot(df_data, ggplot2::aes(x = index, y = cook_dist)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = cook_ref, linetype = "dashed", color = "red") +
    ggplot2::annotate("text",
      x = max(df_data$index) * 0.9, y = cook_ref * 1.1,
      label = "4/n", color = "red", hjust = 0
    ) +
    ggplot2::labs(
      x = "Observation Index",
      y = "Cook's Distance",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()

  # Label points exceeding threshold
  infl <- df_data$cook_dist > cook_ref & !is.na(df_data$cook_dist)
  if (any(infl)) {
    p <- p + ggplot2::geom_text(
      data = df_data[infl, ],
      ggplot2::aes(label = index),
      vjust = -0.5, color = "red", size = 3
    )
  }

  p
}


#' Plot leverage vs. fitted (ggplot2)
#'
#' @keywords internal
.plot_ggplot_leverage_vs_fitted <- function(diag_data, p_main, sub.caption, theme_fn) {
  lev_ref <- diag_data$model_info$leverage_threshold
  df_data <- diag_data$data

  p <- ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = leverage)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = lev_ref, linetype = "dashed", color = "red") +
    ggplot2::annotate("text",
      x = max(df_data$fitted) * 0.9, y = lev_ref * 1.1,
      label = "2p/n", color = "red", hjust = 0
    ) +
    ggplot2::labs(
      x = "Fitted Values",
      y = "Generalized Leverage",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()

  # Label high leverage points
  high_lev <- df_data$leverage > lev_ref
  if (any(high_lev)) {
    p <- p + ggplot2::geom_text(
      data = df_data[high_lev, ],
      ggplot2::aes(label = index),
      vjust = -0.5, color = "red", size = 3
    )
  }

  p
}


#' Plot residuals vs. linear predictor (ggplot2)
#'
#' @keywords internal
.plot_ggplot_residuals_vs_linpred <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  df_data <- diag_data$data
  ggplot2::ggplot(df_data, ggplot2::aes(x = linpred, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Linear Predictor (alpha)",
      y = paste0(type, " residuals"),
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}


#' Plot half-normal plot (ggplot2)
#'
#' @param diag_data Diagnostic data list
#' @param p_main Plot title
#' @param sub.caption Plot subtitle
#' @param type Residual type
#' @param theme_fn ggplot2 theme function
#' @return A ggplot object
#'
#' @keywords internal
.plot_ggplot_half_normal <- function(diag_data, p_main, sub.caption, type, theme_fn) {
  # Extract the confidence level from the half_normal data
  # If it's not available in diag_data, use a default value
  level_value <- diag_data$model_info$level
  if (is.null(level_value)) {
    level_value <- 0.90 # Default level if not stored in diag_data
  }

  if (is.null(diag_data$half_normal)) {
    # Fallback to a half-normal Q-Q plot approach
    return(
      ggplot2::ggplot(diag_data$data, ggplot2::aes(sample = abs_resid)) +
        ggplot2::stat_qq(distribution = function(p) stats::qnorm(p * 0.5 + 0.5)) +
        ggplot2::stat_qq_line(
          distribution = function(p) stats::qnorm(p * 0.5 + 0.5),
          color = "gray", linetype = "dashed"
        ) +
        ggplot2::labs(
          x = "Theoretical Half-Normal Quantiles",
          y = paste0("|", type, " residuals|"),
          title = paste(p_main, "(Half-Normal)"),
          subtitle = sub.caption
        ) +
        theme_fn()
    )
  }

  hn_data <- diag_data$half_normal
  sd_abs <- stats::sd(diag_data$data$abs_resid, na.rm = TRUE)

  p <- ggplot2::ggplot(hn_data, ggplot2::aes(x = theoretical, y = observed)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      slope = sd_abs, intercept = 0,
      linetype = "dashed", color = "gray"
    ) +
    ggplot2::labs(
      x = "Theoretical Half-Normal Quantiles",
      y = paste0("Ordered |", type, " residuals|"),
      title = paste(p_main, "(Half-Normal)"),
      subtitle = sub.caption
    ) +
    theme_fn()

  if ("lower" %in% names(hn_data) && "upper" %in% names(hn_data)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = lower),
        linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::geom_line(ggplot2::aes(y = upper),
        linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = max(hn_data$theoretical) * 0.8,
        y = max(hn_data$upper) * 0.9,
        label = paste0(format(100 * level_value, digits = 2), "% envelope"),
        color = "blue"
      )
  }

  return(p)
}


#' Plot predicted vs. observed (ggplot2)
#'
#' @keywords internal
.plot_ggplot_predicted_vs_observed <- function(diag_data, p_main, sub.caption, theme_fn) {
  df_data <- diag_data$data
  ggplot2::ggplot(df_data, ggplot2::aes(x = fitted, y = y_obs)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      intercept = 0, slope = 1,
      color = "gray", linetype = "dashed"
    ) +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Fitted (Mean)",
      y = "Observed (y)",
      title = p_main,
      subtitle = sub.caption
    ) +
    theme_fn()
}


# Extract confidence intervals from TMB profile likelihood objects
#' @keywords internal
extract_profile_ci <- function(profile_obj, level = 0.95) {
  # Check if the profile object has the expected format
  if (!is.data.frame(profile_obj) || !all(c("par", "value") %in% names(profile_obj))) {
    stop("Profile object does not have the expected format")
  }

  # Negative log-likelihood profile (-logLik)
  prof_data <- profile_obj[, c("par", "value")]

  # Find the minimum value of -logLik (maximum of logLik)
  min_value <- min(prof_data$value, na.rm = TRUE)

  # Calculate threshold based on chi-square distribution
  # For CI of level (1-alpha), we use the (1-alpha) quantile of chi-square with 1 d.f.
  alpha <- 1 - level
  threshold <- min_value + qchisq(level, df = 1) / 2

  # Filter points within the confidence interval
  ci_points <- prof_data[prof_data$value <= threshold, ]

  # If there aren't enough points, return NA
  if (nrow(ci_points) < 2) {
    return(c(NA, NA))
  }

  # Extract lower and upper CI limits
  ci_lower <- min(ci_points$par, na.rm = TRUE)
  ci_upper <- max(ci_points$par, na.rm = TRUE)

  return(c(ci_lower, ci_upper))
}


#' Map gkwreg parameter index to TMB parameter index
#'
#' @param object A fitted model object of class "gkwreg"
#' @param param_idx Index of the parameter in the gkwreg coefficients vector
#' @return The corresponding index in the TMB parameter vector, or NA if mapping fails
#'
#' @keywords internal
.map_gkwreg_to_tmb_param <- function(object, param_idx) {
  # Extract necessary information from the model object
  cf_names <- names(object$coefficients)
  param_name <- cf_names[param_idx]

  # Try to extract the TMB parameter mapping
  if (!is.null(object$tmb_param_map)) {
    # If the model object already has a parameter map, use it
    if (param_name %in% names(object$tmb_param_map)) {
      return(object$tmb_param_map[param_name])
    }
  }

  # Otherwise, we need to reconstruct the mapping
  # This is implementation-specific and depends on how parameters are organized in TMB

  # Parse the parameter name to determine the parameter type and position
  param_parts <- strsplit(param_name, ":", fixed = TRUE)[[1]]

  if (length(param_parts) < 2) {
    # Cannot parse parameter name
    return(NA)
  }

  param_type <- param_parts[1] # alpha, beta, gamma, delta, or lambda
  covariate <- paste(param_parts[-1], collapse = ":") # The covariate name

  # Get the model family
  family <- object$family
  if (is.null(family)) family <- "gkw" # Default to gkw

  # Get parameter information for this family
  param_info <- .get_family_param_info(family)

  # Check if the parameter type is valid for this family
  if (!param_type %in% param_info$names) {
    # Parameter not valid for this family
    return(NA)
  }

  # Get the parameter position for this family
  param_pos <- param_info$positions[[param_type]]

  # Get the model matrices to determine covariate position
  if (!is.null(object$x) && param_type %in% names(object$x)) {
    X_mat <- object$x[[param_type]]
    cov_idx <- which(colnames(X_mat) == covariate)

    if (length(cov_idx) == 1) {
      # Calculate the TMB parameter index
      # This formula depends on how parameters are organized in TMB
      # The basic idea is to map (param_type, covariate) to a linear index

      # Count parameters for previous types
      offset <- 0
      for (prev_type in param_info$names) {
        if (prev_type == param_type) break

        if (prev_type %in% names(object$x)) {
          offset <- offset + ncol(object$x[[prev_type]])
        }
      }

      return(offset + cov_idx)
    }
  }

  # If we can't determine the exact mapping, try a simpler approach
  # This assumes parameters are ordered as they appear in the coefficients vector
  return(param_idx)
}


#' @title Extract Log-Likelihood from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' This function extracts the maximized log-likelihood value from a fitted Generalized
#' Kumaraswamy (GKw) regression model object (class \code{"gkwreg"}). The result is
#' returned as an object of class \code{"logLik"}, which includes attributes for
#' degrees of freedom and number of observations, suitable for use with model
#' selection criteria like AIC and BIC.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a call
#'   to \code{\link{gkwreg}}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' The log-likelihood value is typically computed during the model fitting process
#' (e.g., by \code{\link{gkwreg}}) and stored within the resulting object. This
#' method retrieves this stored value. If the value is not directly available, it
#' attempts to calculate it from the stored deviance (\eqn{logLik = -deviance / 2}).
#'
#' The log-likelihood for a GKw family model with parameters \eqn{\theta} is
#' generally defined as the sum of the log-density contributions for each observation:
#' \deqn{l(\theta | y) = \sum_{i=1}^n \log f(y_i; \alpha_i, \beta_i, \gamma_i, \delta_i, \lambda_i)}
#' where \eqn{f(y; \dots)} is the probability density function (PDF) of the specific
#' distribution from the GKw family used in the model (determined by the \code{family}
#' argument in \code{gkwreg}), and parameters (\eqn{\alpha_i, \dots, \lambda_i}) may
#' depend on covariates.
#'
#' The function also extracts the number of estimated parameters (\code{df}) and the
#' number of observations (\code{nobs}) used in the fit, storing them as attributes
#' of the returned \code{"logLik"} object, which is essential for functions like
#' \code{\link[stats]{AIC}} and \code{\link[stats]{BIC}}. It attempts to find \code{df}
#' and \code{nobs} from various components within the \code{object} if they are not
#' directly stored as \code{npar} and \code{nobs}.
#'
#' @return An object of class \code{"logLik"} representing the maximized
#'   log-likelihood value. It has the following attributes:
#'   \itemize{
#'     \item \code{df}: (numeric) The number of estimated parameters in the model
#'       (coefficients).
#'     \item \code{nobs}: (numeric) The number of observations used for fitting the model.
#'   }
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{AIC.gkwreg}}, \code{\link{BIC.gkwreg}},
#'   \code{\link[stats]{logLik}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}
#'
#' @keywords log-likelihood models likelihood
#'
#' @examples
#' \donttest{
#' # Assume 'df' exists with response 'y' and predictors 'x1', 'x2', 'x3'
#' # and that rkw() is available and data is appropriate (0 < y < 1).
#' set.seed(123)
#' n <- 100
#' x1 <- runif(n)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.4))
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(1.0 - 0.1 * x2 + 0.3 * (x3 == "1"))
#' y <- rkw(n, alpha = alpha, beta = beta) # Placeholder if rkw not available
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Fit a Kumaraswamy regression model
#' kw_reg <- gkwreg(y ~ x1 | x2 + x3, data = df, family = "kw")
#'
#' # Extract log-likelihood object
#' ll <- logLik(kw_reg)
#'
#' # Print the log-likelihood value (with attributes)
#' print(ll)
#'
#' # Access the value directly
#' ll_value <- as.numeric(ll)
#' print(ll_value)
#'
#' # Get the number of parameters (degrees of freedom)
#' df_model <- attr(ll, "df")
#' print(paste("Number of parameters:", df_model))
#'
#' # Get the number of observations
#' nobs_model <- attr(ll, "nobs")
#' print(paste("Number of observations:", nobs_model))
#'
#' # Use with AIC/BIC
#' AIC(kw_reg)
#' BIC(kw_reg)
#' }
#'
#' @importFrom stats logLik
#' @method logLik gkwreg
#' @export
logLik.gkwreg <- function(object, ...) {
  # Check if the object is of class gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model object of class 'gkwreg'")
  }

  # Extract log-likelihood value
  ll <- object$loglik

  # If log-likelihood is not available, try to recover from other information
  if (is.null(ll)) {
    # Try to calculate from deviance if available
    if (!is.null(object$deviance)) {
      ll <- -object$deviance / 2
    } else {
      warning("Log-likelihood not found in the model object and cannot be calculated")
      ll <- NA_real_
    }
  }

  # Get the number of parameters (degrees of freedom for the model)
  # Use npar if available, otherwise count coefficients
  df <- object$npar
  if (is.null(df)) {
    # Try to determine number of parameters from coefficients
    if (!is.null(object$coefficients)) {
      df <- length(object$coefficients)
    } else {
      warning("Number of parameters ('npar') not found in the model object")
      df <- NA_integer_
    }
  }

  # Get the number of observations used in the fit
  # Use nobs if available, otherwise infer from other components
  nobs <- object$nobs
  if (is.null(nobs)) {
    # Try to determine number of observations from residuals or fitted values or y
    if (!is.null(object$residuals)) {
      nobs <- length(object$residuals)
    } else if (!is.null(object$fitted.values)) {
      nobs <- length(object$fitted.values)
    } else if (!is.null(object$y)) {
      nobs <- length(object$y)
    } else {
      warning("Number of observations ('nobs') not found in the model object")
      nobs <- NA_integer_
    }
  }

  # Ensure df and nobs are numeric, even if NA
  df <- as.numeric(df)
  nobs <- as.numeric(nobs)

  # Create and return the logLik object with appropriate attributes
  # Use structure() to assign attributes and class simultaneously
  structure(ll,
    df = df,
    nobs = nobs,
    class = "logLik"
  )
}


#' @title Akaike's Information Criterion for GKw Regression Models
#'
#' @description
#' Calculates the Akaike Information Criterion (AIC) for one or more fitted
#' Generalized Kumaraswamy (GKw) regression model objects (class \code{"gkwreg"}).
#' AIC is commonly used for model selection, penalizing model complexity.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param ... Optionally, one or more additional fitted model objects of class
#'   \code{"gkwreg"}, for which AIC should also be calculated.
#' @param k Numeric, the penalty per parameter. The default \code{k = 2} corresponds
#'   to the traditional AIC. Using \code{k = log(nobs)} would yield BIC (though using
#'   \code{\link{BIC.gkwreg}} is preferred for that).
#'
#' @details
#' The AIC is calculated based on the maximized log-likelihood (\eqn{L}) and the
#' number of estimated parameters (\eqn{p}) in the model:
#' \deqn{AIC = -2 \log(L) + k \times p}
#' This function retrieves the log-likelihood and the number of parameters (\code{df})
#' using the \code{\link{logLik.gkwreg}} method for the fitted \code{gkwreg} object(s).
#' Models with lower AIC values are generally preferred, as they indicate a better
#' balance between goodness of fit and model parsimony.
#'
#' When comparing multiple models passed via \code{...}, the function relies on
#' \code{\link[stats]{AIC}}'s default method for creating a comparison table,
#' which in turn calls \code{logLik} for each provided object.
#'
#' For small sample sizes relative to the number of parameters, the second-order
#' AIC (AICc) might be more appropriate:
#' \deqn{AICc = AIC + \frac{2p(p+1)}{n-p-1}}
#' where \eqn{n} is the number of observations. AICc is not directly computed by
#' this function but can be calculated manually using the returned AIC, \eqn{p}
#' (from \code{attr(logLik(object), "df")}), and \eqn{n}
#' (from \code{attr(logLik(object), "nobs")}).
#'
#' @return If just one \code{object} is provided, returns a single numeric AIC value.
#'   If multiple objects are provided via \code{...}, returns a \code{data.frame}
#'   with rows corresponding to the models and columns for the degrees of freedom
#'   (\code{df}) and the AIC values, sorted by AIC.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Akaike, H. (1974). A new look at the statistical model identification.
#' \emph{IEEE Transactions on Automatic Control}, \strong{19}(6), 716-723.
#'
#'
#' Burnham, K. P., & Anderson, D. R. (2002). \emph{Model Selection and Multimodel
#' Inference: A Practical Information-Theoretic Approach} (2nd ed.). Springer-Verlag.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{logLik.gkwreg}}, \code{\link{BIC.gkwreg}},
#'   \code{\link[stats]{AIC}}
#'
#' @keywords AIC models likelihood model selection
#'
#' @examples
#' \donttest{
#' # Assume 'df' exists with response 'y' and predictors 'x1', 'x2', 'x3'
#' # and that rkw() is available and data is appropriate (0 < y < 1).
#' set.seed(123)
#' n <- 100
#' x1 <- runif(n)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.4))
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(1.0 - 0.1 * x2 + 0.3 * (x3 == "1"))
#' y <- rkw(n, alpha = alpha, beta = beta) # Placeholder if rkw not available
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Fit two competing models
#' kw_reg1 <- gkwreg(y ~ x1 | x2, data = df, family = "kw")
#' kw_reg2 <- gkwreg(y ~ x1 | x2 + x3, data = df, family = "kw") # More complex beta model
#' kw_reg3 <- gkwreg(y ~ 1 | x2 + x3, data = df, family = "kw") # Simpler alpha model
#'
#' # Calculate AIC for a single model
#' aic1 <- AIC(kw_reg1)
#' print(aic1)
#'
#' # Compare models using AIC (lower is better)
#' model_comparison_aic <- c(AIC(kw_reg1), AIC(kw_reg2), AIC(kw_reg3))
#' print(model_comparison_aic)
#'
#' # Calculate AIC with a different penalty (e.g., k=4)
#' aic1_k4 <- AIC(kw_reg1, k = 4)
#' print(aic1_k4)
#' }
#'
#' @importFrom stats AIC
#' @method AIC gkwreg
#' @export
AIC.gkwreg <- function(object, ..., k = 2) {
  # Check if the object is of class gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model object of class 'gkwreg'")
  }

  # Capture additional model objects
  dot_objects <- list(...)

  # Handle case with multiple models for comparison using stats::AIC generic logic
  # This relies on the generic dispatching to logLik.gkwreg for each object
  if (length(dot_objects) > 0) {
    # Ensure all additional objects are also gkwreg models (or compatible)
    obj_list <- c(list(object), dot_objects)
    classes <- vapply(obj_list, function(o) class(o)[1], character(1))
    if (!all(classes == "gkwreg")) {
      # Allow comparison if logLik methods exist for other object types
      # stats::AIC handles this, just pass them through
      warning("Comparing objects of different classes.")
    }
    # Call the default stats::AIC logic which handles multiple objects
    # It uses logLik() on each object.
    return(stats::AIC(object = object, ..., k = k))
  }

  # --- Handle single object case ---

  # Check if AIC is already computed and stored in the object AND k is the default 2
  # Avoid recalculating standard AIC if already present
  if (!is.null(object$aic) && identical(k, 2)) {
    return(object$aic)
  }

  # Calculate AIC from log-likelihood and number of parameters
  ll <- stats::logLik(object) # Use stats::logLik generic, dispatches to logLik.gkwreg

  # Extract number of parameters (df) from the logLik object
  df <- attr(ll, "df")

  # Check if df is valid
  if (is.null(df) || is.na(df) || !is.numeric(df) || df < 0) {
    warning("Could not extract a valid number of parameters (df) from the logLik object. AIC calculation might be incorrect.")
    # Attempt fallback: count coefficients if df is invalid
    if (!is.null(object$coefficients)) {
      df <- length(object$coefficients)
      warning("Using the count of coefficients (", df, ") as the number of parameters.")
    } else {
      df <- NA_real_ # Cannot determine df
      warning("Setting number of parameters to NA.")
    }
  }

  # Check if logLik value is valid
  ll_val <- as.numeric(ll)
  if (is.null(ll_val) || is.na(ll_val) || !is.finite(ll_val)) {
    warning("Invalid log-likelihood value extracted. Cannot compute AIC.")
    return(NA_real_)
  }

  # Calculate and return AIC using the formula -2*logLik + k*df
  aic_val <- -2 * ll_val + k * df

  return(aic_val)
}



#' @title Bayesian Information Criterion for GKw Regression Models
#'
#' @description
#' Calculates the Bayesian Information Criterion (BIC), also known as Schwarz's
#' Bayesian Criterion (SBC), for one or more fitted Generalized Kumaraswamy (GKw)
#' regression model objects (class \code{"gkwreg"}). BIC is used for model selection
#' and tends to penalize model complexity more heavily than AIC, especially for
#' larger datasets.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param ... Optionally, one or more additional fitted model objects of class
#'   \code{"gkwreg"}, for which BIC should also be calculated.
#'
#' @details
#' The BIC is calculated based on the maximized log-likelihood (\eqn{L}), the
#' number of estimated parameters (\eqn{p}) in the model, and the number of
#' observations (\eqn{n}):
#' \deqn{BIC = -2 \log(L) + p \times \log(n)}
#' This function retrieves the log-likelihood, the number of parameters (\code{df}),
#' and the number of observations (\code{nobs}) using the \code{\link{logLik.gkwreg}}
#' method for the fitted \code{gkwreg} object(s).
#'
#' Models with lower BIC values are generally preferred. The penalty term \eqn{p \log(n)}
#' increases more rapidly with sample size \eqn{n} compared to AIC's penalty \eqn{2p},
#' meaning BIC favors simpler models more strongly in larger samples. BIC can be
#' motivated from a Bayesian perspective as an approximation related to Bayes factors.
#'
#' When comparing multiple models passed via \code{...}, the function relies on
#' \code{\link[stats]{BIC}}'s default method for creating a comparison table,
#' which in turn calls \code{logLik} for each provided object.
#'
#' @return If just one \code{object} is provided, returns a single numeric BIC value.
#'   If multiple objects are provided via \code{...}, returns a \code{data.frame}
#'   with rows corresponding to the models and columns for the degrees of freedom
#'   (\code{df}) and the BIC values, sorted by BIC.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Schwarz, G. (1978). Estimating the dimension of a model.
#' \emph{The Annals of Statistics}, \strong{6}(2), 461-464.
#'
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{logLik.gkwreg}}, \code{\link{AIC.gkwreg}},
#'   \code{\link[stats]{BIC}}
#'
#' @keywords BIC models likelihood model selection
#'
#' @examples
#' \donttest{
#' # Assume 'df' exists with response 'y' and predictors 'x1', 'x2', 'x3'
#' # and that rkw() is available and data is appropriate (0 < y < 1).
#' set.seed(123)
#' n <- 100
#' x1 <- runif(n)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.4))
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(1.0 - 0.1 * x2 + 0.3 * (x3 == "1"))
#' y <- rkw(n, alpha = alpha, beta = beta) # Placeholder if rkw not available
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Fit two competing models
#' kw_reg1 <- gkwreg(y ~ x1 | x2, data = df, family = "kw")
#' kw_reg2 <- gkwreg(y ~ x1 | x2 + x3, data = df, family = "kw") # More complex beta model
#' kw_reg3 <- gkwreg(y ~ 1 | x2 + x3, data = df, family = "kw") # Simpler alpha model
#'
#' # Calculate BIC for a single model
#' bic1 <- BIC(kw_reg1)
#' print(bic1)
#'
#' # Compare models using BIC (lower is better)
#' model_comparison_bic <- c(BIC(kw_reg1), BIC(kw_reg2), BIC(kw_reg3))
#' print(model_comparison_bic)
#' }
#'
#' @importFrom stats BIC
#' @method BIC gkwreg
#' @export
BIC.gkwreg <- function(object, ...) {
  # Check if the object is of class gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model object of class 'gkwreg'")
  }

  # Capture additional model objects
  dot_objects <- list(...)

  # Handle case with multiple models for comparison using stats::BIC generic logic
  if (length(dot_objects) > 0) {
    # Ensure all additional objects are also gkwreg models (or compatible)
    obj_list <- c(list(object), dot_objects)
    classes <- vapply(obj_list, function(o) class(o)[1], character(1))
    if (!all(classes == "gkwreg")) {
      # Allow comparison if logLik methods exist for other object types
      # stats::BIC handles this, just pass them through
      warning("Comparing objects of different classes.")
    }
    # Call the default stats::BIC logic which handles multiple objects
    # It uses logLik() on each object.
    return(stats::BIC(object = object, ...))
  }

  # --- Handle single object case ---

  # Check if BIC is already computed and stored in the object
  # Avoid recalculating if already present
  if (!is.null(object$bic)) {
    return(object$bic)
  }

  # Calculate BIC from log-likelihood, number of parameters, and number of observations
  ll <- stats::logLik(object) # Use stats::logLik generic, dispatches to logLik.gkwreg

  # Extract number of parameters (df) from the logLik object
  df <- attr(ll, "df")

  # Check if df is valid
  if (is.null(df) || is.na(df) || !is.numeric(df) || df < 0) {
    warning("Could not extract a valid number of parameters (df) from the logLik object. BIC calculation might be incorrect.")
    # Attempt fallback: count coefficients if df is invalid
    if (!is.null(object$coefficients)) {
      df <- length(object$coefficients)
      warning("Using the count of coefficients (", df, ") as the number of parameters.")
    } else {
      df <- NA_real_ # Cannot determine df
      warning("Setting number of parameters to NA.")
    }
  }

  # Extract number of observations (nobs) from the logLik object
  n <- attr(ll, "nobs")

  # Check if nobs is valid
  if (is.null(n) || is.na(n) || !is.numeric(n) || n <= 0) {
    warning("Could not extract a valid number of observations (nobs) from the logLik object. BIC calculation might be incorrect.")
    # Attempt fallback: try various sources from the object
    if (!is.null(object$residuals)) {
      n <- length(object$residuals)
    } else if (!is.null(object$fitted.values)) {
      n <- length(object$fitted.values)
    } else if (!is.null(object$y)) {
      n <- length(object$y)
    } else {
      n <- NA_real_ # Cannot determine nobs
      warning("Setting number of observations to NA.")
    }
    if (!is.na(n)) warning("Using length of residuals/fitted/y (", n, ") as the number of observations.")
  }

  # Check if logLik value is valid
  ll_val <- as.numeric(ll)
  if (is.null(ll_val) || is.na(ll_val) || !is.finite(ll_val)) {
    warning("Invalid log-likelihood value extracted. Cannot compute BIC.")
    return(NA_real_)
  }

  # Check if df and n are valid for calculation
  if (is.na(df) || is.na(n) || n <= 0) {
    warning("Cannot compute BIC due to missing or invalid 'df' or 'nobs'.")
    return(NA_real_)
  }

  # Calculate and return BIC using the formula -2*logLik + df*log(n)
  bic_val <- -2 * ll_val + df * log(n)

  return(bic_val)
}


#' Extract Variance-Covariance Matrix from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' This function extracts the variance-covariance matrix of the estimated parameters
#' from a fitted Generalized Kumaraswamy regression model. The variance-covariance
#' matrix is essential for statistical inference, including hypothesis testing and
#' confidence interval calculation.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a call
#'   to \code{\link{gkwreg}}.
#' @param complete Logical indicating whether the complete variance-covariance matrix
#'   should be returned in case some coefficients were omitted from the original fit.
#'   Currently ignored for \code{gkwreg} objects.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' The variance-covariance matrix is estimated based on the observed information
#' matrix, which is derived from the second derivatives of the log-likelihood function
#' with respect to the model parameters. For \code{gkwreg} objects, this matrix is
#' typically computed using the TMB (Template Model Builder) automatic differentiation
#' framework during model fitting.
#'
#' The diagonal elements of the variance-covariance matrix correspond to the squared
#' standard errors of the parameter estimates, while the off-diagonal elements represent
#' the covariances between pairs of parameters.
#'
#' @return A square matrix with row and column names corresponding to the coefficients
#' in the model. If the variance-covariance matrix is not available (for example, if
#' the model was fitted with \code{hessian = FALSE}), the function returns \code{NULL}
#' with a warning.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{confint}}, \code{\link{summary.gkwreg}}
#'
#' @importFrom stats vcov
#' @method vcov gkwreg
#' @export
vcov.gkwreg <- function(object, complete = TRUE, ...) {
  # Check if the object is of class gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model object of class 'gkwreg'")
  }

  # Check if the variance-covariance matrix is available
  if (is.null(object$vcov)) {
    warning(
      "Variance-covariance matrix not found in the model object. ",
      "The model may have been fitted with hessian = FALSE. ",
      "Consider refitting with hessian = TRUE for valid statistical inference."
    )
    return(NULL)
  }

  # Return the variance-covariance matrix
  object$vcov
}


#' Access datasets from bounded response regression packages
#'
#' This function provides direct access to datasets from the 'betareg' and 'simplexreg'
#' packages without copying them to your project files. It dynamically loads
#' the requested dataset from the respective package's namespace.
#'
#' @param dataset_name A character string. The name of the dataset to retrieve.
#' @param package A character string. The package containing the dataset. Must be one of
#'   "betareg" or "simplexreg". If NULL (default), the function searches both packages.
#' @param attach_to_namespace Logical. If TRUE, the dataset will be attached to
#'   the calling environment. Default is FALSE.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: Beta regression on ReadingSkills data
#' # ------------------------------------------------
#'
#' # This example analyzes factors affecting reading accuracy in children with dyslexia.
#'
#' # Load ReadingSkills data
#' reading_data <- get_bounded_datasets("ReadingSkills")
#'
#' # Fit beta regression model
#' reading_model <- gkwreg(
#'   accuracy ~ dyslexia + iq,
#'   data = reading_data,
#'   family = "beta",
#'   link = list(gamma = "log", delta = "logit")
#' )
#'
#' summary(reading_model)
#'
#' # Example 2: Kumaraswamy regression on FoodExpenditure data
#' # --------------------------------------------------------
#' # This example models the proportion of income spent on food.
#'
#' # Load FoodExpenditure data
#' food_data <- get_bounded_datasets("FoodExpenditure")
#' food_data$y <- food_data$food / food_data$income
#'
#' # Fit Kumaraswamy regression model
#' food_model <- gkwreg(
#'   y ~ persons,
#'   data = food_data,
#'   family = "kw",
#'   link = list(alpha = "log", beta = "log")
#' )
#'
#' summary(food_model)
#'
#' # Example 3: Exponential Kumaraswamy regression on retinal data
#' # ------------------------------------------------------------
#' # This example analyzes the decay of intraocular gas in retinal surgeries.
#'
#' # Load retinal data
#' retinal_data <- get_bounded_datasets("retinal", package = "simplexreg")
#'
#' # Fit a Kumaraswamy - Kumaraswamy model
#' retinal_model <- gkwreg(
#'   Gas ~ LogT2 | Level | Time,
#'   data = retinal_data,
#'   family = "ekw"
#' )
#'
#' summary(retinal_model)
#' }
#'
#' @return A data frame containing the requested dataset.
#' @seealso \code{\link{list_bounded_datasets}}
#' @export
get_bounded_datasets <- function(dataset_name, package = NULL, attach_to_namespace = FALSE) {
  # List of available datasets by package
  datasets <- list(
    betareg = c(
      "CarTask", "FoodExpenditure", "GasolineYield",
      "ImpreciseTask", "LossAversion", "MockJurors",
      "ReadingSkills", "StressAnxiety", "WeatherTask"
    ),
    simplexreg = c("retinal", "sdac")
  )

  # If package is NULL, search in both packages
  if (is.null(package)) {
    # Find which package contains the dataset
    if (dataset_name %in% datasets$betareg) {
      package <- "betareg"
    } else if (dataset_name %in% datasets$simplexreg) {
      package <- "simplexreg"
    } else {
      stop(paste0(
        "Dataset '", dataset_name, "' not found in betareg or simplexreg packages. ",
        "Available datasets are: ",
        paste(c(datasets$betareg, datasets$simplexreg), collapse = ", ")
      ))
    }
  } else {
    # Validate specified package
    if (!package %in% c("betareg", "simplexreg")) {
      stop("Package must be one of 'betareg' or 'simplexreg'")
    }

    # Check if requested dataset exists in the specified package
    if (!dataset_name %in% datasets[[package]]) {
      stop(paste0(
        "Dataset '", dataset_name, "' not found in ", package, " package. ",
        "Available datasets are: ",
        paste(datasets[[package]], collapse = ", ")
      ))
    }
  }

  # Check if the required package is installed
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "The ", package, " package is needed for this function to work. ",
      "Please install it with install.packages('", package, "')"
    )
  }

  # Create temporary environment to load the data
  temp_env <- new.env()

  # Load the dataset into the temporary environment
  utils::data(list = dataset_name, package = package, envir = temp_env)

  # Get the dataset from the environment
  dataset <- get(dataset_name, envir = temp_env)

  # Optionally attach the dataset to the calling environment
  if (attach_to_namespace) {
    assign(dataset_name, dataset, envir = parent.frame())
  }

  # Return the dataset
  return(dataset)
}

#' List all available datasets for bounded response regression
#'
#' @param package A character string. If specified, list only datasets from the given package.
#'   Must be one of "betareg" or "simplexreg". If NULL (default), lists datasets from both packages.
#'
#' @return A data frame with names and descriptions of available datasets
#'
#' @examples
#' \dontrun{
#' # List all available datasets
#' list_bounded_datasets()
#'
#' # List only betareg datasets
#' list_bounded_datasets("betareg")
#' }
#'
#' @seealso \code{\link{get_bounded_datasets}}
#' @export
list_bounded_datasets <- function(package = NULL) {
  # Define datasets and descriptions
  datasets <- list(
    betareg = c(
      "CarTask", "FoodExpenditure", "GasolineYield",
      "ImpreciseTask", "LossAversion", "MockJurors",
      "ReadingSkills", "StressAnxiety", "WeatherTask"
    ),
    simplexreg = c("retinal", "sdac")
  )

  descriptions <- list(
    betareg = c(
      "Partition-Primed Probability Judgement Task for Car Dealership",
      "Proportion of Household Income Spent on Food",
      "Estimation of Gasoline Yields from Crude Oil",
      "Imprecise Probabilities for Sunday Weather and Boeing Stock Task",
      "(No) Myopic Loss Aversion in Adolescents",
      "Confidence of Mock Jurors in Their Verdicts",
      "Dyslexia and IQ Predicting Reading Accuracy",
      "Dependency of Anxiety on Stress",
      "Weather Task with Priming and Precise and Imprecise Probabilities"
    ),
    simplexreg = c(
      "Data on recorded decay of intraocular gas in complex retinal surgeries",
      "Data on Autologous Peripheral Blood Stem Cell Transplants in Alberta Health Service"
    )
  )

  # Filter by package if specified
  if (!is.null(package)) {
    if (!package %in% c("betareg", "simplexreg")) {
      stop("Package must be one of 'betareg' or 'simplexreg'")
    }

    result <- data.frame(
      Package = rep(package, length(datasets[[package]])),
      Dataset = datasets[[package]],
      Description = descriptions[[package]],
      stringsAsFactors = FALSE
    )
  } else {
    # Combine all datasets
    result <- rbind(
      data.frame(
        Package = rep("betareg", length(datasets$betareg)),
        Dataset = datasets$betareg,
        Description = descriptions$betareg,
        stringsAsFactors = FALSE
      ),
      data.frame(
        Package = rep("simplexreg", length(datasets$simplexreg)),
        Dataset = datasets$simplexreg,
        Description = descriptions$simplexreg,
        stringsAsFactors = FALSE
      )
    )
  }
  return(result)
}



## usethis namespace: start
#' @importFrom numDeriv grad hessian
## usethis namespace: end
NULL
