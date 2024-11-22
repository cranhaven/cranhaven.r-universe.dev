#' merlin - Mixed Effects Regression for Linear, Nonlinear and User-defined models
#'
#' merlin fits linear, non-linear and user-defined mixed effects regression models. merlin can fit
#' multivariate outcome models of any type, each of which could be repeatedly measured
#' (longitudinal), with any number of levels, and with any number of random effects at each level.
#' Standard distributions/models available include the Bernoulli, Gaussian, Poisson, beta,
#' negative-binomial, and time-to-event/survival models include the exponential, Gompertz,
#' Weibull, Royston-Parmar, and general hazard model. merlin provides a
#' flexible predictor syntax, allowing the user to define variables, random effects, spline and
#' fractional polynomial functions, functions of other outcome models, and any interaction
#' between each of them. Non-linear and time-dependent effects are seamlessly incorporated into
#' the predictor. merlin allows multivariate normal random effects, which are integrated out
#' using Gaussian quadrature or Monte-Carlo integration. Relative survival (excess hazard) models
#' are supported. Utility functions are provided to allow user-defined models to be specified,
#' in conjunction with the complex predictor.
#'
#' @author Emma C. Martin, Alessandro Gasparini and Michael J. Crowther
#'
#' @param model specify the fixed and random elements for each model outcome.
#'   Where there are multiple outcomes, the models should be specified in a list.
#'   Each model should be specified as a formula (e.g. \code{y~x}).
#'   A number of different element types can be specified, including
#'   \itemize{
#'     \item varname - an independent variable from the data set
#'     \item random-effects - a random-effect at the cluster level can be specified using \code{M#[cluster level]}, for example \code{M1[id]} would define a random intercept at the ID level.
#'     Each independent random-effect should be given a unique name, if two random-effects are given the same name they will be treated as shared random-effects.
#'     \item \code{rcs()} - restricted cubic spline terms, this option can be used to include a restricted cubic spline function, with the degrees of freedom (number of spline terms) specified using the \code{df} sub-option, with the boundary knots assumed to be at the minimum and maximum of the variable, with internal knots placed at equally spaced centiles.
#'     Other default options \code{orthog = TRUE}, which by default orthogonalises the splines, \code{log = FALSE}, which can be used to calculate splines of the log of the variable and \code{event = F}, which can be used to calculate the internal knots based only on observations that experienced the event of interest (for survival models).
#'     \item \code{srcs()} is a shorthand element, equivalent to \code{rcs(..., log = TRUE, event = TRUE)}, for use with survival models.
#'     \item \code{fp()} - fractional polynomials of order 1 or 2 can be specified, the sub-option \code{powers} is used to specify the powers of the \code{fp} model.
#'     \item \code{EV[depvar]} - the expected value of the response of a submodel.
#'     \item \code{dEV[depvar]} - the first derivative with respect to time of the expected value of the response of a submodel.
#'     \item \code{d2EV[depvar]} - the second derivative with respect to time of the expected value of the response of a submodel.
#'     \item \code{iEV[depvar]} - the integral with respect to time of the expected value of the response of a submodel.
#'     \item \code{XB[depvar]} - the expected value of the complex predictor of a submodel.
#'     \item \code{dXB[depvar]} - the first derivative with respect to time of the expected value of the complex predictor of a submodel.
#'     \item \code{d2XB[depvar]} - the second derivative with respect to time of the expected value of the complex predictor of a submodel.
#'     \item \code{iXB[depvar]} - the integral with respect to time of the expected value of the complex predictor of a submodel.
#'     \item \code{bhazard(varname)} - invokes a relative survival (excess hazard) model.
#'     \code{varname} specifies the expected hazard rate at the event time.
#'     \item \code{exposure(varname)} - include \code{log(varname)} in the linear predictor, with a coefficient of 1.
#'     For use with \code{family = "poisson"}.
#'     \item \code{ap(#)} - defines the number of ancillary parameters.
#'     Used with \code{family="user"}.
#'   }
#' @param family a vector of strings specifying the family for each outcome specified in model.
#' The currently available models include,
#' \itemize{
#'   \item \code{gaussian} - Gaussian distribution
#'   \item \code{bernoulli} - Bernoulli distribution
#'   \item \code{poisson} - Poisson distribution
#'   \item \code{beta} - Beta distribution
#'   \item \code{negbinomial} - Negative binomial distribution
#' }
#' with survival models including,
#' \itemize{
#'   \item \code{exponential} - exponential distribution
#'   \item \code{weibull} - Weibull distribution
#'   \item \code{gompertz} - Gompertz distribution
#'   \item \code{rp} - Royston-Parmar model (complex predictor on the log cumulative hazard scale)
#'   \item \code{loghazard} - general log hazard model (complex predictor on the log hazard scale)
#' }
#' and user-defined,
#' \itemize{
#'   \item \code{user} - fit a user-defined model, which should be written using \code{merlin}'s utility functions.
#'   The name of your user-defined function should be passed through the \code{userf} option.
#'   \item \code{null} - is a convenience tool to define additional complex predictors, that do not contribute to the log likelihood.
#'   For use with \code{family = "user"}.
#' }
#' @param link string vector defining the link functions for each model.
#' Default is \code{"identity"} for all models.
#' If specified, you must define a link function for all submodels.
#' Options include \code{"identity"}, \code{"log"} and \code{"logit"}.
#' @param userf string vector defining the name of the user-written functions for each \code{family="user"}.
#' Each function must be in memory and should return the observation level contribution to the log-likelihood.
#' @param timevar specifies the variable which represents time, this is necessary when a function of time is used in the linear predictor of a survival model as it may interact with other elements of the model.
#' @param data a data frame containing all variables required for fitting the model.
#' Can be a \code{tibble} object.
#' @param covariance the structure of the variance-covariance matrix can be varied, the default is \code{diagonal} where all diagonal elements of the variance-covariance matrix are estimated uniquely, \code{identity} assumes all diagonal elements are equal and \code{unstructured} estimates all elements of the variance-covariance matrix.
#' @param levels if the model contains random-effects then a vector giving the order of levels must be specified, from the highest level to the lowest, e.g. \code{levels=c("practice","id")}.
#' @param from this is an optional argument giving the initial values for the full parameter vector, for more details on how to specify the initial estimates see the vignette.
#' @param sweights Not documented.
#' @param predict Not documented.
#' @param predtype Not documented.
#' @param predmodel Not documented.
#' @param causes Not documented.
#' @param at Not documented.
#' @param contrast Not documented.
#' @param modelfit Not documented.
#' @param control A list of parameters that control the estimation algorithm.
#' Generally it should not be modified, unless there are convergence issues.
#' Possible values are:
#' \itemize{
#'   \item \code{ip} An optional vector of integers specifying the number of integration points to be used when integrating out the random effects.
#'   A different number of \code{ip} can be specified for each \code{level} (from highest to lowest level).
#'   If only a single number is given then \code{merlin} will assume that number of integration points at all levels.
#'   Default is \code{ip = 7} for each level using Gauss-Hermite quadrature, or \code{ip = 100} for each level using Monte-Carlo integration;
#'   \item \code{intmethod} The method used for numerically integrating out the random-effects in order to calculate the likelihood for a mixed effects model which includes random effects.
#'   Options include \code{ghermite} for non-adaptive Gauss-Hermite quadrature, \code{halton} for Monte-Carlo integration using Halton sequences, \code{sobol} for Monte-Carlo integration using Sobol sequences, or \code{mc} for standard Monte-Carlo integration using normal draws.
#'   The default is \code{ghermite}.
#'   Level-specific integration techniques can be specified, for example, with a three level model, we may use Gauss-Hermite quadrature at the highest level, and Monte-Carlo integration with Halton sequences at level 2, using \code{intmethod = c("ghermite","halton")}.
#'   \item \code{debug} Not documented.
#'   \item \code{verbose} Not documented.
#'   \item \code{optim.method} The \code{optim} method to be used.
#'   Defaults to Nelder-Mead, see \link[stats]{optim} for available methods.
#'   \item \code{maxit} The maximum number of iterations for the optimisation routine.
#'   Defaults to 5000.
#' }
#' @seealso \code{\link{predict.merlin}}
#' @seealso \code{\link{merlin_util_depvar}}, \code{\link{merlin_util_timevar}},
#' \code{\link{merlin_util_xzb}}, \code{\link{merlin_util_xzb_mod}}
#' \code{\link{merlin_util_xzb_deriv}}, \code{\link{merlin_util_xzb_deriv_mod}}
#' \code{\link{merlin_util_xzb_deriv2}}, \code{\link{merlin_util_xzb_deriv2_mod}}
#' \code{\link{merlin_util_xzb_integ}}, \code{\link{merlin_util_xzb_integ_mod}}
#' \code{\link{merlin_util_ev}}, \code{\link{merlin_util_ev_mod}}
#' \code{\link{merlin_util_ev_deriv}}, \code{\link{merlin_util_ev_deriv_mod}}
#' \code{\link{merlin_util_ev_deriv2}}, \code{\link{merlin_util_ev_deriv2_mod}}
#' \code{\link{merlin_util_ev_integ}}, \code{\link{merlin_util_ev_integ_mod}}
#' \code{\link{merlin_util_ap}}, \code{\link{merlin_util_ap_mod}}
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear, non-linear and user-defined models.
#'
#' @examples
#' \dontrun{
#' library(merlin)
#' data(pbc.merlin, package = "merlin")
#'
#' # Linear fixed-effects model
#' merlin(logb ~ year,
#'        family = "gaussian",
#'        data = pbc.merlin)
#'
#' # Linear mixed-effects model with random intercept and slope at ID level
#' merlin(logb ~ year + M1[id] * 1 + year:M2[id] * 1,
#'        family = "gaussian",
#'        levels = "id",
#'        data = pbc.merlin)
#'
#' # Joint longitudinal and survival model with shared random effects
#' merlin(model = list(logb ~ year + M1[id] * 1,
#'                     Surv(stime, died) ~ trt + M1[id]),
#'        family = c("gaussian", "weibull"),
#'        levels = "id",
#'        data = pbc.merlin)
#'
#' # Joint longitudinal and survival model with expected value
#' merlin(model = list(logb ~ year + M1[id] * 1,
#'                     Surv(stime, died) ~ trt + EV[logb]),
#'        family = c("gaussian", "weibull"),
#'        levels = "id",
#'        timevar = c("year","stime"),
#'        data = pbc.merlin)
#'
#' # Gaussian distribution - implemented as a user-written family
#' logl_gaussian <- function(gml)
#' {
#'   y    <- merlin_util_depvar(gml)
#'   xzb  <- merlin_util_xzb(gml)
#'   se   <- exp(merlin_util_ap(gml,1))
#'
#'   mu   <- (sweep(xzb,1,y,"-"))^2
#'   logl <- ((-0.5 * log(2*pi) - log(se)) - (mu/(2 * se^2)))
#'   return(logl)
#' }
#'
#' merlin(logb ~ year + ap(1), family = "user", data = pbc.merlin,
#'                                   userf = "logl_gaussian")
#'
#' # 3-level Weibull model
#' merlin(Surv(stime1,dead1) ~ age + M1[id1]*1 + M2[id2]*1,
#'        levels=c("id1","id2"), family="weibull", data=sim3)
#' }

#' @export
merlin <- function(model,
                   from = NULL,
                   family = "gaussian",
                   link = NULL,
                   timevar = NULL,
                   covariance = "diagonal",
                   data,
                   userf = NULL,
                   sweights = NULL,
                   levels = NULL,
                   predict = FALSE,
                   predtype = NULL,
                   predmodel = NULL,
                   causes = NULL,
                   at = NULL,
                   contrast = NULL,
                   modelfit = NULL,
                   control = list())
{

    # Process list of control arguments
    control.default <- list(ip = NULL, intmethod = "ghermite", debug = FALSE, verbose = FALSE, optim.method = "Nelder-Mead", maxit = 5000)
    control.tmp <- unlist(list(
        control[names(control) %in% names(control.default)],
        control.default[!(names(control.default) %in% names(control))]
    ), recursive = FALSE)
    control <- control.tmp

    # Match family
    family <- match.arg(arg = family, choices = c("gaussian", "bernoulli", "poisson", "beta", "negbinomial", "exponential", "weibull", "gompertz", "rp", "loghazard", "user", "null"), several.ok = TRUE)

    # Arguments checks
    if (isFALSE(is.list(model))) model <- list(model)
    merlin_error_check(model = model,
                       data = data,
                       timevar = timevar,
                       family = family,
                       link = link,
                       intmethod = control$intmethod,
                       covariance = covariance,
                       levels = levels,
                       sweights = sweights)

    # Turn tibbles into plain data.frame objects
    data.name <- substitute(data)
    class(data) <- "data.frame"

    est <- merlinEst(model = model,
                     from = from,
                     family = family,
                     link = link,
                     timevar = timevar,
                     covariance = covariance,
                     data = data,
                     userf = userf,
                     sweights = sweights,
                     levels = levels,
                     predict = predict,
                     predtype = predtype,
                     predmodel = predmodel,
                     causes = causes,
                     at=at,
                     contrast = contrast,
                     modelfit = modelfit,
                     control = control)

    if (isFALSE(predict)) {
        # If ip = NULL, then assign the default values to the control object (gets NULL otherwise)
        if (is.null(est$control$ip)) {
            if (est$control$intmethod == "ghermite") {
                est$control$ip <- 7
            } else {
                est$control$ip <- 100
            }
        }
        est$call   <- match.call()
        est$data   <- data.name
        class(est) <- "merlin"
    }
    est
}

# MERLIN
merlinEst <- function(model, from, family, link, timevar, covariance, data, userf, sweights, levels, predict, predtype, predmodel, causes, at, contrast, modelfit, control)
{

    #setup
    gml <- merlin_setup(model = model,
                        intmethod = control$intmethod,
                        ip = control$ip,
                        data = data,
                        timevar = timevar,
                        family = family,
                        link = link,
                        covariance = covariance,
                        userf = userf,
                        from = from,
                        levels = levels,
                        debug = control$debug,
                        sweights = sweights)

    # initial values
    if (length(from) > 1)  b <- from
    else                   b <- merlin_initial_values(gml, model)

    #fit full model or get predictions
    gml$predict <- predict

    if (isFALSE(predict)) {
        if (length(b) == 1) {
            result <- stats::optim(par = b, fn = merlin_gf, gml = gml, hessian = TRUE, method = "BFGS", control = list(maxit = control$maxit))
        } else {
            result <- stats::optim(par = b, fn = merlin_gf, gml = gml, hessian = TRUE, method = control$optim.method, control = list(maxit = control$maxit))
        }

        #gr=merlin_gf_deriv
        if (isTRUE(control$debug)) {
            print("optimisation complete")
            print(result$par)
            print(result$hessian)
            print(result$convergence)
        }

    }
    else {
        gml$par      = from
        gml$modelind = gml$modtouse = predmodel
        if (length(causes)) gml$causes = causes
        pred = merlin_predict(gml,predtype,at,contrast,modelfit)
        return(pred)
    }

    out = list("convergence"    = result$convergence,
               "data"           = data,
               "family"         = family,
               "link"           = link,
               "loglikelihood"  = -result$value,
               "levels"         = gml$levels,
               "responsevar"    = gml$y,
               "gamma"          = gamma,
               "coefficients"   = result$par,
               "hessian"        = result$hessian,
               "par"            = result$par,
               "Nmodels"        = gml$Nmodels,
               "control"        = control
          )

    # Add names to vector of coefficients and Hessian
    names(out$coefficients) = gml$labelso
    rownames(out$hessian) = gml$labelso
    colnames(out$hessian) = gml$labelso
    # Return
    return(out)
}

#' @title Print \code{merlin} Fits
#' @description Print the coefficients from a \code{merlin} fit.
#'
#' @param x An object of class \code{merlin}.
#' @param digits The number of significant digits to use when printing.
#' @param ... Not used.
#'
#' @export
print.merlin <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

    cat("Merlin: mixed-effects model\n")
    cat("Data:", x$data, "\n\n")
    cat("Coefficients:\n")
    print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
    invisible(x)

}

#' @title Summarizing \code{merlin} Fits
#' @description These functions are all methods for class \code{merlin} or \code{summary.merlin} objects.
#' @param object An object of class \code{merlin}
#' @param sig Significancy level for confidence intervals. Defaults to 0.95.
#' @param ... Not used.
#'
#' @export
summary.merlin <- function(object, sig = 0.95, ...) {

    # create coeff table
    btab           = matrix(NA, ncol = 6, nrow = length(stats::coef(object)))
    colnames(btab) = c("Estimate","Std. Error","z","Pr(>|z|)",paste0("[", sprintf("%1.0f%%", 100 * sig), " Conf."),"Interval]")
    rownames(btab) = names(stats::coef(object))

    btab[,1] = stats::coef(object)
    btab[,2] = sqrt(diag(stats::vcov(object)))
    btab[,3] = btab[,1] / btab[,2]
    btab[,5] = btab[,1] - stats::qnorm(1 - (1 - sig) / 2) * btab[,2]
    btab[,6] = btab[,1] + stats::qnorm(1 - (1 - sig) / 2) * btab[,2]
    btab[,4] = 2 * stats::pnorm(-abs(btab[,3]))

    object$coefftable = btab
    class(object) = c("summary.merlin", class(object))
    return(object)
}

#' @param x An object of class \code{summary.merlin}
#' @param digits The number of significant digits to use when printing.
#' @rdname summary.merlin
#' @export
print.summary.merlin <- function(x, digits = max(3, getOption("digits") - 3), ...) {

    cat("Mixed effects regression model\n")
    cat("Log likelihood =", stats::logLik(x))
    cat("\n\n")

    stats::printCoefmat(x$coefftable, digits = digits, na.print = "NA", cs.ind = c(1, 2, 5, 6), tst.ind = 3, zap.ind = 4)

    if (length(x$levels) > 0) {
        cat("\n")
        cat("Integration method: ")
        if (x$control$intmethod[1] == "ghermite")    cat("Non-adaptive Gauss-Hermite quadrature \n")
        else if (x$control$intmethod[1] == "halton") cat("Monte-Carlo integration using Halton sequences \n")
        else if (x$control$intmethod[1] == "sobol")  cat("Monte-Carlo integration using Sobol sequences \n")
        else                                      cat("Monte-Carlo integration \n")
        cat("Integration points:", x$control$ip, "\n")
    }

    if (x$convergence != 0) warning("Model did not converge.", call. = FALSE)
}


#' @title Extract Model Coefficients
#' @description \code{coef} extracts model coefficients from a \code{merlin} model fit. \code{coefficients} is an alias for it.
#' @param object An object of class \code{merlin} or \code{summary.merlin}.
#' @param ... Not used.
#' @export
coef.merlin <- function(object, ...) object$coefficients

#' @rdname coef.merlin
#' @export
coef.summary.merlin <- function(object, ...) coef.merlin(object, ...)

#' @title Calculate Variance-Covariance Matrix for a \code{merlin} Model Object
#' @description Returns the variance-covariance matrix of all estimated parameters of a fitted \code{merlin} model.
#' @param object An object of class \code{merlin} or \code{summary.merlin}.
#' @param ... Not used.
#' @export
vcov.merlin <- function(object, ...) solve(object$hessian)

#' @rdname vcov.merlin
#' @export
vcov.summary.merlin <- function(object, ...) vcov.merlin(object$hessian)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a \code{merlin} model.
#' @param object An object of class \code{merlin} or \code{summary.merlin}.
#' @param ... Not used.
#' @export
logLik.merlin <- function(object, ...) object$loglikelihood

#' @rdname logLik.merlin
#' @export
logLik.summary.merlin <- function(object, ...) logLik.merlin(object, ...)
