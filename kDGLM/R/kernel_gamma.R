#### Default Method ####

#' Gamma outcome for kDGLM models
#'
#' Creates an outcome with gamma distribution with the chosen parameters (can only specify 2).
#'
#' @param phi character or numeric: Name of the linear predictor associated with the shape parameter of the gamma distribution. If numeric, this parameter is treated as known and equal to the value passed. If a character, the parameter is treated as unknown and equal to the exponential of the associated linear predictor. It cannot be specified with alpha.
#' @param mu character: Name of the linear predictor associated with the mean parameter of the gamma distribution. The parameter is treated as unknown and equal to the exponential of the associated linear predictor.
#' @param alpha character: Name of the linear predictor associated with the shape parameter of the gamma distribution. The parameter is treated as unknown and equal to the exponential of the associated linear predictor. It cannot be specified with phi.
#' @param beta character: Name of the linear predictor associated with the rate parameter of the gamma distribution. The parameter is treated as unknown and equal to the exponential of the associated linear predictor. It cannot be specified with sigma.
#' @param sigma character: Name of the linear predictor associated with the scale parameter of the gamma distribution. The parameter is treated as unknown and equal to the exponential of the associated linear predictor. It cannot be specified with beta.
#' @param data numeric: Values of the observed data.
#' @param offset numeric: The offset at each observation. Must have the same shape as data.
#'
#' @return An object of the class dlm_distr
#' @importFrom stats dgamma
#' @export
#'
#' @details
#'
#' For evaluating the posterior parameters, we use the method proposed in \insertCite{ArtigokParametrico;textual}{kDGLM}.
#'
#' For the details about the implementation see  \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary functions for a creating outcomes
#'
#' @examples
#'
#' structure <- polynomial_block(mu = 1, D = 0.95)
#'
#' Y <- (cornWheat$corn.log.return[1:500] - mean(cornWheat$corn.log.return[1:500]))**2
#' outcome <- Gamma(phi = 0.5, mu = "mu", data = Y)
#' fitted.data <- fit_model(structure, corn = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' @references
#'    \insertAllCited{}
Gamma <- function(phi = NA, mu = NA, alpha = NA, beta = NA, sigma = NA,
                  data, offset = as.matrix(data)**0) {
  if (min(data, na.rm = TRUE) <= 0) {
    stop("Error: data must be a strictly positive vector/matrix.")
  }
  alt.method <- FALSE
  data <- as.matrix(data)

  # phi=deparse(substitute(phi))[[1]] |> check.expr()
  # mu=deparse(substitute(mu))[[1]] |> check.expr()
  # alpha=deparse(substitute(alpha))[[1]] |> check.expr()
  # beta=deparse(substitute(beta))[[1]] |> check.expr()
  # sigma=deparse(substitute(sigma))[[1]] |> check.expr()

  t <- length(data)
  r <- 1

  if (is.numeric(phi)) {
    k <- 1
    if (any(!is.na(c(alpha, beta, sigma)))) {
      stop("Error: When phi is known only mu can be estimated.")
    }
    pred.names <- c("Mean (mu)" = mu)
    convert.mat.default <- convert.mat.canom <- diag(1)
    convert.canom.flag <- FALSE
    distr <- list(
      conj_distr = convert_Gamma_Normal,
      norm_distr = convert_Normal_Gamma,
      update = update_Gamma,
      smoother = generic_smoother,
      calc_pred = gamma_pred,
      apply_offset = function(ft, Qt, offset) {
        t <- if.null(dim(ft)[2], 1)
        offset <- matrix(offset, t, r)

        list("ft" = ft + log(t(offset)), "Qt" = Qt)
      },
      link_function = log, inv_link_function = exp,
      log_like_function = function(x, param) {
        dgamma(x, phi, phi / param)
      },
      param.names = c("alpha", "beta")
    )
    # if (alt.method) {
    #   distr$update <- update_Gamma_alt
    # }

    parms <- list(phi = phi)
  } else {
    if (!alt.method) {
      stop("Error: The estimation of the shape parameter phi is still under development. See the nightly build in GitHub to use this functionality.")
    }
    warning("The estimation of the shape parameter phi is still under development. Results are not reliable.")
    k <- 2
    flags <- !is.na(c(phi, mu, alpha, beta, sigma))
    if (sum(flags) < 2) {
      stop("Error: Parameters not fully specified. You must specified exactly 2 non reduntant parameters.")
    }
    if (sum(flags) > 2) {
      stop("Error: Parameters over specified. You must specified exactly 2 non reduntant parameters.")
    }
    if (all(flags[4:5])) {
      stop("Error: Scale specified in more than one variable.")
    }
    if (all(flags[c(1, 3)])) {
      stop("Error: Shape specified in more than one variable.")
    }
    convert.mat.default <- matrix(c(1, 0, 0, 1, 1, 0, 1, -1, -2, 2), 2, 5)[, flags]
    convert.mat.canom <- solve(convert.mat.default)
    convert.canom.flag <- !all(flags[c(1, 2)])
    parms <- list()
    pred.names <- c(phi, mu, alpha, beta, sigma)[flags]
    names(pred.names) <- c("Shape (phi)", "Mean (mu)", "Shape (alpha)", "Rate (beta)", "Scale (sigma)")[flags]

    distr <- list(
      # conj_distr = convert_FGamma_Normal,
      # norm_distr = convert_Normal_FGamma,
      # update = update_FGamma,
      smoother = generic_smoother,
      # calc_pred = Fgamma_pred,
      apply_offset = function(ft, Qt, offset) {
        list("ft" = ft + matrix(c(0, log(offset)), 2, dim(ft)[2]), "Qt" = Qt)
      },
      link_function = log, inv_link_function = exp,
      log_like_function = function(x, param) {
        dgamma(x, param[1], param[1] / param[2], log = TRUE)
      },
      param.names = c("n", "k", "tau", "theta")
    )

    if (alt.method) {
      distr$conj_distr <- format_ft
      distr$norm_distr <- format_param
      # distr$update <- update_FGamma_alt
      # distr$calc_pred <- Fgamma_pred_alt
      distr$param.names <- generic_param_names(k)
    }
  }

  distr$pred.names <- pred.names
  distr$r <- r
  distr$k <- k
  distr$l <- k
  distr$t <- t
  distr$na.condition <- any_na
  distr$offset <- matrix(offset, t, r)
  distr$data <- matrix(data, t, r)
  distr$convert.canom.flag <- convert.canom.flag
  distr$convert.mat.canom <- convert.mat.canom
  distr$convert.mat.default <- convert.mat.default
  distr$parms <- parms
  distr$name <- "Gamma"
  distr$sufix <- ""

  class(distr) <- "dlm_distr"
  distr$alt.method <- alt.method
  return(distr)
}

##### Gamma with known shape but unknown mean #####

#' convert_Gamma_Normal
#'
#' Calculate the parameters of the Inverse-Gamma that best approximates the given log-Normal distribution.
#' The approximation is the best in the sense that it minimizes the KL divergence from the log-Normal to the Inverse-Gamma
#'
#' @param ft vector: A vector representing the means from the normal distribution.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this function.
#'
#' @return The parameters of the conjugated distribution of the linear predictor.
#' @keywords internal
#'
#' @family auxiliary functions for a Gamma outcome with known shape
convert_Gamma_Normal <- function(ft, Qt, parms) {
  alpha <- 1 / (-3 + 3 * sqrt(1 + 2 * Qt / 3))
  beta <- alpha * exp(ft - Qt / 2)
  return(list("alpha" = alpha, "beta" = beta))
}

#' convert_Normal_Gamma
#'
#' Calculates the parameters of the log-Normal that best approximates the given Inverse-Gamma distribution.
#' The approximation is the best in the sense that it minimizes the KL divergence from the Inverse-Gamma to the log-Normal
#'
#' @param conj.param list: A vector containing the parameters of the Inverse-Gamma (alpha,beta).
#' @param parms list: A list of extra known parameters of the distribution. Not used in this function.
#'
#' @return The parameters of the Normal distribution of the linear predictor.
#' @keywords internal
#'
#' @family auxiliary functions for a Gamma outcome with known shape
convert_Normal_Gamma <- function(conj.param, parms) {
  ft <- -digamma(conj.param$alpha) + log(conj.param$beta)
  Qt <- trigamma(conj.param$alpha)
  return(list("ft" = ft, "Qt" = Qt))
}

#' update_Gamma
#'
#' Calculate posterior parameter for the Inverse-Gamma, assuming that the observed values came from a Gamma model from which the shape parameter (phi) is known and the mean (mu) have prior distribution Inverse-Gamma.
#'
#' @param conj.param list: A vector containing the parameters of the Inverse-Gamma (alpha,beta).
#' @param ft numeric: A vector representing the means from the normal distribution. Not used in the default method.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution. Not used in the default method.
#' @param y numeric: A vector containing the observations.
#' @param parms list: A list of extra known parameters of the distribution. For this kernel, parms should containing the shape parameter (phi) for the observational gamma model.
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
#'
#' @family auxiliary functions for a Gamma outcome with known shape
update_Gamma <- function(conj.param, ft, Qt, y, parms) {
  alpha <- conj.param$alpha + parms$phi
  beta <- conj.param$beta + y * parms$phi
  return(list("alpha" = alpha, "beta" = beta))
}

#' gamma_pred
#'
#' Calculate the values for the predictive distribution given the values of the parameter of the conjugated distribution of the linear predictor.
#' The data is assumed to have Gamma distribution with known shape parameter phi and its mean having distribution Inverse-Gamma with shape parameter a e rate parameter b.
#' In this scenario, the marginal distribution of the data is Beta prime with parameters phi, alpha, beta / phi.
#'
#' @param conj.param list or data.frame: The parameters of the conjugated distributions of the linear predictor.
#' @param outcome numeric or matrix (optional): The observed values at the current time. Not used in this function.
#' @param parms list: A list of extra parameters for the model. For this function, it must contain the shape parameter phi of the observational model.
#' @param pred.cred numeric: the desired credibility for the credibility interval.
#'
#' @return A list containing the following values:
#' \itemize{
#'    \item pred numeric/matrix: the mean of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item var.pred numeric/matrix: the variance of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icl.pred numeric/matrix: the percentile of 100*((1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icu.pred numeric/matrix: the percentile of 100*(1-(1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item log.like numeric: the The log likelihood for the outcome given the conjugated parameters.
#' }
#'
#' @importFrom extraDistr qbetapr dbetapr
#' @export
#' @keywords internal
#'
#' @family auxiliary functions for a Gamma outcome with known shape
gamma_pred <- function(conj.param, outcome = NULL, parms = list(), pred.cred = 0.95) {
  pred.flag <- !is.na(pred.cred)
  like.flag <- !is.null(outcome)
  if (!like.flag && !pred.flag) {
    return(list())
  }

  phi <- parms$phi

  alpha <- conj.param$alpha |> t()
  beta <- conj.param$beta |> t()
  pred <- NULL
  var.pred <- NULL
  icl.pred <- NULL
  icu.pred <- NULL
  log.like <- NULL

  if (pred.flag) {
    pred <- ifelse(alpha > 1,
      beta / (alpha - 1),
      NA
    )
    var.pred <- ifelse(alpha > 1,
      ((beta / phi)**2) * (phi * (phi + alpha - 1) / ((alpha - 2) * (alpha - 1)**2)),
      Inf
    )

    icl.pred <- qbetapr((1 - pred.cred) / 2, phi, alpha, beta / phi)
    icu.pred <- qbetapr(1 - (1 - pred.cred) / 2, phi, alpha, beta / phi)
  }
  if (like.flag) {
    log.like <- dbetapr(outcome, phi, alpha, beta / phi, log = TRUE)
  }
  return(list(
    "pred" = pred,
    "var.pred" = var.pred,
    "icl.pred" = icl.pred,
    "icu.pred" = icu.pred,
    "log.like" = log.like
  ))
}
