#### Default Method ####

#' Poisson outcome for kDGLM models
#'
#' Creates an outcome with Poisson distribution with the chosen parameter.
#'
#' @param lambda character: The name of the linear predictor associated with the rate (mean) parameter of the Poisson distribution. The parameter is treated as unknown and equal to the exponential of the associated linear predictor.
#' @param data numeric: The values of the observed data.
#' @param offset numeric: The offset at each observation. Must have the same shape as data.
#'
#' @return A object of the class dlm_distr
#'
#' @importFrom stats dpois
#' @export
#'
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, D = 0.95, order = 2)
#' season <- harmonic_block(rate = 1, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' summary(fitted.data)
#'
#' plot(fitted.data, plot.pkg = "base")
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
#' @references
#'    \insertAllCited{}
Poisson <- function(lambda, data, offset = as.matrix(data)**0) {
  if (any(ceiling(data) != floor(data), na.rm = TRUE)) {
    stop("Error: data must be an intenger vector/matrix.")
  }
  alt.method <- FALSE
  data <- as.matrix(data)

  distr <- list()
  t <- length(data)
  r <- k <- 1
  convert.mat.default <- convert.mat.canom <- diag(r)

  # lambda=deparse(substitute(lambda))[[1]] |> check.expr()

  distr <- list(
    conj_distr = convert_Poisson_Normal,
    norm_distr = convert_Normal_Poisson,
    update = update_Poisson,
    smoother = generic_smoother,
    calc_pred = poisson_pred,
    apply_offset = function(ft, Qt, offset) {
      list("ft" = ft + log(offset), "Qt" = Qt)
    },
    link_function = log, inv_link_function = exp,
    log_like_function = function(x, param) {
      dpois(x, param, log = TRUE)
    },
    param.names = c("alpha", "beta"),
    pred.names = c("Mean (labmda)" = lambda),
    r = r,
    k = k,
    l = k,
    t = t,
    offset = matrix(offset, t, r),
    data = matrix(data, t, r),
    na.condition = any_na,
    convert.mat.canom = convert.mat.canom,
    convert.mat.default = convert.mat.default,
    convert.canom.flag = FALSE,
    parms = list(),
    name = "Poisson"
  )
  class(distr) <- "dlm_distr"
  distr$alt.method <- alt.method

  # if (alt.method) {
  #   distr$update <- update_Poisson_alt
  # }

  return(distr)
}

#' convert_Poisson_Normal
#'
#' Calculate the parameters of the Gamma that best approximates the given log-Normal distribution.
#' The approximation is the best in the sense that it minimizes the KL divergence from the log-Normal to the Gamma
#'
#' @param ft numeric: A vector representing the means from the normal distribution.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the conjugated distribution of the linear predictor.
#' @keywords internal
#'
#' @family auxiliary functions for a Poisson outcome
convert_Poisson_Normal <- function(ft, Qt, parms) {
  # diag(Qt)=ifelse(diag(Qt)<0,0,diag(Qt))
  if (length(ft) > 1) {
    Qt <- diag(Qt)
    ft <- c(ft)
  }
  h <- -3 + 3 * sqrt(1 + 2 * Qt / 3)
  alpha <- abs(1 / h)
  beta <- alpha * exp(-ft - 0.5 * Qt)
  # beta <- alpha * exp(-ft + 0.5 * Qt) # Linear Bayes
  return(list("alpha" = alpha, "beta" = beta))
}

#' convert_Normal_Poisson
#'
#' Calculate the parameters of the log-Normal that best approximates the given Gamma distribution.
#' The approximation is the best in the sense that it minimizes the KL divergence from the Gamma to the log-Normal
#'
#' @param conj.param list: A vector containing the parameters of the Gamma (alpha,beta).
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the Normal distribution of the linear predictor.
#' @keywords internal
#'
#' @family auxiliary functions for a Poisson outcome
convert_Normal_Poisson <- function(conj.param, parms) {
  alpha <- conj.param$alpha
  beta <- conj.param$beta
  ft <- digamma(alpha) - log(beta)
  Qt <- trigamma(alpha)
  return(list("ft" = ft, "Qt" = Qt))
}

# # convert_Poisson_Normal_LB
# convert_Poisson_Normal_LB <- function(ft, Qt, parms) {
#   h <- -3 + 3 * sqrt(1 + 2 * Qt / 3)
#   alpha <- (1 / h)
#   beta <- alpha * exp(-ft + 0.5 * Qt)
#   return(list("alpha" = alpha, "beta" = beta))
# }

#' update_Poisson
#'
#' Calculate posterior parameter for the Gamma, assuming that the observed values came from a Poisson model from which the rate parameter (lambda) have prior distribution Gamma.
#'
#' @param conj.param list: A vector containing the parameters of the Gamma (alpha,beta).
#' @param ft numeric: A vector representing the means from the normal distribution. Not used in the default method.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution. Not used in the default method.
#' @param y numeric: A vector containing the observations.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
#'
#' @family auxiliary functions for a Poisson outcome
update_Poisson <- function(conj.param, ft, Qt, y, parms) {
  alpha <- conj.param$alpha
  beta <- conj.param$beta

  alpha <- alpha + y
  beta <- beta + 1
  return(list("alpha" = alpha, "beta" = beta))
}

#' poisson_pred
#'
#' Calculate the values for the predictive distribution given the values of the parameter of the conjugated distribution of the linear predictor.
#' The data is assumed to have Poisson distribution with its mean having distribution Gamma with shape parameter a e rate parameter b.
#' In this scenario, the marginal distribution of the data is Negative Binomial with a as the dispersion parameter and b/(b+1) as the probability.
#'
#' @param conj.param list or data.frame: The parameters of the conjugated distributions of the linear predictor.
#' @param outcome numeric or matrix (optional): The observed values at the current time. Not used in this function.
#' @param parms list (optional): A list of extra parameters for the model. Not used in this function.
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
#' @importFrom stats rgamma rnorm rpois dpois qnbinom dnbinom var quantile
#' @keywords internal
#'
#' @family auxiliary functions for a Poisson outcome
poisson_pred <- function(conj.param, outcome = NULL, parms = list(), pred.cred = 0.95) {
  pred.flag <- !is.na(pred.cred)
  like.flag <- !is.null(outcome)
  if (!like.flag && !pred.flag) {
    return(list())
  }

  r <- 1
  a <- conj.param$alpha
  b <- conj.param$beta
  t <- length(a)
  pred <- matrix(NA, r, t)
  var.pred <- array(NA, c(1, 1, t))
  icl.pred <- matrix(NA, r, t)
  icu.pred <- matrix(NA, r, t)
  log.like <- rep(NA, t)
  flags <- b > 1e-20
  if (like.flag) {
    outcome <- matrix(outcome, t, r)
  }

  if (pred.flag) {
    pred[, flags] <- a[flags] / b[flags]
    var.pred[, , flags] <- a[flags] * (b[flags] + 1) / (b[flags]**2)

    icl.pred[, flags] <- qnbinom((1 - pred.cred) / 2, a[flags], (b[flags] / (b[flags] + 1)))
    icu.pred[, flags] <- qnbinom(1 - (1 - pred.cred) / 2, a[flags], (b[flags] / (b[flags] + 1)))
  }
  if (like.flag) {
    log.like[flags] <- dnbinom(outcome[flags, 1], a[flags], (b[flags] / (b[flags] + 1)), log = TRUE)
  }

  N <- 5000
  for (i in seq_len(t)[!flags]) {
    h <- 1 / a[i]
    Qt <- 3 * ((((h + 3) / 3)**2) - 1) / 2
    ft <- -log(b[i]) + log(a[i]) - 0.5 * Qt
    # Qt <- trigamma(a[i])
    # ft <- digamma(a[i]) - log(b[i] + 1e-50)

    # sample.lambda <- rgamma(N, a[i], b[i])
    sample.lambda <- exp(rnorm(N) * sqrt(Qt) + ft)
    if (any(is.infinite(sample.lambda))) {
      warning("Predictive distribution is numerically intractable at some time.")
      sample.y <- rep(NA, N)
    } else {
      sample.y <- rpois(N, sample.lambda)
    }

    if (pred.flag) {
      pred[, i] <- mean(sample.y)
      var.pred[, , i] <- var(sample.y)
      icl.pred[, i] <- quantile(sample.y, (1 - pred.cred) / 2, na.rm = TRUE)
      icu.pred[, i] <- quantile(sample.y, 1 - (1 - pred.cred) / 2, na.rm = TRUE)
    }
    if (like.flag) {
      log.like.list <- dpois(outcome[i, 1], sample.lambda, log = TRUE)
      max.like.list <- max(log.like.list)

      if (is.infinite(max.like.list)) {
        log.like[i] <- NA
      } else {
        log.like[i] <- log(mean(exp(log.like.list - max.like.list))) + max.like.list
      }
    }
  }
  if (!pred.flag) {
    pred <- NULL
    var.pred <- NULL
    icl.pred <- NULL
    icu.pred <- NULL
  }
  if (!like.flag) {
    log.like <- NULL
  }

  return(list(
    "pred" = pred,
    "var.pred" = var.pred,
    "icl.pred" = icl.pred,
    "icu.pred" = icu.pred,
    "log.like" = log.like
  ))
}
