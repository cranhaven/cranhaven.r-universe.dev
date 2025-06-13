#' Predict method for FluMoDL objects
#'
#' Obtains predictions (predicted daily or weekly deaths) and optionally estimates
#' standard errors of those predictions
#'
#' @param object A FluMoDL object
#' @param temp A vector of daily mean temperatures. See 'Details'.
#' @param proxyH1 A vector of daily influenza A(H1N1)pdm09 incidence proxies. See 'Details'.
#' @param proxyH3 A vector of daily influenza A(H3N2) incidence proxies. See 'Details'.
#' @param proxyB A vector of daily influenza B incidence proxies. See 'Details'.
#' @param proxyRSV An vector of daily RSV incidence proxies (used only if the FluMoDL
#'     object includes an RSV term). See 'Details'.
#' @param se.fit Logical switch indicating if standard errors are required.
#'     Requires \code{byWeek=FALSE}.
#' @param byWeek If \code{TRUE}, aggregate fitted estimates by week. Has priority
#'     over argument \code{se.fit}. If both \code{se.fit} and \code{byWeek} are
#'     \code{TRUE}, \code{se.fit} is set to \code{FALSE} and a warning is returned.
#' @param ... Further arguments passed to or from other methods
#'
#' @details Arguments \code{temp}, \code{proxyH1}, \code{proxyH3}, \code{proxyB} and
#'     (if \code{hasRSV(object)} is \code{TRUE}) \code{proxyRSV} take a numeric vector
#'     as input, which is recycled to a length of \code{nrow(object$data)}. Alternatively
#'     they can take \code{NULL}, in which case the respective column of \code{object$data}
#'     is used as input. Argument \code{temp} can also take the string \code{"MMP"}, which
#'     is interpreted as the "Minimum Mortality Point", i.e. the temperature at which
#'     mortality is lowest (found in \code{object$MMP}).
#'
#'     In this way, the \code{predict()} method can be flexibly used to calculate the
#'     predicted "baseline" mortality (by setting \code{temp="MMP"} and all incidence proxies
#'     to zero), the model-predicted mortality for the actual input (by leaving all input
#'     arguments to their default \code{NULL}), or predicted mortalities for any combination of
#'     temperature and incidence proxy inputs.
#'
#' @return A vector of daily predicted deaths (corresponding to the rows in
#'     \code{object$data}). If \code{byWeek=TRUE}, the predictions are automatically
#'     aggregated by week (as per \code{object$data$yearweek}) and the vector contains
#'     the respective week (in YYYYWW format) as names.
#'
#'     If \code{se.fit=TRUE}, a list
#'     is returned with elements \code{$fit} and \code{$se.fit} containing the
#'     (daily) predicted deaths and their associated log standard errors.
#'
#'     Note that the first 30 elements (or first 5 elements if \code{byWeek=TRUE}) will be
#'     \code{NA} by default, as FluMoDL uses a maximum lag of 30 days.
#'
#' @import stats
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#' m
#'
#' # Calculate FluMoDL baseline
#' baseline <- predict(m, temp="MMP", proxyH1=0, proxyH3=0, proxyB=0, byWeek=TRUE)
#'
#' # Calculate fitted predictions
#' fitted <- predict(m, byWeek=TRUE)
#'
#' \donttest{
#' # Plot everything
#' plot(with(m$data, tapply(deaths, yearweek, sum)), type="l",
#'      xaxt="n", ylab="Weekly deaths", xlab="Time")
#' points(baseline, type="l", col="blue")
#' points(fitted, type="l", col="green")
#' legend("topleft", c("Actual", "Baseline", "Fitted"), lty="solid",
#'     col=c("black", "blue", "green"), bty="n")
#' }
#'
#' @export
predict.FluMoDL <- function(object, temp=NULL,
    proxyH1=NULL, proxyH3=NULL, proxyB=NULL, proxyRSV=NULL,
    se.fit=FALSE, byWeek=FALSE, ...) {
  # Check arguments
  if (!inherits(object, "FluMoDL")) stop("Argument `object` should be of class 'FluMoDL'.")
  if (is.null(temp)) {
    temp <- object$data$temp
  } else if (length(temp)==1 && temp=="MMP") {
    temp <- object$MMP
  } else if (!is.numeric(temp)) {
    stop("Argument 'temp' must be numeric")
  }
  if (is.null(proxyH1)) {
    proxyH1 <- object$data$proxyH1
  } else if (!is.numeric(proxyH1)) {
    stop("Argument 'proxyH1' must be numeric")
  }
  if (is.null(proxyH3)) {
    proxyH3 <- object$data$proxyH3
  } else if (!is.numeric(proxyH3)) {
    stop("Argument 'proxyH3' must be numeric")
  }
  if (is.null(proxyB)) {
    proxyB <- object$data$proxyB
  } else if (!is.numeric(proxyB)) {
    stop("Argument 'proxyB' must be numeric")
  }
  if (hasRSV(object)) {
    if (is.null(proxyRSV)) {
      proxyRSV <- object$data$proxyRSV
    } else if (!is.numeric(proxyRSV)) {
      stop("Argument 'proxyRSV' must be numeric")
    }
  }

  # Calculate a temperature cross basis
  lg <- 30   # 30 days maximum lag (fixed)
  basis.temp <- crossbasis(rep_len(temp, nrow(object$data)), lag=lg,
      argvar=list(fun="bs", degree=2, knots=quantile(object$data$temp, c(0.1,0.75,0.9)),
          Boundary.knots = attr(object$basis$temp, "argvar")$Boundary.knots),
      arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyH1 <- crossbasis(rep_len(proxyH1, nrow(object$data)), lag=lg,
      argvar=list(fun="poly", degree=1, scale=attr(object$basis$proxyH1, "argvar")$scale),
      arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyH3 <- crossbasis(rep_len(proxyH3, nrow(object$data)), lag=lg,
      argvar=list(fun="poly", degree=1, scale=attr(object$basis$proxyH3, "argvar")$scale),
      arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyB <- crossbasis(rep_len(proxyB, nrow(object$data)), lag=lg,
      argvar=list(fun="poly", degree=1, scale=attr(object$basis$proxyB, "argvar")$scale),
      arglag=list(fun="ns", knots=logknots(lg,3)))

  predictors <- cbind(1, basis.temp,
    basis.proxyH1, basis.proxyH3, basis.proxyB)

  if (hasRSV(object)) {
    basis.proxyRSV <- crossbasis(rep_len(proxyRSV, nrow(object$data)), lag=lg,
        argvar=list(fun="poly", degree=1, scale=attr(object$basis$proxyRSV, "argvar")$scale),
        arglag=list(fun="ns", knots=logknots(lg,3)))
    predictors <- cbind(predictors, basis.proxyRSV)
  }

  predictors <- cbind(predictors, 
        sapply(2:7, function(i) object$data$dow==i), object$data$t)
  if (hasPeriodic(object)) {
    predictors <- cbind(predictors, pbs(object$data$doy, knots=c(91,182,274)))
  }
  fit <- c(exp(predictors %*% coef(object$model)))

  if (byWeek) {
    fit <- tapply(fit, object$data$yearweek, sum)
    if (se.fit) {
      se.fit <- FALSE
      warning("Cannot have se.fit=TRUE if byWeek=TRUE; setting se.fit=FALSE.")
    }
  }

  if (se.fit) {
    se.fit <- sqrt(diag(predictors %*% vcov(object$model) %*% t(predictors)))
    return(list(fit=fit, se.fit=se.fit))
  } else {
    return(fit)
  }
}

