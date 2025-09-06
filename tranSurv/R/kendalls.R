#' Kendall's tau
#'
#' Computes the unconditional Kendall's tau
#'
#' This function computes the unconditional Kendall's tau (the Kendall rank correlation coefficient) for two variables.
#' The returned value is equivalent to that from \code{cor} with \code{method = "kendall"}, but \code{kendall} is
#' implemented in C.
#'
#' @param x a numeric vector.
#' @param y a numeric vector with compatible length to \code{x}.
#'
#' @return The output is a numeric value for the unconditional Kendall's tau.
#' @export
#' @references Kendall, M. G. (1938), A new measure of rank correlation, \emph{Biometrika}, 81--93.
#' @seealso \code{\link{cor}}
#'
#' @return A numeric value representing the unconditional Kendall's tau.
#' 
#' @example inst/examples/ex_kendalls.R
kendall <- function(x, y = NULL) {
    if (is.null(y)) {
        if (NCOL(x) != 2L) stop("Two variables are required.")
        else {
            y <- x[,2]
            x <- x[,1]
        }
    } else {
        if (length(x) != length(y))
            stop("The length of two variables needs to be the same.")
    }
    n <- length(x)
    res <- vector("double", 1)
    .C("uCondKendall", as.double(x), as.double(y), as.integer(n),
       out = as.double(res), PACKAGE = "tranSurv")$out
}

#' Conditional Kendall's tau
#'
#' Computes the conditional Kendall's tau and inference
#'
#' This function performs statistical test for quasi-independence between truncation time and failure time.
#' The hypothesis test is based on the conditional Kendall's tau of Martin and Betensky (2005) and
#' the two versions of the inverse probability weighted Kendall's tau of Austin and Betensky (2014).
#'
#' The output contains the following components:
#' \describe{
#'   \item{PE}{consistent point estimate of the conditional Kendall's tau.}
#'   \item{SE}{asymptotic standard error of the conditional Kendall's tau estimator.}
#'   \item{STAT}{the value of the normal test statistic.}
#'   \item{p.value}{the (Wald) p-value of the test.}
#'   \item{trans}{the transformation model (if applied).}
#'   \item{a}{the estimated transformation parameter.}
#' }
#'
#' @param trun left truncation time satisfying \code{trun} <= \code{obs}.
#' @param obs observed failure time, must be the same length as \code{trun}, might be right-censored.
#' @param delta an optional 0-1 vector of censoring indicator (0 = censored, 1 = event) for \code{obs}.
#' If this vector is not specified, \code{cKendall} assumes no censoring and all observed failure time
#' denote events.
#' @param method a character string specifying the different version of conditional Kendall's tau to be computed.
#' The following are permitted:
#' \describe{
#'   \item{\code{MB}}{conditional Kendall's tau proposed in Martin and Betensky (2005) as \eqn{\hat{\tau}_c,}}
#'   \item{\code{IPW1}}{inverse probability weighted estimator proposed in Austin and Betensky (2014) as \eqn{\hat{\tau}_{c2},}}
#'   \item{\code{IPW2}}{restricted inverse probability weighted estimator proposed in Austin and Betensky (2014) as \eqn{\hat{\tau}_{c3}.}}
#' }
#' @param weights an optional vector of sampling weights used when \code{method = IPW1} or \code{method = IPW2}.
#' Inverse probability censored weighting (IPCW) is the default.
#' @param a a numeric transformation parameter. The default value is 0, which applies no transformation.
#' This parameter must be greater than \code{-1}.
#' See \code{?tranSurvfit} for the transformation model structure.
#' @param trans a character string specifying the transformation structure. The following are permitted:
#' \describe{
#'   \item{linear}{linear transformation structure,}
#'   \item{log}{log-linear transformation structure,}
#'   \item{exp}{exponential transformation structure.}
#' }
#' @param ... for future methods.
#' @export
#'
#' @return A numeric value representing the unconditional Kendall's tau.
#' 
#' @seealso \code{\link{trSurvfit}}
#' @references  Martin E. and Betensky R. A. (2005), Testing quasi-independence of failure and truncation times via conditional Kendall's tau,
#' \emph{Journal of the American Statistical Association}, \bold{100} (470): 484-492.
#' @references Austin, M. D. and Betensky R. A. (2014), Eliminating bias due to censoring in Kendall's tau estimators for quasi-independence of truncation and failure,
#' \emph{Computational Statistics & Data Analysis}, \bold{73}: 16-26.
#' @example inst/examples/ex_condKendall.R
cKendall <- function(trun, obs, delta = NULL, method = "MB",
                        weights = NULL, a = 0, trans = "linear", ...) {
    methName <- c("MB", "IPW1", "IPW2")
    if (!(method %in% methName)) stop("Invalid method name", call. = FALSE)
    ## Weights arranged by c(trun, obs)
    out <- NULL
    out$Call <- match.call()
    n <- length(trun)
    if (inherits(trans, "character")) {
        if (trans == "linear") FUN <- function(X, T, a) (T + a * X) / (1 + a)
        if (trans == "log") FUN <- function(X, T, a) exp((log(replace(T, 0, 1)) + a * log(X)) / (1 + a))
        if (trans == "log2") FUN <- function(X, T, a) exp((1 + a) * log(replace(T, 0, 1)) - a * log(X))
        if (trans == "exp") FUN <- function(X, T, a) log((exp(T) + a * exp(X)) / (1 + a))
    } else {
        FUN <- match.fun(trans)
    }
    trun <- mapply(FUN, X = obs, T = trun, a = a)
    if (is.null(delta)) delta <- rep(1, length(trun))
    if (is.null(weights) & method == "MB") weights <- rep(1, 2 * n)
    if (is.null(weights) & method != "MB") {
        sc <- survfit(Surv(trun, obs, 1 - delta) ~ 1)
        if (length(table(delta)) > 1 & 
            sum(head(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]) == 1) <= 2) {
            sc$time <- sc$time[sc$n.event > 0]
            sc$surv <- exp(-cumsum(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]))
        }
    }
    if (length(weights) == length(trun)) weights <- rep(weights, 2)
    if (is.null(weights) & method != "MB") {
        weights <- approx(sc$time, sc$surv, method = "constant",
                          xout = c(trun, obs), yleft = 1, yright = min(sc$surv))$y
    }
    ## res <- vector("double", 2)
    if (method != "IPW2") {
        tmp <- .C("condKendallC", as.double(trun), as.double(obs), as.double(delta),
                  as.integer(n), as.double(weights), as.integer(which(method == methName)), 
                  tmp = double(2), PACKAGE = "tranSurv")$tmp
    } else {
        event <- delta == 1
        tmp <- .C("condKendallC", as.double(trun[event]), as.double(obs[event]),
                  as.double(delta[event]), as.integer(sum(event)), as.double(weights[rep(event, 2)]),
                  as.integer(which(method == methName)), 
                  tmp = double(2), PACKAGE = "tranSurv")$tmp
    }
    out$PE <- tmp[1]
    out$SE <- ifelse(tmp[2] >= 0, sqrt(tmp[2]), NA)
    out$STAT <- abs(out$PE) / out$SE
    out$p.value <- 2 - 2 * pnorm(out$STAT)
    out$trans <- trans
    out$a <- a
    class(out) <- "cKendall"
    out
}

