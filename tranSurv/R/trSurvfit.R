#' Estimating survival curves via structural transformation model
#'
#' \code{trSurvfit} estimates survival curves under dependent truncation and independent censoring via a structural transformation model.
#'
#' A structural transformation model assumes there is a latent, quasi-independent truncation time
#' that is associated with the observed dependent truncation time, the event time, and an unknown dependence parameter
#' through a specified funciton.
#' The dependence parameter is chosen to either minimize the absolute value of the restricted inverse probability weighted Kendall's tau or maximize the corresponding \eqn{p}-value.
#' The marginal distribution for the truncation time and the event time are completely left unspecified.
#'
#' The structure of the transformation model is of the form:
#' \deqn{h(U) = (1 + a)^{-1} \times (h(T) + ah(X)),} where \eqn{T} is the truncation time, \eqn{X} is the observed failure time,
#' \eqn{U} is the transformed truncation time that is quasi-independent from \eqn{X} and \eqn{h(\cdot)} is a monotonic transformation function.
#' The condition, \eqn{T < X}, is assumed to be satisfied.
#' The quasi-independent truncation time, \eqn{U}, is obtained by inverting the test for quasi-independence by either minimizing
#' the absolute value of the restricted inverse probability weighted Kendall's tau or maximize the corresponding \eqn{p}-value.
#'
#' At the current version, three transformation structures can be specified. \code{trans = "linear"} corresponds to \deqn{h(X) = 1;}
#' \code{trans = "log"} corresponds to \deqn{h(X) = log(X);}
#' \code{trans = "exp"} corresponds to \deqn{h(X) = exp(X).}
#' 
#' @param trun left truncation time satisfying \code{trun} <= \code{obs}.
#' @param obs observed failure time, must be the same length as \code{trun}, might be right-censored.
#' @param delta an optional 0-1 vector of censoring indicator (0 = censored, 1 = event) for \code{obs}.
#' If this vector is not specified, \code{cKendall} assumes no censoring and all observed failure time
#' denote events.
#' @param tFun a character string specifying the transformation function or a user specified function indicating the relationship
#' between \eqn{X}, \eqn{T}, and \eqn{a}.
#' When \code{tFun} is a character, the following are permitted:
#' \describe{
#'   \item{linear}{linear transformation structure;}
#'   \item{log}{log-linear transformation structure;}
#'   \item{exp}{exponential transformation structure.}
#' }
#' @param plots an optional logical value; if TRUE, a series of diagnostic plots as well as the survival curve for the observed failure time will be plotted.
#' @param control controls the lower and upper bounds when \code{trans} is an user specified function.
#' @param ... for future methods.
#' 
#' @return The output contains the following components:
#' \describe{
#'   \item{\code{surv}}{is a \code{data.frame} contains the survival probabilities estimates.}
#'   \item{\code{byTau}}{a list contains the estimator of transformation parameter:
#'     \describe{
#'     \item{\code{par}}{is the best set of transformation parameter found;}
#'     \item{\code{obj}}{is the value of the inverse probability weighted Kendall's tau corresponding to 'par'.}}
#'   }
#'   \item{\code{byP}}{a list contains the estimator of transformation parameter:
#'     \describe{
#'     \item{\code{par}}{is the best set of transformation parameter found;}
#'     \item{\code{obj}}{is the value of the inverse probability weighted Kendall's tau corresponding to 'par'.}}
#'   }
#'   \item{\code{qind}}{a data frame consists of two quasi-independent variables:
#'     \describe{
#'     \item{\code{trun}}{ is the transformed truncation time;}
#'     \item{\code{obs}}{ is the corresponding uncensored failure time.}}}
#' }
#'
#' @references Martin E. and Betensky R. A. (2005), Testing quasi-independence of failure and truncation times via conditional Kendall's tau,
#' \emph{Journal of the American Statistical Association}, \bold{100} (470): 484-492.
#' @references Austin, M. D. and Betensky R. A. (2014), Eliminating bias due to censoring in Kendall's tau estimators for quasi-independence of truncation and failure,
#' \emph{Computational Statistics & Data Analysis}, \bold{73}: 16-26.
#' @references Chiou, S., Austin, M., Qian, J. and Betensky R. A. (2018), Transformation model estimation of survival under dependent truncation and independent censoring,
#' \emph{Statistical Methods in Medical Research}, \bold{28} (12): 3785-3798.
#'
#' @export
#' @example inst/examples/ex_trSurvfit.R
trSurvfit <- function(trun, obs, delta = NULL, tFun = "linear", plots = FALSE, 
                        control = trSurv.control(), ...) {
    ## trun = truncation time
    ## obs = observed failure time
    ## delta = censoring indicator
    out <- NULL
    if (is.null(delta)) delta <- rep(1, length(trun))
    if (class(tFun) == "character") {
        if (tFun == "linear") FUN <- function(X, T, a) (T + a * X) / (1 + a)
        if (tFun == "log") FUN <- function(X, T, a) exp((log(replace(T, 0, 1)) + a * log(X)) / (1 + a))
        if (tFun == "log2") FUN <- function(X, T, a) exp((1 + a) * log(replace(T, 0, 1)) - a * log(X))
        if (tFun == "exp") FUN <- function(X, T, a) log((exp(T) + a * exp(X)) / (1 + a))
    } else {
        FUN <- match.fun(tFun)
    }
    lower <- ifelse(control$lower == -Inf, -.Machine$integer.max, control$lower)
    upper <- ifelse(control$upper == Inf, .Machine$integer.max, control$upper)
    ini <- cKendall(trun, obs, delta)
    ini.ipw <- cKendall(trun, obs, delta, method = "IPW2")    
    sc <- survfit(Surv(trun, obs, 1 - delta) ~ 1)
    S0 <- survfit(Surv(trun, obs, delta) ~ 1) 
    if (length(table(delta)) > 1 & 
        sum(head(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]) == 1) <= 2) {
        sc$time <- sc$time[sc$n.event > 0]
        sc$surv <- exp(-cumsum(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]))
    }
    out$qind <- subset(data.frame(trun = trun, obs = obs, delta = delta), delta == 1)
    out$qind <- out$qind[order(out$qind$obs),]
    y0 <- sort(obs)
    trun1 <- trun[order(obs)][delta[order(obs)] == 1]
    obs1 <- sort(obs[delta == 1])
    delta1 <- delta[delta == 1]
    yi <- unique(obs1)
    byTau <- byP <- NULL
    p0 <- -as.numeric(coef(lm(trun ~ obs))[2])
    byTau$par <- uniroot.all(f = function(x) sapply(x, function(y)
        getA(y, trun1, obs1, sc = sc, FUN = FUN)$PE),
        interval = c(lower + 1e-5, upper))
    if (length(byTau$par) > 0) {
        byTau$par <- unique(c(uniroot.all(f = function(x)
            sapply(x, function(y) getA(y, trun1, obs1, sc = sc, FUN = FUN)$PE),
            interval = c(lower + 1e-5, byTau$par[1])), byTau$par))
        byTau$par <- sort(byTau$par)
        byTau$obj <- sapply(byTau$par, function(x) getA(x, trun1, obs1, sc = sc, FUN = FUN)$PE)
    } else {
        grids <- seq(lower + 1e-5, upper, length.out = 30)
        tmp <- sapply(1:29, function(y) 
            optimize(f = function(x) abs(getA(x, trun1, obs1, delta1, sc = sc, FUN = FUN)$PE),
                     tol = .01, interval = c(grids[y], grids[y + 1])))
        byTau$par <- as.numeric(tmp[1, which.min(tmp[2,])])
        byTau$obj <- as.numeric(tmp[2, which.min(tmp[2,])])
    }
    suppressWarnings(tmpP <- optimize(f = function(x) getA(x, trun1, obs1, delta1, sc = sc, FUN = FUN)$p.value, interval = c(lower, 2 * byTau$par[1] + 1), maximum = TRUE))
    byP$par <- tmpP$maximum
    byP$obj <- tmpP$objective
    tmp <- getA(byTau$par[1], trun1, obs1, sc = sc, FUN = FUN)$p.value
    byP$par <- ifelse(!is.na(tmp) & tmp > byP$obj, byTau$par[1], byP$par)
    byP$obj <- ifelse(!is.na(tmp) & tmp > byP$obj, tmp, byP$obj)
    if (abs(byTau$obj[1]) > 0.1)
        warning("Best estimate does not achieve tau_c = 0. This model may not be appropriate.", call. = FALSE)
    a <- byTau$par[1]
    out$qind$ta <- ta <- mapply(FUN, X = obs1, T = trun1, a = a)
    out$qind$wgtT <- approx(sc$time, sc$surv, ta, "constant", yleft = 1, yright = min(sc$surv))$y
    out$qind$wgtX <- approx(sc$time, sc$surv, obs1, "constant", yleft = 1, yright = min(sc$surv))$y
    ## Turnbull's algorithm
    scX <- approx(sc$time, sc$surv, method = "constant", f = 0,
                  xout = yi, yleft = 1, yright = min(sc$surv))$y
    aij <- sapply(1:length(yi), function(x) 1 * (yi[x] == obs1))
    bij <- sapply(1:length(yi), function(x) 1 * (yi[x] >= ta))
    f0 <- rep(1 / length(yi), length(yi))
    tb <- squarem(par = f0, fixptfn = tauEm, objfn = tauLik, aij = aij, bij = bij,
                  obs = obs1, ta = ta)
    f1 <- tb$par / scX
    f1 <- f1 / sum(f1)
    Sy <- approx(yi, 1 - cumsum(f1), method = "constant", xout = y0, yleft = 1,
                 yright = min(1 - cumsum(f1)))$y
    ## Weighted KM
    ## s0 <- survfit(Surv(ta, yi, rep(1, length(yi))) ~ 1, weights = 1/scX)
    ## if (sum(s0$n.risk == 1) > 0) {
    ## s0 <- survfit(Surv(ta, yi, rep(1, length(yi))) ~ 1, weights = 1/scX,
    ##               subset = -which.min(abs(yi - s0$time[s0$n.risk == 1])))
    ## }
    ## Sy <- approx(s0$time, s0$surv, method = "constant", xout = y0, yleft = 1, yright = 0)$y
    ## Make plots 
    if (plots) {
        op1 <- par(mfrow = c(2,1), oma = c(1,1,1,1) + 0.1, mar = c(3.7,3,1,1) + 0.2)
        plot(trun, obs, cex = .4, main = "", xlab = "", ylab = "", pch = 4)
        mtext("x: uncensored   o: censored", 3, line = .1, cex = .9)
        mtext(expression(bold("Before transformation")), 3, line = .8, cex = 1.2)
        points(trun[delta == 0], obs[delta == 0], cex = .4, pch = 1)
        title(xlab = "Truncation times", ylab = "Failure times", line = 2, cex.lab = 1)
        plot(ta, obs1, cex = .4, main = "", xlab = "", ylab = "", pch = 4)
        mtext("x: uncensored   o: censored", 3, line = .1, cex = .9)
        mtext(expression(bold("After transformation (uncensored objects only)")), 3, line = .8, cex = 1.2)
        points(trun[delta == 0], obs[delta == 0], cex = .4, pch = 1)
        title(xlab = "Truncation times", ylab = "Failure times", line = 2, cex.lab = 1)
        par(op1)
        op1 <- par(mfrow = c(2,1), oma = c(1,1,1,1) + 0.1, mar = c(3.7,3,1,1.5) + 0.2, ask = TRUE)
        if (a > lower + .2)
            run <- c(seq(lower, max(lower, a - .2), length = 30),
                     seq(max(lower, a - .2), a + .2, .01),
                     seq(a + .2, 2 * a + 1, length = 30)) + 1e-5
        if (a <= lower + .2)
            run <- sort(unique(c(seq(lower, 2 * a + 1.01, length = 80) + 1e-5, a)))
        Ytau <- sapply(run, function(x) getA(x, trun1, obs1, delta1, sc, FUN)$PE)
        if (min(abs(Ytau), na.rm = TRUE) < byTau$obj[1]) {
            a <- byTau$par[1] <- run[which.min(abs(Ytau))]
            byTau$obj[1] <- min(abs(Ytau))
        }
        plot(run, Ytau, "l", xlab = "", ylab = "", xlim = c(max(min(run),lower), min(max(run), upper)), 
             ylim = range(Ytau, na.rm = TRUE), 
             main = paste("Estimate a by restricted IPW Kendall's tau", " (a = ", round(a, 3), ")",
                          sep = ""))
        title(xlab = "a", ylab = expression(tau[c]), line = 2, cex.lab = 1)
        abline(v = a, lty = 3)
        abline(h = byTau$obj[1], lty = 3)
        text(x = max(run), y = byTau$obj[1], labels = format(round(byTau$obj[1], 2), nsmall = 2),
             pos = 4, cex = .8, xpd = TRUE, srt = 0, offset = 1.5)
        if (byP$par > lower + .2)
            run <- c(seq(lower, max(lower, byP$par - .2), length = 30),
                     seq(max(lower, byP$par - .2), byP$par + .2, .01),
                     seq(byP$par + .2, 2 * byP$par + 1, length = 30)) + 1e-5
        if (byP$par <= lower + .2)
            run <- sort(unique(c(seq(lower, 2 * byP$par + 1.01, length = 80) + 1e-5, byP$par)))
        Yp <- sapply(run, function(x) getA(x, trun1, obs1, delta1, sc, FUN)$p.value)
        if (max(Yp, na.rm = TRUE) > byP$obj) {
            byP$par <- run[which.max(Yp)]
            byP$obj <- max(Yp, na.rm = TRUE)
        }
        plot(run, Yp, "l", xlab = "", ylab = "", xlim = c(max(min(run),lower), min(max(run), upper)),
             ylim = range(Yp, na.rm = TRUE), 
             main = paste("Estimate a by p-value", " (a = ", round(byP$par, 3), ")", sep = ""))
        title(xlab = "a", ylab = "p-values", line = 2, cex.lab = 1)
        abline(v = byP$par, lty = 3)
        abline(h = byP$obj, lty = 3)
        if (byP$obj >= 0.01)
            text(x = max(run), y = byP$obj, labels = format(round(byP$obj, 2), nsmall = 2),
                 pos = 4, cex = .8, xpd = TRUE, srt = 0, offset = 1.5)
        par(op1)
        op2 <- par(mar = c(3.5, 3.5, 2.5, 2.5), ask = TRUE)
        plot(sort(obs), Sy, xlab = "", ylab = "", "s")
        mtext(expression(bold("Survival estimation")), 3, line = .5, cex = 1.2)
        title(xlab = "Time", ylab = "Survival probability", line = 2, cex.lab = 1)
        lines(S0$time, S0$surv, lty = 2, "s")
        legend("topright", c("Transformation", "Kaplan-Meier"), lty = 1:2, bty = "n")
        par(op2)
    }
    out$surv <- data.frame(Time = y0, trSurv = Sy,
                           kmSurv = approx(S0$time, S0$surv, method = "constant", xout = y0, yleft = 1, yright = min(S0$surv))$y)
    out$byTau <- byTau
    out$byP <- byP
    ## out <- list(Sy = Sy, byTau = byTau, byP = byP,
    ##             qind = data.frame(trun = ta, obs = obs1, wgtT = wgtT, wgtX = wgtX))
    out$Call <- match.call()
    out$iniKendall <- ini$PE
    out$iniKendall.ipw <- ini.ipw$PE
    out$iniP <- ini$p.value
    out$iniP.ipw <- ini.ipw$p.value
    out$tFun <- FUN
    out$.data <- data.frame(start = trun, stop = obs, status = delta)
    class(out) <- "trSurvfit"
    out
}

#' Auxiliary for Controlling trSurvfit Fitting
#'
#' Auxiliary function as user interface for \code{trSurvfit} fitting.
#'
#' @param interval a vector containing the end-points of the interval to be searched the transformation parameter.
#' @param lower the lower end-point of the interval to be searched.
#' @param upper the upper end-point of the interval to be searched.
#'
#' @export
#' @seealso \code{\link{trSurvfit}}
trSurv.control <- function(interval = c(-1, 20), lower = min(interval), upper = max(interval)) {
    list(lower = lower, upper = upper)
}

getA <- function(a, trun, obs, delta = NULL, sc = NULL, FUN, test = "CK") {
    if (is.null(delta)) delta <- rep(1, length(trun))
    FUN <- match.fun(FUN)
    ta <- mapply(FUN, X = obs, T = trun, a = a)
    if (is.null(sc)) {
        if (test == "CK") tmp <- cKendall(ta, obs, delta)
        if (test == "PC") tmp <- pmcc(ta[delta == 1], obs[delta == 1])
     } else {
        weights <- approx(sc$time, sc$surv, method = "constant", xout = c(ta, obs),
                          yleft = 1, yright = min(sc$surv))$y
        tmp <- cKendall(ta, obs, delta, method = "IPW2", weights = weights)
    }
    return(list(PE = tmp$PE, p.value = tmp$p.value))
}

tauEm <- function(f0, aij, bij, obs, ta) {
    uij <- t(t(aij) * f0) / colSums(t(aij) * f0)
    vij <- t(t(1 - bij) * f0) / colSums(t(bij) * f0)
    colSums(uij + vij) / sum(uij + vij)
}

tauLik <- function(f0, aij, bij, obs, ta) {
    yi <- unique(obs)
    yi <- yi[order(yi)]
    de <- rev(cumsum(rev(f0 * c(0, diff(yi)))))
    L <- sapply(1:length(f0), function(x) f0[x] / de[min(which(ta[order(obs)][x] <= yi))])
    -sum(log(ifelse(L <= 0, 1, L)))
}

gofPlot <- function(trun, obs, delta = NULL) {
    n <- length(obs)
    if (is.null(delta)) delta <- rep(1, length(trun))
    sc <- survfit(Surv(trun, obs, 1 - delta) ~ 1)
    if (length(table(delta)) > 1 & 
        sum(head(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]) == 1) <= 2) {
        sc$time <- sc$time[sc$n.event > 0]
        sc$surv <- exp(-cumsum(sc$n.event[sc$n.event > 0]/sc$n.risk[sc$n.event > 0]))
    }
    fit <- trSurvfit(trun, obs, delta)
    ta <- fit$qind$trun
    xa <- fit$qind$obs
    Fw <- survfit(Surv(-obs, -trun, rep(1, length(obs))) ~ 1, data = fit$qind)
    Fwz <- approx(-Fw$time, Fw$surv, method = "constant", xout = unique(sort(xa)), yleft = 0, yright = 1)$y
    fx <- diff(c(0, 1 - fit$Sy))
    fxz <- approx(sort(obs), fx, method = "constant", yleft = 0, yright = 0, xout = unique(sort(xa)))$y
    Scz <- approx(sc$time, sc$surv, method = "constant", yleft = 0, yright = 1, xout=unique(sort(xa)))$y
    gof <- fxz * Fwz * Scz
    s0 <- survfit(Surv(obs, rep(1, n)) ~ 1)
    plot(unique(sort(xa)), cumsum(gof) / sum(gof), "s", xlab = "Time", main = "Goodness of fit")
    lines(s0$time, 1 - s0$surv, "s", col = 2)
    legend("topleft", c("Transformation GOF", "ECDF"), col = 1:2, lty = 1, bty = "n")
}
