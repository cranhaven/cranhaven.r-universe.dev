globalVariables("wgtX") ## global variable for within getA2

#' 1. Find quasi-independent latent truncation time by maximizing conditional Kendall's tau based on the uncensored observations only
#' 2. Use the latent truncation time in the Cox model as a truncation time
#'
#' @noRd
#' @keywords internal
trFit.kendall <- function(DF, engine, stdErr) {
    out <- NULL
    trun <- DF$start
    obs <- DF$stop
    delta <- DF$status
    ## Finding the latent truncation time
    trun1 <- trun[delta == 1] ## trun[order(obs)][delta[order(obs)] == 1]
    obs1 <- obs[delta == 1]
    delta1 <- delta[delta == 1]
    wgtX <- approx(engine@sc$time, engine@sc$surv, obs1, "constant", yleft = 1,
                   yright = min(engine@sc$surv))$y
    if (engine@a < -1) {
        ## optimize getA in many grids
        grids <- seq(engine@lower + 1e-5, engine@upper, length.out = engine@G)
        tmp <- sapply(1:(engine@G - 1), function(y)
            optimize(f = function(x) abs(getA(x, trun1, obs1, delta1,
                                              sc = engine@sc, FUN = engine@tFun)$PE),
                     tol = engine@tol, interval = c(grids[y], grids[y + 1])))
        tmp2 <- optim(fn = function(x)
            abs(getA(x, trun1, obs1, delta1, sc = engine@sc, FUN = engine@tFun)$PE), par = 0)
        if (tmp2$par > -1) tmp <- cbind(tmp, c(tmp2$par, tmp2$value))
        a <- as.numeric(tmp[1, which.min(tmp[2,])])
    } else a <- engine@a    
    ta <- mapply(engine@tFun, X = obs1, T = trun1, a = a)
    out$PE <- coef(summary(coxph(Surv(ta, obs1, delta1) ~ as.matrix(DF[delta == 1, engine@vNames]),
                                 weights = 1 / wgtX)))
    out$varNames <- rownames(out$PE) <- engine@vNames
    out$SE <- NA
    out$a <- a
    return(out)
}


#' This is like \code{trFit.kendall} but with piece-wise regression
#'
#' @noRd
#' @keywords internal
trFit.kendall2 <- function(DF, engine, stdErr) {
    out <- NULL
    P <- max(engine@P, 1)    
    ti <- DF$stop[DF$status > 0]
    ti <- ti[!(ti %in% boxplot(ti, plot = FALSE)$out)]
    out$breaks <- ti <- seq(min(ti), max(ti), length.out = P + 2)
    out$breaks[1] <- ti[1] <- -Inf
    out$breaks[length(ti)] <- ti[length(ti)] <- Inf
    p <- length(ti) - 1
    dats <- split(DF, cut(DF$stop, ti, include.lowest = TRUE))
    m <- lapply(dats, nrow)
    pwReg <- lapply(dats, function(d) {
        tmp <- trReg(Surv(start, stop, status) ~ as.matrix(d[, engine@vNames]),
                     data = d, method = "kendall", B = 0, tFun = engine@tFun, 
                     control = list(engine@sc, G = engine@G, Q = engine@Q, a = engine@a, 
                                    tol = engine@tol, lower = engine@lower, upper = engine@upper))
        names(tmp$.data)[-(1:3)] <- engine@vNames
        tmp$.data$ta <- with(tmp$.data, engine@tFun(stop, start, tmp$a))
        tmp$.data$a <- tmp$a
        return(tmp$.data)
    })
    a0 <- unique(do.call(rbind, pwReg)$a)
    getA2 <- function(a) {
        if (any(a <= -1)) return(Inf)
        dat0 <- do.call(rbind, dats)
        dat0$ta <- unlist(sapply(1:p, function(x)
            mapply(engine@tFun, X = dats[[x]]$stop, T = dats[[x]]$start, a[x])))
        dat0$wgtX <- approx(engine@sc$time, engine@sc$surv, dat0$stop, "constant", yleft = 1,
                            yright = min(engine@sc$surv))$y       
        cKendall(dat0$ta, dat0$stop, dat0$status, method = "IPW2", weights = dat0$wgtX)$PE
    }
    a1 <- optim(a0, fn = function(x) getA2(x)^2)
    a2 <- optim(rep(0, p), fn = function(x) getA2(x)^2)
    if (a1$value < a2$value) a <- a1$par
    else a <- a2$par
    DF2 <- do.call(rbind, pwReg)
    DF2$a <- rep(a, unlist(lapply(dats, nrow)))
    DF2$ta <- mapply(engine@tFun, X = DF2$stop, T = DF2$start, a = DF2$a)        
    trun <- DF2$start
    obs <- DF2$stop
    delta <- DF2$status
    ## Finding the latent truncation time
    trun1 <- trun[delta == 1] ## trun[order(obs)][delta[order(obs)] == 1]
    obs1 <- obs[delta == 1]
    delta1 <- delta[delta == 1]
    ta1 <- DF2$ta[delta == 1]
    wgtX <- approx(engine@sc$time, engine@sc$surv, obs1, "constant", yleft = 1,
                   yright = min(engine@sc$surv))$y
    suppressWarnings(out$PE <- coef(summary(coxph(Surv(ta1, obs1, delta1) ~
                                                      as.matrix(DF2[delta == 1, engine@vNames]),
                                                  weights = 1 / wgtX))))
    out$varNames <- rownames(out$PE) <- engine@vNames
    out$SE <- NA
    out$a <- unique(DF2$a)
    return(out)
}

#' @noRd
#' @keywords internal
#' @importFrom rootSolve uniroot.all
trFit.adjust <- function(DF, engine, stdErr) {
    out <- NULL
    trun <- DF$start
    obs <- DF$stop
    delta <- DF$status
    trun1 <- trun[delta == 1]
    obs1 <- obs[delta == 1]
    delta1 <- delta[delta == 1]
    wgtX <- approx(engine@sc$time, engine@sc$surv, obs1, "constant", yleft = 1,
                   yright = min(engine@sc$surv))$y
    if (engine@a < -1) {
        coxAj <- function(a, see = TRUE) {
            ta <- mapply(engine@tFun, X = obs1, T = trun1, a = a)
            if (engine@Q > 0)
                covs <- model.matrix(
                    ~ cut(ta, breaks = quantile(ta, 0:(1 + engine@Q) / (1 + engine@Q)),
                          include.lowest = TRUE) - 1)
            else covs <- ta
            tmp <- coxph(Surv(ta, obs1, delta1) ~
                             as.matrix(DF[delta == 1, engine@vNames]) + covs,
                         weights = 1 / wgtX)
            if (see) {
                return(min(sum(coef(tmp)[-(1:length(engine@vNames))]^2, na.rm = TRUE), 1e4))
            } else {
                out <- as.numeric(coef(tmp)[-(1:length(engine@vNames))])
                return(out[!is.na(out)])
            }
        }
        ## Filter out lower bound that gives warning (fail to converge in coxph)
        if (engine@lower <= -1) {
            boundary <- -1 + log(1 + 10^-(1:5), 10)
            tmp <- sapply(boundary, function(x)
                tryCatch(coxAj(x), warning = function(e) NA, error = function(e) NA))
            engine@lower <- min(boundary[!is.na(tmp)])
        }
        ## optimize in many grids
        grids <- seq(engine@lower, engine@upper, length.out = engine@G)
        grids <- c(grids, sapply(1:max(engine@Q, 1), function(z) 
            uniroot.all(f = function(x) sapply(x, function(y) coxAj(y, see = FALSE)[z]),
                        interval = c(engine@lower, engine@upper))))
        grids <- unique(sort(unlist(grids)))
        tmp <- sapply(1:(length(grids) - 1), function(y)
            optimize(f = function(x) suppressWarnings(coxAj(x)),
                     interval = c(grids[y], grids[y + 1])))
        tmp2 <- suppressWarnings(
            optim(fn = coxAj, par = 0, control = list(warn.1d.NelderMead = FALSE)))
        if (tmp2$par > -1) tmp <- cbind(tmp, c(tmp2$par, tmp2$value))
        a <- as.numeric(tmp[1, which.min(tmp[2,])])
    } else a <- engine@a
    ta <- mapply(engine@tFun, X = obs1, T = trun1, a = a)
    if (engine@Q > 0) {
        tq <- quantile(ta, 0:(1 + engine@Q) / (1 + engine@Q))
        covs <- model.matrix( ~ cut(ta, breaks = tq, include.lowest = TRUE) - 1)
    } else covs <- ta
    out$PE <- coef(summary(coxph(Surv(ta, obs1, delta1) ~
                                     as.matrix(DF[delta == 1,engine@vNames]) + covs,
                                 weights = 1 / wgtX)))
    out$PEta <- out$PE[-(1:(length(engine@vNames))),,drop = FALSE]
    out$PE <- out$PE[1:(length(engine@vNames)),,drop = FALSE]
    if (engine@Q > 0) {
        tq[which.min(tq)] <- -Inf
        tq[which.max(tq)] <- Inf
        nn <- NULL
        tq <- round(tq, 3)
        for (i in 1:(engine@Q + 1)) nn[i] <- paste("T'(a) in (", tq[i], ", ", tq[i + 1], "]", sep = "")
        rownames(out$PEta) <- nn
    } else rownames(out$PEta) <- "T'(a)"
    out$PEta <- out$PEta[complete.cases(out$PEta),,drop = FALSE]
    out$varNames <- rownames(out$PE) <- engine@vNames
    out$SE <- NA
    out$a <- a
    return(out)    
}

#' @noRd
#' @importFrom stats as.formula optim update
#' @keywords internal
trFit.adjust2 <- function(DF, engine, stdErr) {
    out <- NULL
    P <- max(engine@P, 1)    
    ti <- DF$stop[DF$status > 0]
    ti <- ti[!(ti %in% boxplot(ti, plot = FALSE)$out)]
    out$breaks <- ti <- seq(min(ti), max(ti), length.out = P + 2)
    out$breaks[1] <- ti[1] <- -Inf
    out$breaks[length(ti)] <- ti[length(ti)] <- Inf
    p <- length(ti) - 1
    dats <-split(DF, cut(DF$stop, ti, include.lowest = TRUE))
    m <- lapply(dats, nrow)
    pwReg <- lapply(dats, function(d) {
        tmp <- trReg(Surv(start, stop, status) ~ as.matrix(d[, engine@vNames]),
                     data = d, method = "adjust", B = 0, tFun = engine@tFun, 
                     control = list(engine@sc, G = engine@G, Q = engine@Q, P = 0, a = engine@a,
                                    tol = engine@tol, lower = engine@lower, upper = engine@upper))
        names(tmp$.data)[-(1:3)] <- engine@vNames
        tmp$.data$ta <- with(tmp$.data, engine@tFun(stop, start, tmp$a))
        tmp$.data$a <- tmp$a
        return(tmp$.data)
    })    
    a0 <- unique(do.call(rbind, pwReg)$a)
    getA2 <- function(a, model = FALSE) {
        if (any(a <= -1)) return(Inf)
        dat0 <- do.call(rbind, dats)
        dat0$ta <- unlist(sapply(1:p, function(x)
            mapply(engine@tFun, X = dats[[x]]$stop, T = dats[[x]]$start, a[x])))
        dat0$wgtX <- approx(engine@sc$time, engine@sc$surv, dat0$stop, "constant", yleft = 1,
                            yright = min(engine@sc$surv))$y
        if (engine@Q > 0) {
            q1 <- quantile(dat0$ta[dat0$status > 0], 0:(1 + engine@Q)/(1 + engine@Q))
            q1[1] <- -Inf
            q1[length(q1)] <- Inf
            covs <- model.matrix(~ cut(dat0$ta, breaks = q1, include.lowest = TRUE) - 1)
        } else covs <- dat0$ta
        fm <- as.formula(paste("Surv(ta, stop, status) ~ ", paste(engine@vNames, collapse = "+")))
        tmp <- update(coxph(fm, data = dat0, subset = status > 0, weights = 1 / wgtX), ~ . + covs)
        if (model) {
            nn <- NULL
            if (engine@Q > 0) {
                tq <- round(q1, 3)
                for (i in 1:(engine@Q + 1)) nn[i] <- paste("T'(a) in (", tq[i], ", ", tq[i + 1], "]", sep = "")
            } else nn <- "T'(a)"
            return(list(model = tmp,
                        ta = dat0$ta[dat0$status > 0],
                        taName = nn))
        } else return(coef(tmp)[-(1:length(engine@vNames))])
    }
    a <- tryCatch(optim(a0, fn = function(x) sum(getA2(x)^2, na.rm = TRUE))$par,
                  error = function(e)
                      optim(rep(0, p), fn = function(x) sum(getA2(x)^2, na.rm = TRUE))$par)
    tmp <- getA2(a, TRUE)
    f <- tmp$model
    out$PE <- coef(summary(f))
    out$PEta <- out$PE[-(1:(length(engine@vNames))),,drop = FALSE]
    out$PE <- out$PE[1:(length(engine@vNames)),,drop = FALSE]
    rownames(out$PEta) <- tmp$taName
    out$PEta <- out$PEta[complete.cases(out$PEta),,drop = FALSE]
    out$varNames <- rownames(out$PE) <- engine@vNames
    out$SE <- NA
    out$a <- a
    return(out)    
}

#' @importFrom parallel makeCluster clusterExport parSapply stopCluster
#' @noRd
#' @keywords internal
trFit.boot <- function(DF, engine, stdErr) {
    trun <- DF$start
    obs <- DF$stop
    delta <- DF$status
    out <- trFit(DF, engine, NULL)
    win <- (engine@upper - engine@lower) / engine@G
    engine@lower <- max(engine@lower, out$a - win)
    engine@upper <- min(engine@upper, out$a + win)
    engine@G <- max(5, round(engine@G / 10))
    if (stdErr@parallel) {
        cl <- makeCluster(stdErr@parCl)
        clusterExport(cl = cl,
                      varlist = c("DF", "engine"), envir = environment())
        out$SE <- parSapply(cl, 1:stdErr@B, function(x)
            tryCatch(trFit(DF[sample(1:NROW(DF), NROW(DF), TRUE),], engine, NULL)$PE[,1],
                     error = function(e) rep(NA, length(engine@vNames))))
        stopCluster(cl)
    } else out$SE <- replicate(
               stdErr@B,
               tryCatch(trFit(DF[sample(1:NROW(DF), NROW(DF), TRUE),], engine, NULL)$PE[,1],
                        error = function(e) rep(NA, length(engine@vNames))))
    if (nrow(out$PE) > 1) out$SE <- apply(out$SE, 1, sd, na.rm = TRUE)
    else out$SE <- sd(out$SE)
    out
}

#' Class definition
#' @noRd
#' @keywords internal
setClass("Engine",
         representation(tol = "numeric", lower = "numeric", upper = "numeric",
                        G = "numeric", Q = "numeric", P = "numeric", a = "numeric", 
                        tFun = "function", vNames = "character", sc = "list"),
         prototype(tol = 1e-2, lower = -1, upper = 20, G = 50, Q = 0, P = 0, a = -2),
         contains= "VIRTUAL")
setClass("kendall", contains = "Engine")
setClass("adjust", contains = "Engine")

## For piecewise
setClass("kendall2", contains = "Engine") 
setClass("adjust2", contains = "Engine")

setClass("stdErr",
         representation(B = "numeric", parallel = "logical", parCl = "numeric"),
         prototype(B = 100, parallel = FALSE, parCl = parallel::detectCores() / 2),
         contains = "VIRTUAL")
setClass("bootstrap", contains = "stdErr")

#' Method Dispatch
#' @noRd
#' @keywords internal
setGeneric("trFit", function(DF, engine, stdErr) {standardGeneric("trFit")})

setMethod("trFit", signature(engine = "kendall", stdErr = "NULL"), trFit.kendall)
setMethod("trFit", signature(engine = "adjust", stdErr = "NULL"), trFit.adjust)
setMethod("trFit", signature(engine = "kendall2", stdErr = "NULL"), trFit.kendall2)
setMethod("trFit", signature(engine = "adjust2", stdErr = "NULL"), trFit.adjust2)

setMethod("trFit", signature(engine = "kendall", stdErr = "bootstrap"), trFit.boot)
setMethod("trFit", signature(engine = "adjust", stdErr = "bootstrap"), trFit.boot)
setMethod("trFit", signature(engine = "kendall2", stdErr = "bootstrap"), trFit.boot)
setMethod("trFit", signature(engine = "adjust2", stdErr = "bootstrap"), trFit.boot)

#' Fitting regression model via structural transformation model
#'
#' \code{trReg} fits transformation model under dependent truncation and independent censoring via a structural transformation model.
#'
#' The main assumption on the structural transformation model is that it assumes there is a latent, quasi-independent truncation time
#' that is associated with the observed dependent truncation time, the event time, and an unknown dependence parameter
#' through a specified funciton.
#' The structure of the transformation model is of the form:
#' \deqn{h(U) = (1 + a)^{-1} \times (h(T) + ah(X)),} where \eqn{T} is the truncation time, \eqn{X} is the observed failure time,
#' \eqn{U} is the transformed truncation time that is quasi-independent from \eqn{X} and \eqn{h(\cdot)} is a monotonic transformation function.
#' The condition, \eqn{T < X}, is assumed to be satisfied.
#' The quasi-independent truncation time, \eqn{U}, is obtained by inverting the test for quasi-independence by one of the following methods:
#' \describe{
#'   \item{\code{method = "kendall"}}{ by minimizing the absolute value of the restricted inverse probability weighted Kendall's tau or maximize the corresponding \eqn{p}-value.
#' This is the same procedure used in the \code{trSUrvfit()} function.}
#'   \item{\code{method = "adjust"}}{ includes a function of latent truncation time, \eqn{U}, as a covariate.
#'   A piece-wise function is constructed based on (\eqn{Q + 1}) indicator functions on whether \eqn{U} falls in the \eqn{Q}th and the (\eqn{Q+1})th percentile,
#'   where \eqn{Q} is the number of cutpoints used. See \code{control} for details. 
#'   The transformation parameter, \eqn{a}, is then chosen to minimize the significance of the coefficient parameter.}} 
#' 
#' @param formula a formula expression, of the form \code{response ~ predictors}.
#' The \code{response} is assumed to be a \code{survival::Surv} object with both left truncation and right censoring.
#' When there is no covariates, e.g., when the right hand side of the formula is \code{~ 1}, the \code{trReg()} function returns a \code{trSurvfit} object.
#' See \code{?survival::Surv} for more details.
#' @param data  an optional data frame in which to interpret the variables occurring
#'     in the \code{formula}.
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param tFun a character string specifying the transformation function or a user specified function indicating the relationship between \eqn{X}, \eqn{T}, and \eqn{a}.
#' When \code{tFun} is a character, the following are permitted:
#' \describe{
#'   \item{linear}{linear transformation structure,}
#'   \item{log}{log-linear transformation structure,}
#'   \item{exp}{exponential transformation structure.}
#' }
#' @param method a character string specifying how the transformation parameter is estimated. The available options are \code{"kendall"} and \code{"adjust"}. See \bold{Details}.
#' @param B a numerical value specifies the bootstrap size for estimating the standard error.
#' When \code{B = 0} (default), the bootstrap standard errors will not be computed.
#' @param control a list of control parameters. The following arguments are allowed:
#' \describe{
#'   \item{\code{lower}}{The lower bound to search for the transformation parameter; default at -1.}
#'   \item{\code{upper}}{The upper bound to search for the transformation parameter; default at 20.}
#'   \item{\code{tol}}{The tolerance used in the search for the transformation parameter; default at 0.01.}
#'   \item{\code{G}}{The number of grids used in the search for the transformation parameter; default is 50.
#' A smaller \code{G} could results in faster search, but might be inaccurate.}
#'   \item{\code{Q}}{The number of cutpoints for the truncation time used when \code{method = "adjust"}. The default is 0.}
#'   \item{\code{P}}{The number of breakpoints to divide the event times into equally spaced segmenets.
#' When \code{P > 1}, the latent truncation time, \eqn{T'(a)} will be computed in each subset.
#' The transformation model is then applied to the aggregated data.} 
#'   \item{\code{a}}{The transformation parameter. When this is specified, the transformation model is applied based on the specified \code{a}.
#'                   When this is not specified, an optimized \code{a} will be determined by optimization one of the quasi-independence measure. See \bold{Details}.}
#'   \item{\code{parallel}}{an logical value indicating whether parallel computation will be applied when \code{B > 0}.}
#'   \item{\code{parCl}}{an integer value specifying the number of CPU cores to be used when \code{parallel = TRUE}.
#' The default value is half the CPU cores on the current host.}
#' }
#'
#' 
#' @importFrom survival is.Surv coxph
#' @importFrom methods getClass
#' @seealso \code{\link{trSurvfit}}
#' 
#' @export
#' @example inst/examples/ex_trReg.R
trReg <- function(formula, data, subset, tFun = "linear",
                  method = c("kendall", "adjust"),
                  B = 0, control = list()) {
    method <- match.arg(method)
    Call <- match.call()
    engine.control <- control[names(control) %in% names(attr(getClass(method), "slots"))]
    if (max(engine.control$P, 0) > 0)
        engine <- do.call("new", c(list(Class = paste(method, 2, sep = "")), engine.control))
    else engine <- do.call("new", c(list(Class = method), engine.control))
    stdErr.control <- control[names(control) %in% names(attr(getClass("bootstrap"), "slots"))]
    stdErr <- do.call("new", c(list(Class = "bootstrap"), stdErr.control))
    stdErr@B <- B
    if (B == 0) class(stdErr)[[1]] <- "NULL"
    if (class(tFun) == "character") {
        if (tFun == "linear") engine@tFun <- function(X, T, a) (T + a * X) / (1 + a)
        if (tFun == "log") engine@tFun <- function(X, T, a) exp((log(replace(T, 0, 1)) + a * log(X)) / (1 + a))
        if (tFun == "log2") engine@tFun <- function(X, T, a) exp((1 + a) * log(replace(T, 0, 1)) - a * log(X))
        if (tFun == "exp") engine@tFun <- function(X, T, a) log((exp(T) + a * exp(X)) / (1 + a))
    } else {
        engine@tFun <- match.fun(tFun)
    }
    if (missing(data)) {
        resp <- eval(formula[[2]], parent.frame())
        covM <- model.matrix(formula, parent.frame())
    } else {
        resp <- eval(formula[[2]], data)
        covM <- model.matrix(formula, data)
    }
    engine@vNames <- setdiff(colnames(covM), "(Intercept)") # attr(terms(formula), "term.labels")
    if (!is.Surv(resp)) stop("Response must be a Surv resect")
    if (!match("start", attr(resp, "dimnames")[[2]])) stop("Missing left-truncation time")
    engine@lower <- ifelse(engine@lower == -Inf,  -.Machine$integer.max, engine@lower)
    engine@upper <- ifelse(engine@upper == Inf, .Machine$integer.max, engine@upper)
    formula[[2]] <- NULL
    DF <- as.data.frame(unclass(resp))
    if (!length(engine@sc)) {
        sc <- survfit(Surv(start, stop, 1 - status) ~ 1, data = DF)
        if (min(sc$surv) == 0) 
            sc$surv <- ifelse(sc$surv == min(sc$surv), sort(unique(sc$surv))[2], sc$surv)
        engine@sc <- list(time = sc$time, surv = sc$surv)
    }
    if (formula == ~1) {
        out <- trSurvfit(DF$start, DF$stop, DF$status, trans = tFun, plots = FALSE,
                         control = trSurv.control(lower = engine@lower, upper = engine@upper))
        class(out) <- "trSurvfit"
    } else {
        DF <- as.data.frame(cbind(DF, covM)) ## First 3 columns reserved for `start`, `stop`, `status`
        DF <- DF[,(colnames(DF) != "(Intercept)")]
        suppressWarnings(out <- trFit(DF, engine, stdErr))
        class(out) <- "trReg"
    }
    out$Call <- Call
    out$B <- B
    out$Q <- engine@Q
    out$P <- engine@P
    out$tFun <- engine@tFun
    out$vNames <- engine@vNames
    out$method <- method
    out$.data <- DF 
    out
}

is.trReg <- function(x) is(x, "trReg")
is.trSurvfit <- function(x) is(x, "trSurvfit")





## trFit.adjust2 <- function(DF, engine, stdErr) {
##     out <- NULL
##     P <- max(engine@P, 1)    
##     ti <- DF$stop[DF$status > 0]
##     ti <- ti[!(ti %in% boxplot(ti, plot = FALSE)$out)]
##     out$breaks <- ti <- seq(min(ti), max(ti), length.out = P + 2)
##     out$breaks[1] <- ti[1] <- -Inf
##     out$breaks[length(ti)] <- ti[length(ti)] <- Inf
##     pwReg <- lapply(split(DF, cut(DF$stop, ti, include.lowest = TRUE)), function(d) {
##         tmp <- trReg(Surv(start, stop, status) ~ as.matrix(d[, engine@vNames]),
##                      data = d, method = "adjust", B = 0, tFun = engine@tFun, 
##                      control = list(engine@sc, G = engine@G, Q = engine@Q, P = 0, a = engine@a,
##                                     tol = engine@tol, lower = engine@lower, upper = engine@upper))
##         names(tmp$.data)[-(1:3)] <- engine@vNames
##         tmp$.data$ta <- with(tmp$.data, engine@tFun(stop, start, tmp$a))
##         tmp$.data$a <- tmp$a
##         return(tmp$.data)
##     })    
##     DF2 <- do.call(rbind, pwReg)
##     trun <- DF2$start
##     obs <- DF2$stop
##     delta <- DF2$status
##     trun1 <- trun[delta == 1]
##     obs1 <- obs[delta == 1]
##     delta1 <- delta[delta == 1]
##     ta1 <- DF2$ta[delta == 1]
##     wgtX <- approx(engine@sc$time, engine@sc$surv, obs1, "constant", yleft = 1,
##                    yright = min(engine@sc$surv))$y
##     if (engine@Q > 0) {
##         tq <- quantile(ta1, 0:(1 + engine@Q) / (1 + engine@Q))
##         covs <- model.matrix( ~ cut(ta1, breaks = tq, include.lowest = TRUE) - 1)
##     } else covs <- ta1
##     suppressWarnings(out$PE <- coef(summary(coxph(Surv(ta1, obs1, delta1) ~
##                                                       as.matrix(DF2[delta == 1,engine@vNames]) + covs,
##                                                   weights = 1 / wgtX))))
##     out$PEta <- out$PE[-(1:(length(engine@vNames))),,drop = FALSE]
##     out$PE <- out$PE[1:(length(engine@vNames)),,drop = FALSE]
##     if (engine@Q > 0) {
##         tq[which.min(tq)] <- -Inf
##         tq[which.max(tq)] <- Inf
##         nn <- NULL
##         tq <- round(tq, 3)
##         for (i in 1:(engine@Q + 1)) nn[i] <- paste("T'(a) in (", tq[i], ", ", tq[i + 1], "]", sep = "")
##         rownames(out$PEta) <- nn
##     } else rownames(out$PEta) <- "T'(a)"
##     out$PEta <- out$PEta[complete.cases(out$PEta),,drop = FALSE]
##     out$varNames <- rownames(out$PE) <- engine@vNames
##     out$SE <- NA
##     out$a <- unique(DF2$a)
##     return(out)    
