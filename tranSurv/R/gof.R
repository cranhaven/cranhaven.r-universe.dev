globalVariables(c("start", "status")) ## global variables for gof()

#' Goodness of fit based on left-truncated regression model
#'
#' Provide goodness-of-fit diagnostics for the transformation model.
#'
#' The goodness of fit assessment of the transformation model focus on the structure of the
#' transformation model, which has the form:
#' \deqn{h(U) = (1 + a)^{-1} \times (h(T) + ah(X)),}
#' where \eqn{T} is the truncation time, \eqn{X} is the observed failure time,
#' \eqn{U} is the transformed truncation time that is quasi-independent from \eqn{X} and
#' \eqn{h(\cdot)} is a monotonic transformation function.
#' With the condition, \eqn{T < X}, assumed to be satisfied,
#' the structure of the transformation model implies
#' \deqn{X - T = -(1 + a) E(U) + (1 + a) X - (1 + a) \times [U - E(U)] := \beta_0 + \beta_1X + \epsilon.}
#' The regression estimates can be obtained by the left-truncated regression model (Karlsson and Lindmark, 2014).
#' To evaluate the goodness of fit of the transformation model,
#' the \code{gof()} function directly test the linearity in \eqn{X} by considering larger model that are nonlinear in \eqn{X}.
#' In particular, we expand the covariates \eqn{X} to \code{P} piecewise linearity terms and test for equality of the associated coefficients.
#' 
#' 
#' @param x an object of class \code{trSurvfit} returned by the \code{trSurvfit()} or the \code{trReg()} function or a survival object returned by the \code{Surv()}.
#' @param B an integer value specifies the bootstrap size for the left-truncated regression model. A value greater than 2 is required. 
#' @param P an integer value specifies number of breakpoints to test the linearity of the transformation model.
#' When \code{P > 0}, \eqn{P} breakpoints divides the event times into \eqn{P} equal spaced segments.
#' Piecewise linear function constructed from those segments of event times  are used in the left-truncated regression model,
#' and the overall significance testing if the coefficient estimates are equal is reported. 
#' Default value for \code{P} is 1. See \bold{Details} for a description of the goodness of fit procedure. 
#' 
#' @export
#' @example inst/examples/ex_gof.R
#' @importFrom stats pchisq quantile complete.cases
#' @importFrom utils combn
#' @importFrom graphics boxplot
#' @importFrom methods is
#' 
#' @references Karlsson, M., Lindmark, A. (2014) truncSP: An R Package for Estimation of Semi-Parametric Truncated Linear Regression Models, \emph{Journal of Statistical Software}, \bold{57} (14), pp 1--19.
#' @return A list containing the following elements
#' \describe{
#'   \item{coefficients}{the regression coefficients of the left-truncated regression model.}
#'   \item{pval}{the p-value for the equality of the piecewise linearity terms in the expanded model. See \bold{Details}.}
#'   \item{input}{the class of the inputted object, \code{x}.}
#'   \item{dat.gof}{a data frame used in fitting the inputted model \code{x}.}
#' }
gof <- function(x, B = 200, P = 1) {
    if (all(!is.trReg(x), !is.trSurvfit(x), !is.Surv(x)))
        stop("An 'trReg', 'trSurvfit', or 'Surv' xect is required.")
    out <- NULL
    input <- class(x)
    if (is.trReg(x) || is.trSurvfit(x)) {
        B <- max(x$B, B, 2)
        P <- max(x$P, P, 1)
        Q <- x$Q
    }
    if (is.Surv(x)) {
        B <- max(B, 2)
        P <- max(P, 1)
        Q <- 0
        x <- list(.data = as.data.frame(unclass(x)))
    }
    ti <- x$.data$stop[x$.data$status > 0]
    rm <- (ti %in% boxplot(ti, plot = FALSE)$out)
    ti <- ti[!rm]
    ti <- seq(min(ti), max(ti), length.out = P + 2)
    out <- getTL(x$.data$start[!rm], x$.data$stop[!rm], ti[-c(1, P + 2)], B)
    out$input <- input
    out$breaks <- ti
    sc <- survfit(Surv(start, stop, 1 - status) ~ 1, data = x$.data)
    if (min(sc$surv) == 0) 
        sc$surv <- ifelse(sc$surv == min(sc$surv), sort(unique(sc$surv))[2], sc$surv)
    if (is.trReg(x)) {
        out$fitQs <- lapply(split(x$.data, cut(x$.data$stop, ti)), function(d) {
            tmp <- trReg(Surv(start, stop, status) ~ as.matrix(d[,x$vNames]), data = d,
                         method = x$method,
                         control = list(sc = list(time = sc$time, surv = sc$surv), Q = Q))
            tmp$.data$ta <- with(tmp$.data, x$tFun(stop, start, tmp$a))
            tmp$.data$a <- tmp$a
            return(tmp$.data)
        })
        out$dat.gof <- do.call(rbind, out$fitQs)
        if (x$method == "adjust" & Q > 0) {
            tq <- quantile(out$dat.gof$ta, 0:(1 + Q) / (1 + Q))
            tmp <- model.matrix( ~ cut(out$dat.gof$ta, breaks = tq,
                                       include.lowest = TRUE) - 1)
            nn <- NULL
            tq <- round(tq, 4)
            for (i in 1:(Q + 1)) nn[i] <- paste("T'(a) in (", tq[i], ", ", tq[i + 1], "]", sep = "")
            colnames(tmp) <- nn
            out$dat.gof <- cbind(out$dat.gof, tmp) ##[,1])
        }
        colnames(out$dat.gof)[4:(3 + length(x$vNames))] <- x$vNames
    }
    if (is.trSurvfit(x)) {
        out$fitQs <- lapply(split(x$.data, cut(x$.data$stop, ti)), function(d) {
            tmp <- with(d, trSurvfit(start, stop, status, tFun = x$tFun))
            tmp$.data$ta <- with(tmp$.data, x$tFun(stop, start, tmp$byTau$par))
            tmp$.data$a <- tmp$byTau$par
            return(tmp$.data)
        })
        out$dat.gof <- do.call(rbind, out$fitQs)
    }
    class(out) <- "trgof"
    return(out)
}

#' @importFrom truncSP lt
#'
#' @param tt is the truncation time
#' @param yy is the observed survival times (events only)
#' @param ti is the endpoints of grids
#' @param B is the bootstrap size
#'
#' @noRd
getTL <- function(tt, yy, ti, B) {
    dat <- NULL
    dat$tt <- tt
    dat$yy <- yy
    dat$xt <- dat$yy - dat$tt
    dat <- data.frame(dat)
    nCpt <- length(ti)
    covr <- matrix(NA, nrow = nrow(dat), ncol = nCpt + 1)
    covr[,1] <- with(dat, pmin(yy, ti[1]))
    if (length(ti) > 1) {
        covr[, ncol(covr)] <- pmax(dat$yy - ti[2], 0)
        for (i in 2:nCpt)
            covr[,i] <- pmin(pmax(dat$yy - ti[i - 1], 0), ti[i] - ti[i - 1])
    } else covr[,2] <- pmax(dat$yy - ti[1], 0)
    colnames(covr) <- c(sapply(1:(nCpt + 1), function(x) paste("t", x, sep = "")))                  
    fit <- lt(xt ~ covr, data = dat, covar = TRUE, R = B)
    perm <- combn(length(fit$coefficients[-1]), 2)
    con <- matrix(0, ncol = ncol(perm), nrow = length(fit$coefficients[-1]))
    ## overall significance
    for (i in 1:ncol(perm)) {
        con[perm[1,i], i] <- 1
        con[perm[2,i], i] <- -1
    }     
    dif.vet <- t(con) %*% fit$coefficients[-1]
    dif.cov <- (t(con) %*% fit$covariance[-1, -1]) %*% con
    k <- qr(dif.cov)$rank    
    if (k < nrow(dif.cov)) {
        dif.cov <- dif.cov[1:k, 1:k]
        dif.vet <- dif.vet[1:k]
    }
    coefficients <- fit$coefficients
    pval <- 1 - pchisq((t(dif.vet) %*% solve(dif.cov)) %*% dif.vet, length(dif.vet))
    list(coefficients = coefficients, pval = pval)
}
