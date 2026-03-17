#' @name xtable
#' @title \code{xtable} methods
#' @description \code{xtable} methods
#'
#' @param x
#' An object with an xtable method.
#' @param caption Caption.
#' @param label Label.
#' @param align Alignment of columns.
#' @param digits Number of digits to display.
#' @param display How to display - passed to \code{formatC}.
#' @param ... Additional arguments (not implemented).
#' 
#' @return An \code{xtable}, suitable for use with/ parsing by LaTeX.
#'
#' @note
#' \code{xtable.survfit} - this does \emph{not} show the (restricted) mean survival, 
#'  only the median with confidence intervals.
#' 
#' @seealso ? xtable
#' ? xtable::print.xtable
#' methods("xtable")
#'
#' @rdname xtable
#' @export
#' 
xtable <- function (x,
                    caption=NULL,
                    label=NULL,
                    align=NULL,
                    digits=NULL, 
                    display=NULL, ...) {
    UseMethod("xtable")
}
#'
#' @rdname xtable
#' @method xtable table
#' @aliases xtable.table
#' @export
#' 
#' @examples
#' data("kidney", package="KMsurv")
#' xtable(with(kidney, table(delta, type)))
#' 
xtable.table <- function(x,
                         caption=paste0(
                           paste(names(dimnames(x)),
                                 collapse=" $\\times$ "),
                           "\\\\ chi-sq=",
                           signif(suppressWarnings(
                               stats::chisq.test(x)$p.value), digits)),
                         label=NULL,
                         align=c("l", rep("c", dim(x)[2])),
                         digits=2,
                         display=NULL, ...){
  identity(caption)
  dn1 <- dimnames(x)
  for (i in 1:length(dn1)) dn1[[i]][which(is.na(dn1[[i]]))] <- "NA"
  n1 <- names(dn1)
  names(attr(x, "dimnames")) <- NULL
  rownames(x) <- NULL
  if (length(dim(x))==1) {
    dim(x) <- c(1L, dim(x))
    r1 <- x
    colnames(r1) <- dn1[[1]]
    rownames(r1) <- "."
    class(r1) <- "matrix"
  } else {
    r1 <- rbind(dn1[[2]], signif(x, digits))
    rownames(r1) <- c(n1[1], dn1[[1]])
    colnames(r1) <- c(n1[2], rep(".", dim(x)[2] - 1))
  }
  xtable::xtable(r1,
                 caption=caption,
                 label=label,
                 align=align,
                 digits=digits,
                 display=display, ...)
}
#' 
#' @rdname xtable
#' @method xtable survfit
#' @aliases xtable.survfit
#' @export
#' 
#' @examples
#' ## K&M. Example 7.2, pg 210.
#' xtable(survfit(Surv(time=time, event=delta) ~ type, data=kidney))
#' 
#' 
xtable.survfit <- function (x,
                            caption=paste0("Survival for ", deparse(x$call[[2]])),
                            label=NULL,
                            align=c("l", rep("c", 7)),
                            digits=NULL, 
                            display=rep("fg", 8), ...) {
    m1 <- survMean(x)
    xtable::xtable(m1,
                   caption=caption, label=label,
                   align=align, digits=digits,
                   display=display, ...)
}

#### hidden functions
### these are based on
## survival:::survmean
minMin <- function(y, x) {
    tolerance <- .Machine$double.eps^0.5
    keep <- (!is.na(y) & y < (0.5 + tolerance))
    if (!any(keep)) {
        return(NA)
    } else {
        x <- x[keep]
        y <- y[keep]
        if (abs(y[1] - 0.5) < tolerance && any(y < y[1])) {
            return((x[1] + x[min(which(y < y[1]))]) / 2)
        } else {
            return(x[1])
        }
    }
}
printFun <- function(nused, time,
                     surv, n.risk, n.event,
                     lower, upper,
                     start.time, end.time) {
    if (!is.na(end.time)) {
        hh <- ifelse((n.risk - n.event) == 0,
                     0,
                     n.event / (n.risk * (n.risk - n.event)))
        keep <- which(time <= end.time)
        if (length(keep) == 0) {
            temptime <- end.time
            tempsurv <- 1
            hh <- 0
        } else {
            temptime <- c(time[keep], end.time)
            tempsurv <- c(surv[keep], surv[max(keep)])
            hh <- c(hh[keep], 0)
        }
        n <- length(temptime)
        delta <- diff(c(start.time, temptime))
        rectangles <- delta * c(1, tempsurv[-n])
        varmean <- sum(cumsum(rev(rectangles[-1]))^2 * rev(hh)[-1])
        mean <- sum(rectangles) + start.time
    } else {
        mean <- 0
        varmean <- 0
    }
    med <- minMin(surv, time)
    if (!is.null(upper)) {
        upper <- minMin(upper, time)
        lower <- minMin(lower, time)
        return(c(nused, max(n.risk),
                 n.risk[1], sum(n.event),
                 sum(mean), sqrt(varmean),
                 med, lower, upper))
    } else {
        return(c(nused, max(n.risk),
                 n.risk[1], sum(n.event),
                 sum(mean), sqrt(varmean),
                 med, 0, 0))
    }
}
survMean <- function(x, scale=1, rmean="none") {
    if (!is.null(x$start.time)) {
        start.time <- x$start.time
    } else {
        start.time <- min(0, x$time)
    }
    stime <- x$time / scale
    surv <- x$surv
    plab <- c("records", "n.max", "n.start", "events", "*rmean", 
              "*se(rmean)", "median",
              paste(x$conf.int, c("LCL", "UCL"), sep = ""))
    ncols <- 9
    if (is.null(x$strata)) {
        end.time <- NA
        if (is.matrix(surv)) {
            out <- matrix(0, ncol(surv), ncols)
            for (i in 1:ncol(surv)) {
                if (is.null(x$conf.int)) { 
                    out[i, ] <- printFun(x$n, stime,
                                     surv[, i], x$n.risk, x$n.event,
                                     NULL, NULL,
                                     start.time, end.time)
                } else {
                    out[i, ] <- printFun(x$n, stime,
                                     surv[, i], x$n.risk, x$n.event,
                                     x$lower[, i], x$upper[, i],
                                     start.time, end.time)
                }
            }
            dimnames(out) <- list(dimnames(surv)[[2]], plab)
        } else {
            out <- matrix(printFun(x$n, stime, surv,
                               x$n.risk, x$n.event, 
                               x$lower, x$upper,
                               start.time, end.time), nrow = 1)
            dimnames(out) <- list(NULL, plab)
        }
    } else {
        nstrat <- length(x$strata)
        stemp <- rep(1:nstrat, x$strata)
        last.time <- (rev(x$time))[match(1:nstrat, rev(stemp))]
        end.time <- rep(NA, nstrat)
        if (is.matrix(surv)) {
            ns <- ncol(surv)
            out <- matrix(0, nstrat * ns, ncols)
            if (is.null(dimnames(surv)[[2]])) {
                dimnames(out) <- list(rep(names(x$strata),
                                          rep(ns, nstrat)), plab)
            } else {
                cname <- outer(dimnames(surv)[[2]],
                               names(x$strata), 
                               paste, sep = ", ")
                dimnames(out) <- list(c(cname), plab)
            }
            k <- 0
            for (i in 1:nstrat) {
                who <- (stemp == i)
                for (j in 1:ns) {
                    k <- k + 1
                    if (is.null(x$lower)) {
                        out[k, ] <- printFun(x$n[i], stime[who],
                                             surv[who, j], x$n.risk[who], x$n.event[who],
                                             NULL, NULL,
                                             start.time, end.time[i])
                    } else {
                        out[k, ] <- printFun(x$n[i], stime[who],
                                             surv[who, j], x$n.risk[who], x$n.event[who],
                                             x$lower[who, j], x$upper[who, j],
                                             start.time, end.time[i])
                    }
                }
            }
        } else {
            out <- matrix(0, nstrat, ncols)
            dimnames(out) <- list(names(x$strata), plab)
            for (i in 1:nstrat) {
                who <- (stemp == i)
                if (is.null(x$lower)) {
                    out[i, ] <- printFun(x$n[i], stime[who],
                                     surv[who], x$n.risk[who], x$n.event[who],
                                     NULL, NULL, 
                                     start.time, end.time[i])
                } else {
                    out[i, ] <- printFun(x$n[i], stime[who],
                                         surv[who], x$n.risk[who], x$n.event[who],
                                         x$lower[who], x$upper[who],
                                         start.time, end.time[i])
                }
            }
        }
    }
    if (is.null(x$lower)) out <- out[, 1:7, drop=FALSE]
    out <- out[, -(5:6), drop=FALSE]
    return(out[, , drop=FALSE])
}

