globalVariables(c("variable", "SE", "Est", "time"))

#' Extend a "\code{qris}" object to a specified range of \eqn{tau} or \eqn{t_0} values.
#'
#' @param x is an qris object or a data.frame returned by plot.qris
#' @param t0s is a vector of range of t0 to plot; when not specified, the default value is from 0 to presently defined \eqn{t_0}
#' @param Qs  is a vector of range of Q to plot; when not specified, the default value is from 5\% to presently defined \eqn{Q}
#' @param nB is the number of multiplier bootstrapping for standard error estimation.
#' @param vari is a character string to choose variables to draw the regression coefficient.
#'
## #' @importFrom dplyr bind_rows
#' @export
qris.extend <- function(x, t0s = NULL, Qs = NULL, nB = NULL, vari = NULL) {
    if (!is.qris(x))
        stop("Must be qris class")
    if (all(is.null(t0s), is.null(Qs))) {
        Qs <- 1:9 / 10
        t0s <- x$para$t0
    } else {
        if (!is.null(Qs)) {
            Qs <- sort(Qs)
            if (min(Qs) <= 0 | max(Qs) >= 1) stop("Qs outside [0, 1]") 
        }
        if (!is.null(t0s)) {
            t0s <- sort(t0s)
            if (min(t0s) <= 0) stop("t0s is negative")
        }
        if (is.null(Qs)) Qs <- x$para$Q
        if (is.null(t0s)) t0s <- x$para$t0
    }
    if (x$para$method == "iterative")
        stop('Effect plot is not yet available for method = "iterative"')
    if (is.null(vari)) vari <- x$varNames
    vari <- intersect(vari, x$varNames)
    if (length(vari) == 0) stop('No matching variable names')
    if (is.null(nB)) nB <- x$para$nB
    if (nB < 0) stop("nB must greater than 0")
    ## if (is.null(x$ggdat)) {
    d <- expand.grid(Qs = Qs, t0s = t0s, KEEP.OUT.ATTRS = F)
    ddd <- apply(d, 1, function(dd) {
        tmp <- update(x, t0 = dd['t0s'], Q = dd['Qs'], nB = nB, data = x$data)
        c(coef(tmp), sqrt(diag(vcov(tmp))))
    })
    nc <- length(x$varNames)
    Est <- as.data.frame(t(ddd[1:nc,]))
    if (nB > 0) SE <- as.data.frame(t(ddd[nc + 1:nc,]))
    d <- d[rep(1:nrow(d), nc),]
    d$variable <- factor(rep(x$varNames, each = ncol(ddd)))
    d$Est <- unlist(Est, use.names = F)
    if (nB > 0) d$SE <- unlist(SE, use.names = F)
    d <- d[complete.cases(d),] ## remove missing
    rownames(d) <- NULL
    ##  } else d <- x$ggdat
    ## d <- bind_rows(d, x$ggdat)
    d <- subset(d, variable %in% vari)
    x$ggdat <- d
    invisible(x)
}


#' Draw 95\% confidence interval by a quantile regression estimator of residual lifetime from survival data
#'
#' @param x is an "\code{qris}" object or a data.frame returned by \code{plot.qris}.
#' @param t0s is a vector of range of \code{t0} to plot;
#' when not specified, the default value is from 0 to presently defined \eqn{t_0}
#' @param Qs  is a vector of range of \code{Q} to plot;
#' when not specified, the default value is from 5\% to presently defined \eqn{Q}
#' @param nB is the number of multiplier bootstrapping for standard error estimation.
#' @param vari is a character string to choose variables to draw the regression coefficient.
#' @param byQs put \code{Q}'s on x-axis; only used when both \code{t0}'s and \code{Q}'s are specified.
## #' @param exportData is a logical variable to specify whether to return the data.frame used to construct \code{ggplot}
#' @param ggextra is a list that contains additional components to apply to the \code{ggplot} output.
#' The \code{ggplot2} library must be loaded in order to utilize this feature. 
#' @param ... for future extension
#'
#' @importFrom stats vcov coef update complete.cases
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line geom_ribbon labs xlab ylab theme
#' @export
#'
#' @method plot qris
#' @return A list contains \code{ggplot} object and the information to generate it.
#' 
#' @example inst/examples/ex_plot.R
plot.qris <- function(x, t0s = NULL, Qs = NULL, nB = NULL, vari = NULL,
                      byQs = FALSE, ggextra = NULL, ...) {
  if (is.null(nB)) nB <- x$para$nB
  if (is.null(x$ggdat)) 
      x <- qris.extend(x, t0s = t0s, Qs = Qs, nB = nB, vari = vari)
  if (!is.null(t0s) && any(is.na(match(t0s, unique(x$ggdat$t0ss))))) {
      x <- qris.extend(x, t0s = t0s, Qs = Qs, nB = nB, vari = vari)
  }  
  if (!is.null(Qs) && any(is.na(match(Qs, unique(x$ggdat$Qs))))) {
      x <- qris.extend(x, t0s = t0s, Qs = Qs, nB = nB, vari = vari)
  }
  d <- x$ggdat
  if (length(unique(d$t0s)) == 1) byQs <- TRUE
  if (byQs) ## Q on x-axis
      p <- ggplot(d, aes(x = Qs, y = Est, col = variable)) +
          geom_line() +
      facet_wrap(~ factor(t0s, labels = paste0("Base time is ", unique(t0s)))) +
      xlab("Quantiles")
  else   
    ## t0s on x-axis
      p <- ggplot(d, aes(x = t0s, y = Est, col = variable)) +
          geom_line() +
      facet_wrap(~ factor(Qs, labels = paste0("Quantile is ", unique(Qs)))) + 
      xlab("Base times") 
  if (nB > 0) 
    p <- p + geom_ribbon(aes(ymax = Est + 1.96 * SE, ymin = Est - 1.96 * SE, fill = variable),
                         linetype = 2, alpha = .2, show.legend = FALSE)
  p + labs(color = "Covariates") + ylab("Regression coefficients") + ggextra
}

