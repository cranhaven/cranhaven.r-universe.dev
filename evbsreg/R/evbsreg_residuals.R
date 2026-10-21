#' Randomized Quantile Residuals for the Log-EVBS Regression Model
#'
#' Computes randomized quantile residuals (Dunn and Smyth, 1996) for a
#' log-EVBS regression fit. Under a correctly specified model these
#' residuals are approximately standard normal, so departures from
#' normality indicate lack of fit.
#'
#' @param X A numeric design matrix with an intercept column.
#' @param t A numeric vector of strictly positive responses.
#'
#' @return A numeric vector of length \code{length(t)} containing the
#'   randomized quantile residuals.
#'
#' @references
#' Dunn, P. K. and Smyth, G. K. (1996). Randomized quantile residuals.
#' \emph{Journal of Computational and Graphical Statistics}, 5, 236--244.
#'
#' @seealso \code{\link{rcoxsnell}}, \code{\link{envelope_qq}},
#'   \code{\link{evbsreg.fit}}.
#'
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure)
#' r <- rqrandomized(X, itajai$wind)
#' shapiro.test(r)
#'
#' @export
rqrandomized <- function(X, t) {
  fit       <- evbsreg.fit(X, t)
  betahat   <- fit$betahat
  alphahat  <- fit$alphahat
  gamahat   <- fit$gamahat
  muhat     <- as.vector(X %*% betahat)
  xi2hat    <- (2 / alphahat) * sinh((log(t) - muhat) / 2)
  Ghat      <- exp(-(1 + gamahat * (xi2hat))^(-1/gamahat))  # log-EVBS cdf
  qnorm(Ghat)
}


#' Cox-Snell Residuals for the Log-EVBS Regression Model
#'
#' Computes Cox-Snell residuals for a log-EVBS regression fit. Under a
#' correctly specified model these residuals form an approximately
#' standard exponential sample.
#'
#' @param X A numeric design matrix with an intercept column.
#' @param t A numeric vector of strictly positive responses.
#'
#' @return A numeric vector of length \code{length(t)} containing the
#'   Cox-Snell residuals.
#'
#' @references
#' Cox, D. R. and Snell, E. J. (1968). A general definition of residuals.
#' \emph{Journal of the Royal Statistical Society, Series B}, 30, 248--275.
#'
#' @seealso \code{\link{rqrandomized}}, \code{\link{envelope_qq}}.
#'
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure)
#' cs <- rcoxsnell(X, itajai$wind)
#' summary(cs)
#'
#' @export
rcoxsnell <- function(X, t) {
  fit       <- evbsreg.fit(X, t)
  betahat   <- fit$betahat
  alphahat  <- fit$alphahat
  gamahat   <- fit$gamahat
  muhat     <- as.vector(X %*% betahat)
  xi2hat    <- (2 / alphahat) * sinh((log(t) - muhat) / 2)
  Ghat      <- exp(-(1 + gamahat * (xi2hat))^(-1/gamahat))
  -log(1 - Ghat)
}


#' Normal Probability Plot with Simulation Envelope
#'
#' Produces a normal probability (QQ) plot of the randomized quantile
#' residuals together with a simulated envelope, reproducing the residual
#' diagnostic figure of the paper. Points falling outside the envelope
#' indicate poor fit.
#'
#' @param X A numeric design matrix with an intercept column.
#' @param t A numeric vector of strictly positive responses.
#' @param nrep Number of simulated samples used to build the envelope
#'   (default \code{100}).
#'
#' @return Invisibly, a list with components \code{r} (observed residuals),
#'   \code{e1} and \code{e2} (lower and upper envelope bounds), and
#'   \code{med} (envelope median). A base-graphics plot is produced as a
#'   side effect.
#'
#' @references
#' Atkinson, A. C. (1985). \emph{Plots, Transformations and Regression}.
#' Oxford University Press.
#'
#' @seealso \code{\link{rqrandomized}}.
#'
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure)
#' \donttest{envelope_qq(X, itajai$wind, nrep = 100)}
#'
#' @importFrom stats qqnorm rnorm
#' @importFrom graphics par
#' @export
envelope_qq <- function(X, t, nrep = 100) {
  r <- rqrandomized(X, t)
  n <- length(r)

  e  <- matrix(0, n, nrep)
  for (i in seq_len(nrep)) {
    r1     <- rnorm(n)
    e[, i] <- sort(r1)
  }

  e1 <- numeric(n)
  e2 <- numeric(n)
  for (i in seq_len(n)) {
    eo <- sort(e[i, ])
    e1[i] <- (eo[round(0.025 * nrep) - 1 +
                  ifelse(round(0.025 * nrep) <= 1, 1, 0)] +
                eo[round(0.025 * nrep) +
                     ifelse(round(0.025 * nrep) <= 1, 1, 0)]) / 2
    e2[i] <- (eo[round(0.975 * nrep) - 1] +
                eo[round(0.975 * nrep)]) / 2
  }
  med   <- apply(e, 1, mean)
  faixa <- range(r, e1, e2, med)

  oldpar <- par("pty")
  on.exit(par(oldpar))
  par(pty = "s")
  qqnorm(r, main = "", ylim = faixa,
         ylab = "Randomized quantile residuals",
         xlab = "Quantiles of N(0,1)",
         cex = 0.75, pch = 16)
  par(new = TRUE)
  qqnorm(e1, axes = FALSE, type = "l", main = "",
         ylim = faixa, lty = 1, xlab = "", ylab = "")
  par(new = TRUE)
  qqnorm(e2, axes = FALSE, type = "l", main = "",
         ylim = faixa, lty = 1, xlab = "", ylab = "")
  par(new = TRUE)
  qqnorm(med, axes = FALSE, type = "l", main = "",
         ylim = faixa, lty = 2, xlab = "", ylab = "")
  invisible(list(r = r, e1 = e1, e2 = e2, med = med))
}
