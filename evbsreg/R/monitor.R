#' Prospective endpoint-identifiability index
#'
#' Computes the monitoring index \eqn{F_t} that combines the conformal normal
#' curvature diagnostic with the finite upper endpoint of the fitted model. The
#' index compares the support the model would infer without the observation
#' flagged by the curvature against the largest response actually observed.
#'
#' @details
#' Let \eqn{j^{*}(t)} be the observation with the largest aggregate contribution
#' when the model is fitted to the first \eqn{t} records, and \eqn{y_{\max}(t)}
#' the largest response up to \eqn{t}. Then
#' \deqn{F_t = \hat T^{*}_{(-j^{*}(t))}(t) / y_{\max}(t),}
#' where \eqn{\hat T^{*}_{(-j^{*})}} is the endpoint estimated after deleting the
#' flagged observation, evaluated at its covariate value.
#'
#' Values below one indicate that the model fitted without the flagged
#' observation assigns probability zero to a value that was in fact observed: the
#' endpoint is then determined by a single record and should not be quoted as a
#' design value. The control limit at one follows from the definition and is not
#' a tuning constant.
#'
#' Deleting the sample maximum instead of the flagged observation (\code{use =
#' "max"}) lowers the endpoint essentially by construction and produces an index
#' that signals almost everywhere; it is provided for comparison only.
#'
#' @param X Design matrix (including the intercept column).
#' @param y Response vector, in time order.
#' @param burn Number of initial observations before the index is first computed.
#' @param use Either \code{"curvature"} (default) or \code{"max"}.
#'
#' @return A data frame with the time index, the running maximum, the flagged
#'   observation, the fitted shape parameter and the index \code{F}.
#' @examples
#' \donttest{
#' data(itajai)
#' ct <- evbs_monitor(cbind(1, itajai$pressure), itajai$wind, burn = 40)
#' plot(ct$t, ct$F, type = "l"); abline(h = 1, lty = 2)
#' }
#' @export
evbs_monitor <- function(X, y, burn = 40, use = c("curvature", "max")) {
  use <- match.arg(use)
  X <- as.matrix(X); n <- length(y)
  stopifnot(burn >= ncol(X) + 2, burn < n)
  out <- NULL
  for (t in burn:n) {
    yt <- y[seq_len(t)]; Xt <- X[seq_len(t), , drop = FALSE]
    f <- try(evbsreg.fit(Xt, yt), silent = TRUE)
    if (inherits(f, "try-error")) next
    jm <- which.max(yt)
    if (use == "curvature") {
      B <- try(colMeans(cnc_diagnostics(f)$Bj), silent = TRUE)
      if (inherits(B, "try-error")) next
      j <- which.max(B)
    } else {
      j <- jm
    }
    fj <- try(evbsreg.fit(Xt[-j, , drop = FALSE], yt[-j]), silent = TRUE)
    if (inherits(fj, "try-error")) next
    eta <- sum(Xt[jm, ] * fj$betahat)
    ep <- try(evbs_endpoint(eta, fj$alphahat, fj$gamahat), silent = TRUE)
    if (inherits(ep, "try-error")) next
    out <- rbind(out, data.frame(
      t = t, ymax = max(yt), flagged = j, gama = f$gamahat,
      endpoint = as.numeric(ep), F = as.numeric(ep) / max(yt)
    ))
  }
  structure(out, class = c("evbs_monitor", "data.frame"), use = use)
}

#' Plot an endpoint-identifiability control chart
#'
#' @param x An object returned by \code{\link{evbs_monitor}}.
#' @param ... Further arguments passed to \code{plot}.
#' @return Called for its side effect; invisibly returns \code{x}.
#' @export
plot.evbs_monitor <- function(x, ...) {
  graphics::plot(x$t, x$F, type = "o", pch = 16, cex = 0.5,
                 xlab = "Observation index", ylab = expression(F[t]), ...)
  graphics::rect(min(x$t) - 10, 0, max(x$t) + 10, 1,
                 col = grDevices::rgb(1, 0, 0, 0.07), border = NA)
  graphics::lines(x$t, x$F, lwd = 2)
  graphics::abline(h = 1, lty = 2, lwd = 2)
  invisible(x)
}
