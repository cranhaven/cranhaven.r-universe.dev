#' The Generic \code{plot} Function for Object of \code{coxphMIC} Class
#'
#' @name plot.coxphMIC
#' @param x an object of \code{coxphMIC} class.
#' @param conf.level	confidence level used for error bar plots. Default is 0.95.
#' @param mar margin in terms of the number of lines to be specified on the four sides of the plot
#' @param horizontal  Logical indicator of horizontal alignment. Default is \code{TRUE}.
#' @param ... further arguments passed to or from other methods.
#' @details
#' The (generic) plot method for an \code{coxphMIC} object. It plots MIC estimates of
#' gamma and beta. For 0 beta estimates, their corresponding
#' SE are reset to 0 to make the plot.
#' @return Error bar plots for estimated gamma and beta at a given confidenve level.
#' @references
#'\itemize{
#' \item Abdolyousefi, R. N. and Su, X. (2016). \bold{coxphMIC}: An R package for sparse estimation of Cox PH Models via approximated information criterion. Tentatively accepted, \emph{The R Journal}.
#' \item Su, X. (2015). Variable selection via subtle uprooting.
#' \emph{Journal of Computational and Graphical Statistics}, \bold{24}(4): 1092--1113.
#' URL \url{http://www.tandfonline.com/doi/pdf/10.1080/10618600.2014.955176}
#' \item Su, X., Wijayasinghe, C. S., Fan, J., and Zhang, Y. (2015). Sparse estimation of Cox proportional
#' hazards models via approximated information criteria. \emph{Biometrics}, \bold{72}(3): 751--759.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/biom.12484/epdf}
#' }
#' @seealso \code{\link[coxphMIC]{coxphMIC}}
#' @export


plot.coxphMIC <- function(x, conf.level=0.95, horizontal=TRUE, mar=rep(4.5, 4), ...)
{
  gamma <- x$gamma; se.gamma <- x$se.gamma;
  beta <- x$beta; se.beta <- x$se.beta;

  z0 <- stats::qnorm(1-(1-conf.level)/2)
  lb.gamma <- gamma-z0*se.gamma; ub.gamma <- gamma+z0*se.gamma
  lb.beta <- beta-z0*se.beta; ub.beta <- beta+z0*se.beta
  y.min <- min(lb.gamma, stats::na.omit(lb.beta))
  y.max <- max(ub.gamma, stats::na.omit(ub.beta))
  p <- length(gamma); xnames <- row.names(x$result)


  if (horizontal) graphics::par(mfrow=c(1, 2), mar=mar, ...)
  else graphics::par(mfrow=c(2, 1), mar=mar, ...)
  # (a) ERROR BAR PLOT OF GAMMA
  graphics::plot(1:p, gamma, ylim=c(y.min, y.max), pch=19, ylab=expression(tilde(gamma)),
       xlab="", xaxt="n", cex=1.5, col="blue",
      main=expression(paste("(a) Error Bar for ", gamma, sep="")), ...)
  graphics::arrows(1:p, lb.gamma, 1:p, ub.gamma, length=0.05, angle=90, code=3, col="lightblue")
  # mtext(text=xnames, side = 1, line=-0.2, outer = FALSE, at = 1:p,
  #      las=3, cex=0.5, col ="black")   # CANNOT ROTATE WITH mtext
  graphics::text(x = 1:p, graphics::par("usr")[3]-0.01, labels = xnames, srt = 45,
      cex=0.8, pos = 1, xpd = TRUE)
  graphics::abline(h=0, lwd=3, col="gray60")

  # (b) ERROR BAR PLOT OF GAMMA
  col0 <- rep("black", p); col0[!is.na(se.beta)] <- "forestgreen"
  graphics::plot(1:p, beta, ylim=c(y.min, y.max), pch=15, ylab=expression(tilde(beta)),
       xlab=NA, xaxt="n", cex=1.5, col=col0,
       main=expression(paste("(b) Error Bar for ", beta, sep="")), ...)
  graphics::arrows((1:p)[!is.na(lb.beta)], lb.beta[!is.na(lb.beta)], (1:p)[!is.na(lb.beta)], ub.beta[!is.na(lb.beta)],
         length=0.05, angle=90, code=3, col="darkseagreen")
  graphics::text(x = 1:p, graphics::par("usr")[3]-0.01, labels = xnames, srt = 45,
       cex=0.8, pos = 1, xpd = TRUE, col=col0)
  graphics::abline(h=0, lwd=3, col="gray60")
}


