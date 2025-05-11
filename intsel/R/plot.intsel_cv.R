#' Plots for \code{intsel_cv}
#'
#' @description
#' Plot the solution path or cross-validation curves produced by \code{\link{intsel_cv}()}.
#' 
#' @param x The \code{\link{intsel_cv}} object.
#' @param type Character string, "\code{solution-path}" to generate a solution path with marks at \code{lambda.min} and \code{lambda.1se}; "\code{cv-curve}" to generate a cross-validation curve.
#' @param ... Other graphical parameters to plot
#' 
#' @return
#' The "\code{solution-path}" plot produces a coefficient profile plot of the coefficient paths for a fitted \code{\link{intsel}} model. The "\code{cv-curve}" plot is the \code{cvm} (red dot) for each lambda with its standard error (vertical bar). The two vertical dashed lines corresponds to the \code{lambda.min} and \code{lambda.1se}.
#' 
#' @examples
#' n <- 1000
#' p.int <- 5
#' p.noint <- 3
#' intercept <- TRUE
#' p.screen <- 5
#' 
#' p.int.expand <- p.int*(p.int-1)/2
#' p.main <- p.int + p.noint
#' x <- matrix(rnorm(n * p.main), nrow = n, ncol = p.main)
#' 
#' # true model
#' # logit(p) = 0.1 + 0.3 x1 + 0.3 x2 + 0.3 x8 + 0.2 * x1 * x2
#' 
#' beta.true <- rep(0, p.main)
#' beta.true[c(1, 2, p.main)] <- 0.3
#' eta <- x %*% beta.true + 0.2 * x[, 1] * x[, 2]
#' 
#' if (intercept) eta <- eta + 0.1
#' 
#' py <- 1/(1 + exp(-eta))
#' 
#' y <- rbinom(n, 1, py)
#' 
#' nlam <- 30
#' lambdas <- exp(seq(log(0.1), log(0.00005), length.out = nlam))
#' 
#' # All the pairwise two-way interactions for the first p.screen variables 
#' # are included in the model and screened in a data-driven manner.
#' cv <- intsel_cv(x = x,
#'                 y = y,
#'                 p.screen =5,
#'                 intercept = intercept,
#'                 stepsize_init = 1,
#'                 lambda = lambdas,
#'                 nfolds = 5,
#'                 foldid = NULL)
#' plot(cv)
#' plot(cv, type = "solution-path") 
#' @seealso \code{\link{intsel}}, \code{\link{intsel_cv}}.
#' @method plot intsel_cv
#' @rdname plot.intsel_cv
#' @export
#' @importFrom graphics matplot
#' @importFrom graphics abline
#' @importFrom graphics segments
#' @importFrom graphics points
#' @importFrom graphics axis

plot.intsel_cv <- function(x, type = "cv-curve", ...) {
  cvup <- x$cvup
  cvlo <- x$cvlo
  cvm <- x$cvm
  
  lam.seq <- x$lambdas
  lambda.min <- x$lambda.min
  lambda.1se <- x$lambda.1se
  
  if (type == "cv-curve") {
    ylims <- range(c(cvup, cvlo))
    plot(x = log(lam.seq), y = cvm, ylim = ylims, col = 0,
         ylab = "CV Error",
         xlab = expression(paste("Log(", lambda, ")")))
    for (i in 1:length(cvm)) {
      segments(x0 = log(lam.seq)[i], y0 = cvup[i],
               x1 = log(lam.seq)[i], y1 = cvlo[i],
               col = 'grey60')
    }
    points(x = log(lam.seq), y = cvup, pch = 95, cex = 1, col = 'grey60')
    points(x = log(lam.seq), y = cvlo, pch = 95, cex = 1, col = 'grey60')
    points(x = log(lam.seq), y = cvm, pch = 20, cex = 1, col = 'red')
    abline(v = log(lambda.min), lty = 3)
    abline(v = log(lambda.1se), lty = 3)
    axis(side = 3, at = log(lam.seq), labels = x$nzero, tick = FALSE)
  } else if (type == "solution-path") {
    estimates <- x$intsel.fit$estimates
    
    matplot(x = log(lam.seq),
            y = t(estimates),
            xlab = expression(paste("Log(", lambda, ")")),
            ylab = "",
            type = "l")
    abline(h = 0, lty = 2)
    abline(v = log(lambda.min), lty = 3)
    abline(v = log(lambda.1se), lty = 3)
  }
}
