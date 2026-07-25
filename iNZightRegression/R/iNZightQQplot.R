#' Produces a sample of QQ-plots based on the fitted values, overlaid by
#' a QQ-plot of the original data.
#'
#' Multiple bootstrap models are generated from the fitted values of
#' the model, each with different random normal errors with standard
#' error equal to the estimated residual standard error from the
#' original model. These are plotted, and then overlaid by the QQ plot
#' from the original data.
#' \cr \cr
#' This plot can be used to assess the assumption of normality in the
#' residuals for a linear regression model.
#' @title iNZight QQ Plot
#'
#' @param x an \code{lm} or \code{svyglm} object (with \code{family = "Gaussian"}.
#'
#' @param n the number of sampled QQ plots to produce beneath the QQ plot of
#' \code{x}.
#'
#' @param env environment for finding data to bootstrap
#' @return No return value, called to produce plot.
#'
#' @author David Banks, Tom Elliott
#'
#' @seealso \code{\link{histogramArray}}
#'
#' @examples
#' fit <- lm(Volume ~ Height + Girth, data = trees)
#' iNZightQQplot(fit)
#'
#' @export
iNZightQQplot <- function(x, n = 5, env = parent.frame()) {

  # ------------------------------------------------------------------ #
  # Generates n (= 5) parameteric bootstrap samples with normal errors
  # from the fitted values. These are plotted and overlaid by the qq
  # values from the original data set.
  # ------------------------------------------------------------------ #

  # Assumes lm model (gaussian errors only)
    if (isGlm(x))
        if (x$family$family != 'gaussian')
            stop('histrogramArray only works with linear models.')

  # Calculate the QQ values for the fitted model
    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warnings("Not plotting observations with leverage one:\n ",
                     paste(which(isInf), collapse = ", "), call. = FALSE)
            x[isInf] <- NaN
        }
        x
    }
    qqVals <- function(x) {
        hii <- lm.influence(x, do.coef = FALSE)$hat
        r <- residuals(x)
        s <- sqrt(deviance(x) / df.residual(x))
        rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
        qq <- normCheck(rs, plot = FALSE)
        qq
    }
    qq <- qqVals(x)

    ## Generate n bootstrap samples with normal errors
    n.obs <- nrow(x$model)
    xvars <- as.character(x$call$formula)[3]
    fmla <- as.formula(paste("response", xvars, sep = " ~ "))

    qqList <- vector("list", length = n)
    sdhat <- ifelse(isGlm(x),
                 sqrt(1 / (n.obs - ncol(x$model)) *
                      sum((x$residuals)^2)),
                 summary(x)$sigma)

    ## drop rows missing the final model (NAs)
    .data <- eval(x$call$data, envir = env)[as.numeric(names(predict(x))),]
    qqList <- replicate(n, {
        .data$response <- x$fitted.values + rnorm(n.obs, sd = sdhat)
        bslm <- update(x, formula = fmla, data = .data)
        qqVals(bslm)
    }, FALSE)

    ## Set up a plotting window
    xlim <- range(qq$x, na.rm = TRUE)
    ylim <- range(sapply(qqList, function(q) q$y), na.rm = TRUE)

    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    axis(1)
    axis(2)
    box()
    title(xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
          main = "")
    abline(c(0, 1), lty = 2)

    ## Plot the n random qq values
    for (i in 1:n)
        points(qqList[[i]], pch = 4, cex = 0.8,
               col = hcl(i/n * 360, 80, 50))

    ## Overlay the true values
    points(qq, pch = 1, cex = 0.8, lwd = 2,
           col = hcl(240, 80, 0))

    ## Add a legend
    legend("topleft", c("Original Data", "Sampled Normal Errors"),
           pch = c(1, 4), pt.cex = 0.8, col = "black",
           bty = "n")

    invisible(NULL)
}
