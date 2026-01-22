#--------------------------------------------------
#' @title Plot ANN validation results.
#'
#' @description Plot method for objects of class `validann'. Produces a series
#'    of plots used for validating and assessing ANN models based on results
#'    returned by \code{\link{validann}}.
#'
#' @param x  object of class `validann' as returned
#'    by \code{\link{validann}}. This is a list comprising metrics and
#'    statistics that can be used for validating ANN models.
#' @param obs,sim   vectors comprising observed (\code{obs}) and simulated
#'    (\code{sim}) examples of a single response variable used for computing
#'    \code{x} object.
#' @param gof   logical; should goodness-of-fit plots be produced?
#'    Default = TRUE.
#' @param resid  logical; should residual analysis plots be produced?
#'    Default = TRUE.
#' @param sa   logical; should input sensitivity analysis plots be
#'    produced? Default = TRUE.
#' @param display   character string defining how plots should be
#'    displayed. The default is ``multi'' where multiple plots are displayed
#'    together according to whether they are goodness-of-fit, residual analysis
#'    or sensitivity analysis plots. For ``single'', each plot is displayed on
#'    its own. If the session is interactive, the user will be asked to confirm
#'    a new page whether \code{display} is ``single'' or ``multi''.
#' @param profile   character string defining which structural
#'    validity Profile method outputs should be plotted. The default is ``all''
#'    where outputs corresponding to 5 summary statistics are plotted together
#'    with the median predicted response for each input value.
#'    For ``median'', only the median response is plotted.
#' @param \dots Arguments to be passed to plot (not currently used).
#' @details   This function can be invoked by calling
#'    \code{plot(x, obs, sim)} for an object \code{x} of class
#'    `validann'.
#'
#'    To produce plots for all types of validation metrics and statistics,
#'    \code{gof}, \code{resid} and \code{sa} must be
#'    \code{TRUE} and corresponding results must have been successfully
#'    computed by \code{\link{validann}} and returned in object \code{x}.
#'
#'    If \code{gof} is \code{TRUE}, a scatter plot, Q-Q plot and
#'    time/sample plot of observed (\code{obs}) versus predicted (\code{sim})
#'    data are produced.
#'
#'    If \code{resid} is \code{TRUE} and \code{x$residuals}
#'    is not \code{NULL}, plots of the model residuals are produced including
#'    histogram, Q-Q plot (standardized residuals compared to standard normal),
#'    autocorrelation (acf), partial autocorrelation (pacf), standardized
#'    residual versus predicted output (i.e. \code{sim}) and standardized
#'    residual versus time/order of the data.
#'
#'    If \code{sa} is \code{TRUE} and \code{x$y_hat} is not
#'    \code{NULL}, model response values resulting from the Profile
#'    sensitivity analysis are plotted against percentiles of each
#'    input. If \code{x$rs} is not \code{NULL}, the relative sensitivities of
#'    each input, as computed by the partial derivative (PaD) sensitivity
#'    analysis, are plotted against predicted output.
#'
#'    Setting \code{gof}, \code{resid} and/or \code{sa} to \code{FALSE}
#'    will `turn off' the respective validation plots.
#'
#' @seealso \code{\link{validann}}
#' @examples
#' ## Build ANN model and compute replicative and structural validation results
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#' results <- validann(fit, x = x)
#' obs <- observed(fit)
#' sim <- fitted(fit)
#'
#' ## Plot replicative and structural validation results to the current device
#' ## - a single page for each type of validation
#' plot(results, obs, sim)
#'
#' ## Plot results to the current device - a single page for each plot
#' plot(results, obs, sim, display = "single")
#'
#' ## Plot replicative and structural validation results to single file
#' pdf("RepStructValidationPlots.pdf")
#' plot(results, obs, sim)
#' dev.off()
#'
#' ## Get predictive validation results for above model based on a new sample
#' ## of ar9 data.
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' obs <- y
#' sim <- predict(fit, newdata = x)
#' results <- validann(fit, obs = obs, sim = sim, x = x)
#'
#' ## Plot predictive results only to file
#' pdf("PredValidationPlots.pdf")
#' plot(results, obs, sim, resid = FALSE, sa = FALSE)
#' dev.off()
#'
#' @export
#' @importFrom grDevices colors
#' @importFrom grDevices dev.interactive
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics abline
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics title
#' @importFrom stats acf
#' @importFrom stats pacf
#' @importFrom stats dnorm
#' @importFrom stats qnorm
#' @importFrom stats qqnorm
#' @importFrom stats qqplot

#--------------------------------------------------
plot.validann <- function(x, obs, sim, gof = TRUE, resid = TRUE, sa = TRUE,
                          display = c("multi", "single"),
                          profile = c("all", "median"), ...) {


#  ask <- devAskNewPage(TRUE)
#  on.exit(devAskNewPage(ask))

  if (is.null(obs) & is.null(sim)) {
    stop("'obs' and 'sim' objects required.")
  }
  display <- match.arg(NULL, display)
  profile <- match.arg(NULL, profile)
  if (display == "single" && dev.interactive()) devAskNewPage(ask = TRUE)

# Goodness-of-fit plots
# ----
  if (gof == TRUE) {
    if (is.null(obs)) {
      message1 <- "'obs' data missing :"
      message2 <- "Goodness-of-fit plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else if (is.null(sim)) {
      message1 <- "'sim' data missing :"
      message2 <- "Goodness-of-fit plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      if (display == "multi") {
        m <- rbind(c(1, 2), c(3, 3))
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

      # scatterplot - obs vs sim
      min_plot <- min(obs, sim)
      max_plot <- max(obs, sim)
      plot(x = obs, y = sim, type = "p", pch = 21, col = "black",
           bg = colors()[240], xlim = c(min_plot, max_plot),
           ylim = c(min_plot, max_plot), xlab = "Observed", ylab = "Predicted",
           main = "Scatter Plot")
      abline(a = 0, b = 1, col = "red", lty = "dashed")


      # qq plot of obs v sim
      qqplot(obs, sim, pch = 21, col = "black", bg = colors()[240],
             xlim = c(min_plot, max_plot), ylim = c(min_plot, max_plot),
             xlab = "Observed", ylab = "Predicted",
             main = "Q-Q Plot")
      abline(a = 0, b = 1, col = "red", lty = "dashed")

      nsamps <- length(obs)
      plot(x = 1:nsamps, y = obs, type = "p", pch = 23, col = "black",
           bg = "black", ylim = c(min_plot, max_plot),
           xlab = "Sample", ylab = "Value")
      points(x = 1:nsamps, y = sim, pch = 23, col = "black",
             bg = colors()[240], cex = 0.8)
      title(main = "Observed Vs Predicted", line = 2)
    # add legend
      par(oma = c(0, 0, 1, 0), mar = c(0, 0, 1, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend(x = "top", legend = c("Observed", "Predicted"),
             pch = c(23, 23), col = c("black", "black"),
             pt.bg = c("black", colors()[240]), pt.cex = c(1, 0.8),
             horiz = TRUE, bty = "n", inset = c(0, 0), xpd = TRUE)
      if (display == "multi") {
        if (dev.interactive()) devAskNewPage(ask = TRUE)
        title(main = "Goodness-of-fit", outer = TRUE)
      }
    }
  }


# Residual analysis plots
# ----
  if(resid == TRUE) {
    if (is.null(sim)) {
      message1 <- "'sim' data missing :"
      message2 <- "Residual analysis plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      if(display == "multi") {
        m <- rbind(c(1, 2), c(3, 4), c(5, 6))
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

    # residuals histogram
      tmp_hist <- hist(x$residuals, plot = FALSE)
      tmp_norm <- dnorm(x$residuals, mean = 0,
                        sd = x$resid_stats$sd)
      ymax <- max(tmp_hist$density, tmp_norm) * 1.2
      plot(x = tmp_hist$mids, y = tmp_hist$density, type = "h", lwd = 30,
           lend = 2, col = colors()[240], ylim = c(0, ymax), yaxs = "i",
           xlab = "Residual", ylab = "Density", main = "Residuals Histogram")
      lines(x = sort(x$residuals), y = tmp_norm[order(x$residuals)],
            col = "red", lty = "dashed")
      if (display == "multi") devAskNewPage(ask = FALSE)

    # qq plot of residual vs normal distribution
      sd_err <- (x$residuals - x$resid_stats$mean) /
                 x$resid_stats$sd
      qqnorm(sd_err, pch = 21, col = "black", bg = colors()[240],
             xlab = "Standard Normal Quantiles", ylab = "Standardized Residual",
             main = "Residual Q-Q Plot")
      abline(a = 0, b = 1, col = "red", lty = "dashed")

    # residual autocorrelation plots
      acf_tmp <- acf(x$residuals, plot = FALSE)
      pacf_tmp <- pacf(x$residuals, plot = FALSE)

      clim <- qnorm((1 + 0.95) / 2) / sqrt(acf_tmp$n.used)
      ylim <- range(c(-clim, clim, acf_tmp$acf[, 1, 1]))
      plot(acf_tmp$lag[, 1, 1], acf_tmp$acf[, 1, 1], type = "h", ylim = ylim,
           xlab = "Lag", ylab = "ACF", main = "Residual Autocorrelation")
      abline(h = 0)
      abline(h = c(clim, -clim), col = "blue", lty = 2)

      ylim <- range(c(-clim, clim, pacf_tmp$acf[, 1, 1]))
      plot(pacf_tmp$lag[, 1, 1], pacf_tmp$acf[, 1, 1], type = "h", ylim = ylim,
           xlab = "Lag", ylab = "Partial ACF",
           main = "Residual Partial-Autocorrelation")
      abline(h = 0)
      abline(h = c(clim, -clim), col = "blue", lty = 2)

    # Standardised residuals vs simulated
      plot(x = sim, y = sd_err, type = "p", pch = 21, col = "black",
           cex = 0.8, bg = colors()[240], xlab = "Predicted Value",
           ylab = "Standardized Residual",
           main = "Residuals Vs Simulated")
      abline(h = 0, lty = "dashed", col = "red")
      abline(h = -1.96, lty = "dashed", col = "blue")
      abline(h = 1.96, lty = "dashed", col = "blue")

    # Standardised residuals vs 'time'
      plot(x = 1:length(sd_err), y = sd_err, type = "p", pch = 21,
           col = "black", cex = 0.8, bg = colors()[240],
           xlab = "Order", ylab = "Standardized Residual",
           main = "Residuals Vs Order/Time")
      abline(h = 0, lty = "dashed", col = "red")
      abline(h = -1.96, lty = "dashed", col = "blue")
      abline(h = 1.96, lty = "dashed", col = "blue")

      if (display == "multi") {
        if (dev.interactive()) devAskNewPage(ask = TRUE)
        title(main = "Residual analysis", outer = TRUE)
      }
    }
  }


# Sensitivity analysis plots
# ----
  if(sa == TRUE) {
    if (is.null(x$y_hat) && is.null(x$rs)) {
      message1 <-
        "Sensitivity analysis results missing : "
      message2 <- "Sensitivity analysis plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      ninputs <- ncol(x$y_hat) / 6
    }
    if (!is.null(x$y_hat)) {
      if (display == "multi") {
        rem <- ninputs %% 2
        rep <- ninputs %/% 2
        if (rem > 0) rep <- rep + 1
        rep <- min(rep, 3)
        m <- matrix(1:(rep * 2), ncol = 2, byrow = TRUE)
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
        rep <- 1
      }
      par(oma = c(0, 0, 1, 3), mar = c(4, 4, 3, 0.3))
      cols <- c("blue", "red", "gold", "magenta", "turquoise", "black")
      for (i in 1:ninputs) {
        y_hat <- x$y_hat[, (i - 1) * 6 + 1:6]
        miny <- min(y_hat)
        maxy <- max(y_hat) + 0.3 * (max(y_hat) - miny)
        p_name <- colnames(y_hat)[1]
        p_name <- substr(p_name, 1, nchar(p_name) - 2)
        plot(x = seq(0, 100, by = 1), y = y_hat[, 6], type = "l",
             col = "black",
             ylim = c(miny, maxy), ylab = "Predicted Response",
             xlab = paste("Percentile of Input:", p_name),
             main = "")
        title(main = p_name, line = 1)
        if(profile == "all") {
          for(j in 1:6) {
            lines(x = seq(0, 100, by = 1), y = y_hat[, j], col = cols[j])
          }
           legend("top",
                  legend = c("Min.", "25%", "50%",
                             "75%", "Max.", "Median"),
                  cex = 0.6 + rep * 0.1, pch = 20, col = cols,
                  horiz = TRUE, bty = "n", xpd = NA)
        }
        if (dev.interactive() && (i %% (rep * 2) == 0)) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && i == ninputs) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && display == "single") {
          devAskNewPage(ask = TRUE)
        } else {
          devAskNewPage(ask = FALSE)
        }
        title(main = "Sensitivity analysis - Profile", outer = TRUE)
      }
    }
    if (!is.null(x$rs)) {
      if (display == "multi") {
        rem <- ninputs %% 2
        rep <- ninputs %/% 2
        if (rem > 0) rep <- rep + 1
        rep <- min(rep, 3)
        m <- matrix(1:(rep * 2), ncol = 2, byrow = TRUE)
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }

      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))
      yrange <- c(min(x$rs), max(x$rs))
      for (i in 1:ninputs) {
        plot(x = obs, y = x$rs[, i], type = "p", ylim = yrange,
             xlab = "Observed Response Value", ylab = "Relative Sensitivity",
             main = colnames(x$rs)[i])
        if (dev.interactive() && (i %% (rep * 2) == 0)) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && i == ninputs) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && display == "single") {
          devAskNewPage(ask = TRUE)
        } else {
          devAskNewPage(ask = FALSE)
        }
        title(main = "Sensitivity analysis - PaD - Relative", outer = TRUE)
      }

      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))
      yrange <- c(min(x$as), max(x$as))
      for (i in 1:ninputs) {
        plot(x = obs, y = x$as[, i], type = "p", ylim = yrange,
             xlab = "Observed Response Value", ylab = "Absolute Sensitivity",
             main = colnames(x$as)[i])
        if (dev.interactive() && (i %% (rep * 2) == 0)) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && i == ninputs) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && display == "single") {
          devAskNewPage(ask = TRUE)
        } else {
          devAskNewPage(ask = FALSE)
        }
        title(main = "Sensitivity analysis - PaD - Absolute", outer = TRUE)
      }
    }

  }
  devAskNewPage(ask = FALSE)
}
#-------------------------------------------------------------------------------
