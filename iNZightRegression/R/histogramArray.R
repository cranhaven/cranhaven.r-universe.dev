#' Produces an array of histograms to compare against the histogram of
#' residuals for a fitted linear model.
#'
#' The histogram of the model \code{x} appears in the top-left
#' position. For each of the other histograms, the fitted values of
#' \code{x} are taken and normal random errors are added to these. The
#' normal residual standard errors have standard error equal to the
#' estimated residual standard error of \code{x}. A model is then fitted
#' to this altered data and a histogram is produced.
#'
#' @title Histogram Array
#'
#' @param x an \code{lm} or \code{svyglm} object.
#'
#' @param n the number of additional histograms to plot alongside the original.
#'
#' @param env environment for finding data to bootstrap
#' @return No return value, called to generate plot.
#'
#' @author David Banks, Tom Elliott
#'
#' @seealso \code{\link{iNZightQQplot}}
#'
#' @export
#' @examples
#' histogramArray(lm(Sepal.Length ~ Sepal.Width + Species, data = iris))
histogramArray <- function(x, n = 7, env = parent.frame()) {
    if (isGlm(x))
        if (x$family$family != 'gaussian')
            stop('histrogramArray only works with linear models.')

  # Assumes lm model (gaussian errors only)
    n <- min(n, 11)
    nRows <- 2
    nCols <- ceiling(round(n + 1) / nRows)
   # opar <- par(mfrow = c(2, nCols))
   # on.exit(par(opar))

  # Create a layout:
    grid.newpage()
    wl <- unit(4, "lines")  # the width of outside columns (axes etc)
    wp <- rep(unit(1, "null"), nCols)  # the width of plots
    wr <- unit(3, "lines")

    ht <- unit(5, "lines")  # height of top row
    hp <- rep(unit(1, "null"), nRows)  # height of plots
    hb <- unit(4, "lines")  # height of bottom row

    layout <- grid.layout(nrow = nRows + 2, ncol = nCols + 2,
                          widths = unit.c(wl, wp, wr),
                          heights = unit.c(ht, hp, hb))

    r <- residuals(x)
    h <- hist(r, plot = FALSE)
    xlab <- "Residuals"
    mx <- mean(r)
    sx <- sd(r)
    rx <- range(r)
    xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
    xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])

    ## Generate n bootstrap samples with normal errors
    n.obs <- nrow(x$model)
    xvars <- as.character(x$call$formula)[3]
    fmla <- as.formula(paste("response", xvars, sep = " ~ "))

    resList <- list()
    breaksList <- list()
    sdhat <- ifelse(isGlm(x),
                 sqrt(1 / (n.obs - ncol(x$model)) *
                      sum((x$residuals)^2)),
                 summary(x)$sigma)

    ## drop rows missing the final model (NAs)
    .data <- eval(x$call$data, envir = env)[as.numeric(names(predict(x))),]
    for (i in 1:n) {
        .data$response <- x$fitted.values + rnorm(n.obs, sd = sdhat)
        bslm <- update(x, formula = fmla, data = .data)
        r <- residuals(bslm)
        resList[[i]] <- r
        h <- hist(r, plot = FALSE)
        breaksList[[i]] <- h$breaks
        mx <- mean(r, na.rm = TRUE)
        sx <- sd(r, na.rm = TRUE)
        rx <- range(r, na.rm = TRUE)
        xmin <- min(xmin, min(rx[1], mx - 3.5 * sx, h$breaks[1]))
        xmax <- max(xmax, max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)]))
    }

    binWidth <- diff(breaksList[[1]][1:2])
    breaks <- seq(binWidth * floor(xmin / binWidth),
                  binWidth * ceiling(xmax / binWidth),
                  by = binWidth)

  # Need to define histograms again without drawing, to find max density
    ymax <- 0
    for (i in 1:n) {
        r <- resList[[i]]
        h <- hist(r, plot = FALSE, breaks = breaks)
        ymax <- max(ymax, max(h$density, dnorm(mean(r), mean(r), sd(r))) * 1.05)
    }

    oh <- hist(residuals(x), breaks = breaks, plot = FALSE)
    r <- residuals(x)
    maxOh <- max(oh$density, dnorm(mean(r), mean(r), sd(r)))
    ymax <- max(ymax, maxOh)

    xlim <- c(xmin, xmax)
    ylim <- c(0, ymax) / 0.9

  # Start the layout:
    pushViewport(viewport(layout = layout))

  # Original data histogram of residuals
    h1 <- hist(residuals(x), breaks = breaks, plot = FALSE)
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2,
                          xscale = xlim, yscale = ylim))

    grid.rect()
    xx <- h1$breaks
    yy <- h1$density
    for (b in 1:length(yy)) {
        grid.rect(xx[b], 0,
                  width = xx[b + 1] - xx[b],
                  height = yy[b],
                  default.units = "native",
                  just = c("left", "bottom"),
                  gp = gpar(fill = grDevices::hcl(240, 80, 80)))
    }
    grid.yaxis(gp = gpar(cex = 0.8))

   # mtext("Original data", 3, font = 2, col = "navy", line = 1)
   # box()
    x1 <- seq(xmin, xmax, length = 100)
    y1 <- dnorm(x1, mx, sx)
    grid.lines(x1, y1, default.units = "native",
               gp = gpar(lwd = 1.5, lty = 3))

    grid.text("Original Data", x = 0.5, y = 0.95,
              gp = gpar(cex = 0.8))

    upViewport()  # back to layout

  # Normal error sample histograms ----------------- ##

    col <- row <- 1
    for (i in 1:n) {
        h <- hist(resList[[i]], breaks = breaks, plot = FALSE)

        if (col == nCols) {
            col <- 1
            row <- row + 1
        } else {
            col <- col + 1
        }

        pushViewport(viewport(layout.pos.row = row + 1,
                              layout.pos.col = col + 1,
                          xscale = xlim, yscale = ylim))

        grid.rect()
        xx <- h$breaks
        yy <- h$density
        for (b in 1:length(yy)) {
            grid.rect(xx[b], 0,
                      width = xx[b + 1] - xx[b],
                      height = yy[b],
                      default.units = "native",
                      just = c("left", "bottom"),
                      gp = gpar(fill = grDevices::hcl(0, 80, 80)))
        }

        grid.lines(x1, y1, default.units = "native",
                   gp = gpar(lwd = 1.5, lty = 3))

        if (col == 1)
            grid.yaxis(gp = gpar(cex = 0.8))
        if (col == nCols)
            grid.yaxis(main = FALSE, gp = gpar(cex = 0.8))

        if (row == nRows & (col %% 2 == 1))
            grid.xaxis(gp = gpar(cex = 0.8))
        if (row == 1 & (col %% 2 == 0))
            grid.xaxis(main = FALSE, gp = gpar(cex = 0.8))

        grid.text(paste("Sample", i), x = 0.5, y = 0.95,
                  gp = gpar(cex = 0.8))

        upViewport()
    }


  # Add a title
    pushViewport(viewport(layout.pos.row = 1, gp = gpar(cex = 1.2)))
    grid.text("Histograms of Residuals",
              x = 0.5, y = unit(3.1, "lines"))
    grid.text("Actual data cf. Generated Normal-Error Data",
              x = 0.5, y = unit(2.5, "lines"), gp = gpar(cex = 0.8))
    upViewport()

  # Add x-label
    pushViewport(viewport(layout.pos.row = nRows + 2, gp = gpar(cex = 0.9)))
    grid.text("Residuals", x = 0.5, y = unit(1.5, "lines"))
    upViewport()

  # Add y-label
    pushViewport(viewport(layout.pos.col = 1, gp = gpar(cex = 0.9)))
    grid.text("Density", x = unit(1, "lines"), y = 0.5, rot = 90)
    upViewport()


    upViewport() # out of layout
}
