#' @import grid
plotlm6grid <- function(x, which = 1:6,
                        panel = if (add.smooth) panel.smooth
                        else points, sub.caption = NULL,
                        main = "",
                        ask = prod(par("mfcol")) < length(which) && dev.interactive(),
                        id.n = 3, labels.id = names(residuals(x)),
                        cex.id = 1, qqline = TRUE, cook.levels = c(0.5, 1),
                        add.smooth = getOption("add.smooth"), label.pos = c(2, 4),
                        cex.caption = 1,
                        showBootstraps = nrow(x$model) >= 30 && nrow(x$model) < 4000, ...) {

    # DEPRECATED in favour of `inzplot.lm()`, which uses ggplot2

  # This is a grid-graphics implementation of the summary plots, however is only
  # invoked when the iNZightPlots package is present. This means users can still
  # use iNZightRegression as a completely standalone package, but the version used
  # by iNZight will use the different plots.

    smColour = "orangered"      # colour of data loess line
    bsmColour = "lightgreen"    # colour of bootstrap loess lines

    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warning("Not plotting observations with leverage one:\n  ",
                    paste(which(isInf), collapse = ", "), call. = FALSE)
            x[isInf] <- NaN
        }
        x
    }
    if (!inherits(x, "lm"))
        stop("use only with \"lm\" objects")

    if (!is.numeric(which) || any(which < 1) || any(which > 7))
        stop("'which' must be in 1:7")

    ## Are we only showing the summary plot?
    if (7 %in% which) {
        onlyShowAll <- TRUE
        which <- 1:6
    } else {
        onlyShowAll <- FALSE
    }

    show <- rep(FALSE, 6)
    show[which] <- TRUE
    r <- residuals(x)
    yh <- predict(x)
    w <- weights(x)
    if (!is.null(w)) {
        wind <- w != 0
        r <- r[wind]
        yh <- yh[wind]
        w <- w[wind]
        labels.id <- labels.id[wind]
    }
    n <- length(r)
    if (any(show[2:6])) {
        s <- if (inherits(x, "rlm"))
            x$s
        else if (isGlm(x))
            sqrt(summary(x)$dispersion)
        else sqrt(deviance(x)/df.residual(x))
        hii <- lm.influence(x, do.coef = FALSE)$hat
        if (any(show[3L:6L])) {
            cook <- if (isGlm(x))
                cooks.distance(x)
            else cooks.distance(x, sd = s, res = r)
        }
    }
    if (any(show[c(2,5)])) {
        ylab23 <- if (isGlm(x))
            "Std. deviance resid."
        else "Standardized residuals"
        r.w <- if (is.null(w)) r else sqrt(w) * r
        rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    }
    if (show[3]) {
        r.hat <- range(hii, na.rm = TRUE)
        isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 *
            mean(hii, na.rm = TRUE)
    }
    if (any(show[1:2]))
        l.fit <- ifelse(isGlm(x), "Predicted values", "Fitted values")
    if (is.null(id.n)) {
        id.n <- 0
    } else {
        id.n <- as.integer(id.n)
        if (id.n < 0 || id.n > n)
            stop(gettextf("'id.n' must be in {1,..,%d}", n),
                 domain = NA)
    }
    if (id.n > 0) {
        if (is.null(labels.id))
            labels.id <- paste(1:n)
        iid <- 1:id.n
        show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
        if (any(show[c(2,5)]))
            show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
        text.id <- function(x, y, ind, adj.x = TRUE) {
            if (adj.x)
                x <- x + ifelse(x > mean(x), 1, -1) *
                    (convertWidth(unit(0.4, "char"), "native", TRUE) +
                     convertWidth(grobWidth(textGrob(as.character(ind),
                                                       gp =
                                                     gpar(cex = cex.id / sqrt(nrow * ncol)))),
                                  "native", TRUE) * 0.5)

            grid.text(labels.id[ind], x, y, default.units = "native",
                      just = "center", gp = gpar(cex = cex.id / sqrt(nrow * ncol)))
        }
    }

    caption = list("Residuals vs Fitted", "Scale-Location",
                   "Residuals vs Leverage","Cook's distance",
                   "Normal Q-Q", "Histogram")
    getCaption <- function(k) {
        if (length(caption) < k)
            NA_character_
        else
            as.graphicsAnnot(caption[[k]])
    }
    if (is.null(sub.caption)) {
        cal <- x$call
        if (!is.na(m.f <- match("formula", names(cal)))) {
            cal <- cal[c(1, m.f)]
            names(cal)[2] <- ""
        }
        cc <- deparse(cal, 80)
        nc <- nchar(cc[1], "c")
        abbr <- length(cc) > 1 || nc > 75
        sub.caption <- if (abbr)
            paste(substr(cc[1], 1, min(75, nc)), "...")
        else cc[1]
    }

    one.fig <- length(which) == 1 || onlyShowAll
    if (ask) {
        oask <- devAskNewPage(! one.fig)
        on.exit(devAskNewPage(oask))
    } else {
        oask <- devAskNewPage(FALSE)
        on.exit(devAskNewPage(oask))
    }

    if (showBootstraps) {
        bsModels = bootstrapModels(x)
        nBootstraps = length(bsModels)
        ## New bootstrapped values (bs suffix stands for bootstrap)
        rbs = rsbs = rbs.w = wbs = wbsind = yhbs = sbs = hiibs =
            sbs = vector("list", nBootstraps)
        for (i in 1:nBootstraps) {
            rbs[[i]] = residuals(bsModels[[i]])
            yhbs[[i]] = predict(bsModels[[i]])
            wbs[i] = list(weights(bsModels[[i]]))  # list() prevents
                                                   # deletion if NULL
            if (!is.null(wbs[[i]])) {
                wbsind[[i]] <- wbs[[i]] != 0
                rbs[[i]] <- rbs[[i]][wind]
                yhbs[[i]] <- yhbs[[i]][wind]
                wbs[[i]] <- wbs[[i]][wind]
            }
            sbs[[i]] = if (inherits(x, "rlm"))
                bsModels[[i]]$s
            else if (isGlm(x))
                sqrt(summary(bsModels[[i]])$dispersion)
            else sqrt(deviance(bsModels[[i]])/df.residual(bsModels[[i]]))
            if (any(show[2:6]))
                hiibs[[i]] <- lm.influence(bsModels[[i]],
                                           do.coef = FALSE)$hat
            if (any(show[2:3])) {
                rbs.w[[i]] <-
                    if (is.null(wbs[[i]])) {
                        rbs[[i]]
                    } else {
                        sqrt(wbs[[i]]) * rbs[[i]]
                    }
                rsbs[[i]] <- dropInf(rbs.w[[i]] /
                                     (sbs[[i]] * sqrt(1 - hiibs[[i]])),
                                     hiibs[[i]])
            }
        }
    }

  # If we want to show all of the plots, assume "all" is the
  # seventh plot
    showAllPlots = all(show)

  # Start setting up the graphics window:

    N <- length(which)  # if which = 7, then which -> 1:6 -> length = 6
  # layout dimensions
    if (!showAllPlots) {
        nrow <- 1
        ncol <- 1
    } else {
        ncol <- ceiling(sqrt(N))
        nrow <- ceiling(N / ncol)
    }


  # Draw plot title:
    newPlot <- function(id = NULL) {
        grid.newpage()
        pushViewport(viewport(layout =
                              grid.layout(2, 1,
                                          heights =
                                          unit.c(unit(1, "null"),
                                                 unit(1.5, "lines"))),
                              name = "titleVP"))
        pushViewport(viewport(layout.pos.row = 2))
        grid.text(sub.caption, gp = gpar(cex = 0.8))

        seekViewport("titleVP")
        pushViewport(viewport(layout.pos.row = 1))
        main.layout <- if (showAllPlots) grid.layout(nrow, ncol) else grid.layout(1, 1)
        pushViewport(viewport(layout = main.layout, name = "topVP"))

        if (!is.null(id)) {
            pushViewport(viewport(layout = fig.layout,
                                  name = paste("VP", id, sep = "-")))
        }
    }

  # because iNZscatterplot needs a 2-row layout:
    scatter.layout <- grid.layout(2, 1, heights = unit(c(0, 1), "null"))

  # layout for each individual plots (with labels etc)
    fig.layout <- grid.layout(3, 3,
                              heights =
                              unit.c(unit(2, "lines"), unit(1, "null"), unit(3, "lines")),
                              widths =
                              unit.c(unit(3, "lines"), unit(1, "null"), unit(1, "lines")))

    plotID <- 1  # increments with each plot
    nextVP <- function(id, nrow, ncol) {
        if (nrow == 1 & ncol == 1) {
            wcol <- wrow <- 1
        } else {
            wcol <- (id - 1) %% ncol + 1
            wrow <- (id - 1) %/% ncol + 1
        }

        seekViewport("topVP")
        pushViewport(viewport(layout.pos.row = wrow, layout.pos.col = wcol))
        pushViewport(viewport(layout = fig.layout,
                              name = paste("VP", id, sep = "-")))
    }
    getLim <- function(x) {
        r <- range(x)
        mult <- c(-1, 1)
        r + mult * 0.04 * diff(r)
    }
    drawLabs <- function(xlab = "", ylab = "", main = "") {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
        grid.text(main)
        upViewport()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
        grid.text(ylab, x = unit(1, "lines"), rot = 90, gp = gpar(cex = 0.7))
        upViewport()

        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
        grid.text(xlab, y = unit(1, "lines"), gp = gpar(cex = 0.7))
        upViewport()
    }

    opts <- iNZightPlots::inzpar()
    opts$cex.pt <- 0.4
    opts$cex.axis <- 0.7
    opts$col.pt <- "black"
    opts$lwd.pt <- 1
    if (is.null(opts$largesample))
        opts$largesample <- length(r) > opts$large.sample.size
    opts$scatter.grid.bins <- opts$scatter.grid.bins / sqrt(N)

    if (showAllPlots)
        newPlot()

  # ----------------------------------------------------------------------------------- #
  #                                                             RESIDUALS VERSUS FITTED
    if (1 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
        ylim <- getLim(r)
        if (id.n > 0)
            ylim <- extendrange(r = ylim, f = 0.08)
        # iNZightPlots:::iNZscatterplot(yh, r, layout = scatter.layout,
        #                               xlim = getLim(yh), ylim = ylim,
        #                               axis = c(2, 2, 0, 0),
        #                               opts = opts)
      # Add necessary stuff:
        seekViewport("SCATTERVP")

        if (showBootstraps) {
            ## Draw bootstrap sample loess lines
            for (i in 1:nBootstraps) {
                if (opts$largesample) {
                    iNZightPlots:::addQuantileSmoother(yhbs[[i]], rbs[[i]],
                                                       0.5, bsmColour, 1, 2)
                } else {
                    bsm = loess(rbs[[i]] ~ yhbs[[i]])
                    bsmOrd = order(bsm$x)
                    grid.lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], default.units = "native",
                               gp = gpar(col = bsmColour))
                }
            }
        }
        ## Draw loess line for original data set
        if (opts$largesample) {
            iNZightPlots:::addQuantileSmoother(yh, r, 0.5, smColour, 1, 2)
        } else {
            sm <- loess(r ~ yh)
            smOrd <- order(sm$x)
            grid.lines(sm$x[smOrd], sm$fitted[smOrd], default.units = "native",
                       gp = gpar(col = smColour, lwd = 2))
        }

        grid.lines(c(0, 1), y = unit(1, "native"),
                   gp = gpar(col = "gray", lty = 2))

        ## Label extreme points
        if (id.n > 0)
            text.id(yh[show.r], r[show.r], show.r, adj.x = TRUE)

        grid.rect(gp = gpar(fill = "transparent"))
        popViewport()
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = l.fit, ylab = "Residuals", main = getCaption(1))
        dev.flush()

        plotID <- plotID + 1
    }

  # ----------------------------------------------------------------------------------- #
  #                                                                      SCALE LOCATION
    if (2 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))

        sqrtabsr <- sqrt(abs(rs))
        ylim <- getLim(c(0, max(sqrtabsr, na.rm = TRUE)))
        yl <- as.expression(substitute(sqrt(abs(YL)),
            list(YL = as.name(ylab23))))
        yhn0 <-
            if (is.null(w))
                yh
            else
                yh[w != 0]

        # iNZightPlots:::iNZscatterplot(yhn0, sqrtabsr, layout = scatter.layout,
        #                               xlim = getLim(yhn0), ylim = ylim,
        #                               axis = c(2, 2, 0, 0),
        #                               opts = opts)
      # Add necessary stuff:
        seekViewport("SCATTERVP")

        if (showBootstraps) {
            ## Draw bootstrap sample loess lines
            for (i in 1:nBootstraps) {
                if (opts$largesample) {
                    iNZightPlots:::addQuantileSmoother(yhbs[[i]], sqrt(abs(rsbs[[i]])),
                                                       0.5, bsmColour, 1, 2)
                } else {
                    bsm = loess(sqrt(abs(rsbs[[i]])) ~ yhbs[[i]])
                    bsmOrd = order(bsm$x)
                    grid.lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], default.units = "native",
                               gp = gpar(col = bsmColour))
                }
            }
        }
        ## Draw loess line for original data set
        if (opts$largesample) {
            iNZightPlots:::addQuantileSmoother(yhn0, sqrtabsr, 0.5, smColour, 1, 2)
        } else {
            sm <- loess(sqrtabsr ~ yhn0)
            smOrd <- order(sm$x)
            grid.lines(sm$x[smOrd], sm$fitted[smOrd], default.units = "native",
                       gp = gpar(col = smColour, lwd = 2))
        }

        ## Label extreme points
        if (id.n > 0)
            text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)

        grid.rect(gp = gpar(fill = "transparent"))
        popViewport()
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = l.fit, ylab = yl, main = getCaption(2))
        dev.flush()

        plotID <- plotID + 1
    }

  # ----------------------------------------------------------------------------------- #
  #                                                           RESIDUALS VERSUS LEVERAGE
    if (3 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))

        ylab5 <-
            if (isGlm(x))
                "Std. Pearson resid."
            else "Standardized residuals"
        r.w <- residuals(x, "pearson")
        if (!is.null(w))
            r.w <- r.w[wind]
        rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
        ylim <- getLim(rsp)

        if (id.n > 0) {
            ylim <- extendrange(r = ylim, f = 0.08)
            show.rsp <- order(-cook)[iid]
        }
        do.plot <- TRUE

        xx <- hii
        xx[xx >= 1] <- NA
        xlim <- getLim(c(0, max(xx, na.rm = TRUE)))

        # iNZightPlots:::iNZscatterplot(xx, rsp, layout = scatter.layout,
        #                               xlim = xlim, ylim = ylim,
        #                               axis = c(2, 2, 0, 0),
        #                               opts = opts)
      # Add necessary stuff:
        seekViewport("SCATTERVP")

        if (showBootstraps) {
            ## Draw bootstrap sample loess lines
            rspbs <- vector("list", nBootstraps)
            for (i in 1:nBootstraps) {
                pearsonResid <- residuals(bsModels[[i]], "pearson")
                rspbs[[i]] <- dropInf(pearsonResid / (sbs[[i]] * sqrt(1 - hiibs[[i]])),
                                      hiibs[[i]])

                xxbs <- hiibs[[i]]
                xxbs[xxbs >= 1] <- NA

                if (opts$largesample) {
                    iNZightPlots:::addQuantileSmoother(xxbs, rspbs[[i]],
                                                       0.5, bsmColour, 1, 2)
                } else {
                    bsm = loess(rspbs[[i]] ~ xxbs)
                    bsmOrd = order(bsm$x)
                    grid.lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], default.units = "native",
                               gp = gpar(col = bsmColour))
                }
            }
        }
        ## Draw loess line for original data set
        if (opts$largesample) {
            iNZightPlots:::addQuantileSmoother(xx, rsp, 0.5, smColour, 1, 2)
        } else {
            sm <- loess(rsp ~ xx)
            smOrd <- order(sm$x)
            grid.lines(sm$x[smOrd], sm$fitted[smOrd], default.units = "native",
                       gp = gpar(col = smColour, lwd = 2))
        }

        ## Something about cooks levels...
        if (length(cook.levels)) {
            p <- length(coef(x))
            usr <- xlim[2]
            hh <- seq.int(min(r.hat[1L], r.hat[2L] / 100),
                          usr, length.out = 101)
            for (crit in cook.levels) {
                cl.h <- sqrt(crit * p * (1 - hh) / hh)
                grid.lines(hh, cl.h, default.units = "native",
                           gp = gpar(lty = 2, col = 2))
                grid.lines(hh, -cl.h, default.units = "native",
                           gp = gpar(lty = 2, col = 2))
            }
#            legend("bottomleft", legend = "Cook's distance",
#                   lty = 2, col = 2, bty = "n")

            xmax <- min(0.99, usr)
            ymult <- sqrt(p * (1 - xmax) / xmax)

            aty <- c(-sqrt(rev(cook.levels)) * ymult,
                     sqrt(cook.levels) * ymult)
            waxs <- aty > ylim[1] & aty < ylim[2]
            aty <- aty[waxs]
            if (length(aty)) {
                labs <- paste(c(rev(cook.levels), cook.levels))
                labs <- labs[waxs]

                cvp <- current.viewport()
                pushViewport(viewport(clip = "off",
                                      xscale = cvp$xscale, yscale = cvp$yscale))
                grid.yaxis(at = aty, main = FALSE, label = labs,
                           gp = gpar(lwd = 0, cex = cex.id / 2, col = "red"))
            }
        }

        ## Label extreme points
        if (do.plot) {
            if (id.n > 0)
                text.id(xx[show.rsp], rsp[show.rsp], show.rsp)
        }
        upViewport()
        grid.rect(gp = gpar(fill = "transparent"))
        popViewport()
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = "Leverage", ylab = "Standardized", main = getCaption(3))
        dev.flush()

        plotID <- plotID + 1
    }

  # ----------------------------------------------------------------------------------- #
  #                                                                      COOKS DISTANCE
    if (4 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))

        cdx <- cooks.distance(x)
        show.mx <- order(-cdx)[1:3]

        line.ys <- rep(cdx, each = 2)
        line.ys[1:length(line.ys) %% 2 == 1] <- 0
        line.id <- rep(1:length(cdx), each = 2)

        pushViewport(viewport(xscale = getLim(c(0, length(cdx))),
                              yscale = getLim(c(0, max(cdx)))))
        grid.polyline(x = line.id, y = line.ys, id = line.id, default.units = "native")
        grid.xaxis(gp = gpar(cex = opts$cex.axis))
        grid.yaxis(gp = gpar(cex = opts$cex.axis))

        ## Label extreme points
        yy <- cdx[show.mx] + 0.75 *
            convertHeight(grobHeight(textGrob("text",
                                              gp = gpar(cex = cex.id / sqrt(nrow * ncol)))),
                          "native", TRUE)
        text.id(show.mx, yy, show.mx, adj.x = FALSE)

        grid.rect(gp = gpar(fill = "transparent"))
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = "Observation Number", ylab = "Cook's Distance",
                 main = getCaption(3))
        dev.flush()

        plotID <- plotID + 1
    }


  # ----------------------------------------------------------------------------------- #
  #                                                                             QQ PLOT
    if (5 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))

      # calculate qq-values
        qq <- qqnorm(rs, plot.it = FALSE)

        opts$LOE <- TRUE
        largesample <- opts$largesample
        opts$largesample <- FALSE
        ylim <- getLim(qq$y)
        if (id.n > 0)
            ylim <- getLim(ylim)
        ylim[2] <- ylim[2] + diff(ylim) * 0.075
        # iNZightPlots:::iNZscatterplot(qq$x, qq$y, axis = c(2, 2, 0, 0),
        #                               xlim = getLim(qq$x), ylim = ylim,
        #                               layout = scatter.layout, opts = opts)
        opts$LOE <- FALSE
        opts$largesample <- largesample

        seekViewport("SCATTERVP")

        ## Label extreme points

        if (id.n > 0) {
            xx <- qq$x[show.rs]
            xx <- xx + ifelse(xx < mean(qq$x), 1, -1) *
                (convertWidth(unit(0.4, "char"), "native", TRUE) +
                 convertWidth(grobWidth(textGrob(as.character(show.rs),
                                                 gp =
                                                 gpar(cex = cex.id / sqrt(nrow * ncol)))),
                              "native", TRUE) * 0.5)

            text.id(xx, qq$y[show.rs], show.rs, adj.x = FALSE)
        }

        ## Add the shapiro-wilk test text
        if (length(rs) > 3 & length(rs) < 5000) {
            stest <- shapiro.test(rs)
            p.val <- round(stest$p.value, 3)
            if (p.val == 0)
                p.val <- "< 0.001"
            else
                p.val <- paste("=", p.val)

            txt <- paste("Shapiro-Wilk Normality Test:", "\n", "W = ",
                         round(stest$statistic, 4), "\n", "P-value ",
                         p.val, sep = "")

            grid.text(txt, x = 0.1, y = 0.9,
                      gp = gpar(cex = 1 / sqrt(nrow * ncol)), just = "left")
        }

        grid.rect(gp = gpar(fill = "transparent"))
        popViewport()
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
                 main = getCaption(5))
        dev.flush()

        plotID <- plotID + 1
    }

    # ----------------------------------------------------------------------------------- #
  #                                                                             HISTOGRAM
    if (6 %in% which) {
        if (showAllPlots) nextVP(plotID, nrow, ncol) else newPlot(plotID)
        dev.hold()

        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))

        h <- hist(r, plot = FALSE)
        xlab <- "Residuals"
        mx <- mean(r)
        sx <- sd(r)
        rx <- range(r)
        xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
        xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])
        ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05

        pushViewport(viewport(xscale = getLim(c(xmin, xmax)),
                              yscale = getLim(c(0, ymax))))
        grid.xaxis(gp = gpar(cex = opts$cex.axis))
        grid.yaxis(gp = gpar(cex = opts$cex.axis))

        xx <- h$breaks
        yy <- h$density

      # Need to make a vector of points for corners of polygons
        wx <- diff(xx)[1] / 2
        x.mid <- xx[-length(xx)] + wx
        xmat <- sapply(x.mid, function(x) x + c(-1, -1, 1, 1) * wx)
        ymat <- sapply(yy, function(y) y * c(0, 1, 1, 0))
        matid <- matrix(rep(1:ncol(xmat), each = 4), nrow = 4)

        grid.polygon(xmat, ymat, id = matid, default.units = "native",
                     gp = gpar(fill = "light blue"))

      # Overlay the normal curve
        xc <- seq(xmin, xmax, length = 101)
        yc <- dnorm(xc, mx, sx)
        grid.lines(xc, yc, default.units = "native",
                   gp = gpar(lwd = 1.5, lty = 3))

        grid.rect(gp = gpar(fill = "transparent"))
        popViewport()
        seekViewport(paste("VP", plotID, sep = "-"))

        drawLabs(xlab = "Residuals", ylab = "Density",
                 main = getCaption(6))
        dev.flush()

        plotID <- plotID + 1
    }
}
