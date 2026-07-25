#' These plots are an extension of the original plots provided by
#' \code{plot.lm}.
#' \cr \cr
#' Six plots are currently available: residuals versus fitted,
#' Scale-Location of \eqn{\sqrt{| residuals|}}{sqrt{|residual|}} against
#' fitted values, residuals against leverages, Cook's distance, Normal
#' Q-Q plot and histogram of residuals.
#' \cr \cr
#' Also provided is the summary plot which shows all diagnostic plots
#' arranged in a 2 by 3 grid. By default, this is shown first, then each
#' of the individual plots in turn.
#'
#' For the residuals versus fitted values plot, we add bootstrapped
#' smoothers to illustrate variance. The smoother is also added to the
#' Scale-Location plot.
#' \cr \cr
#' The Normal Q-Q and histogram plots are taken from the \code{normcheck}
#' function in the \code{s20x} package.
#'
#' @title Extended Plot Diagnostics for (g)lm Models
#'
#' @param x an \code{lm} object, typically the result of
#' \code{\link{lm}} or \code{\link{glm}}. Can also take
#' \code{\link[survey]{svyglm}} objects.
#'
#' @param which numeric, if a subset of the plots is required, specify a subset of
#' the numbers \code{1:6}. \code{7} will produce a summary plot showing
#' all of the plots arranged in a a grid. \code{1:6} will show the
#' summary plot followed by each of the single plots one by one
#' (default).
#'
#' @param panel panel function. the useful alternative to \code{\link{points}},
#' \code{\link{panel.smooth}} can be chosen by \code{add.smooth = TRUE}.
#'
#' @param sub.caption common title. Above the figures if there are more than one; used as
#' \code{sub} (s.\code{\link{title}}) otherwise. If \code{NULL}, as by
#' default, a possible abbreviated version of \code{deparse(x$call)} is
#' used.
#'
#' @param main title to each plot, in addition to \code{caption}.
#'
#' @param ask logical, if \code{TRUE}, the user is \emph{ask}ed before each plot,
#' see \code{\link{par}(ask=.)}. Ignored when only one plot is being
#' shown.
#'
#' @param id.n number of points to be labelled in each plot, starting with the most
#' extreme.
#'
#' @param labels.id vector of labels, from which the labels for extreme plots will be
#' chosen. \code{NULL} uses observation numbers.
#'
#' @param cex.id magnification of point labels.
#'
#' @param qqline logical, if \code{TRUE}, a \code{\link{qqline}()} is added to the
#' normal QQ plot.
#'
#' @param cook.levels levels of the Cook's distance at which to draw contours.
#'
#' @param add.smooth logical, if \code{TRUE}, a smoother is drawn to the appropriate
#' plots; see also \code{panel} above.
#'
#' @param label.pos positioning of labels, for the left half and right half of the graph
#' respectively, for plots 1--3.
#'
#' @param cex.caption controls the size of \code{caption}.
#'
#' @param showBootstraps logical, if \code{TRUE}, bootstrap loess smoothers are drawn in the
#' first 4 plots. By default, only drawn for sample sizes of at least 30.
#'
#' @param use.inzightplots logical, if set to \code{TRUE}, the iNZightPlots
#' package will be used for plotting, rather than base R graphics.
#'
#' @param env environment for performing bootstrap simulations (i.e., to find the dataset!)
#' @param ... other arguments to be passed to through to plotting functions.
#'
#' @return No return value; called for the side-effect of producing a plot.
#'
#' @author Simon Potter, David Banks, Tom Elliott.
#'
#' @seealso \code{\link{histogramArray}}, \code{\link{iNZightQQplot}}
#'
#' @export
#' @examples
#'
#' m <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
#' plotlm6(m, which = 1)
#'
#' # the summary grid:
#' plotlm6(m, which = 7)
#'
#' \donttest{# the default cycles through all 6 plots
#' plotlm6(m)
#' }
plotlm6 <- function(x, which = 1:6,
                    panel = if (add.smooth) panel.smooth
                            else points, sub.caption = NULL,
                    main = "",
                    ask = prod(par("mfcol")) < length(which) && dev.interactive(),
                    id.n = 3, labels.id = names(residuals(x)),
                    cex.id = 0.75, qqline = TRUE, cook.levels = c(0.5, 1),
                    add.smooth = getOption("add.smooth", TRUE), label.pos = c(4, 2),
                    cex.caption = 1,
                    showBootstraps = nrow(x$model) >= 30 && nrow(x$model) < 4000,
                    use.inzightplots = FALSE,
                    env = parent.frame(),
                    ...) {


    ## disable bootstraps for survey designs:
    if (inherits(x, "svyglm"))
        showBootstraps <- FALSE

    ## disable smoother for intercept-only models
    if (add.smooth && ncol(x$model) == 1) add.smooth <- FALSE
    if (showBootstraps && !add.smooth) showBootstraps <- FALSE

    ## Use grid graphics from iNZightPlots if they're available.
    if (FALSE && use.inzightplots && requireNamespace("iNZightPlots", TRUE)) {
        plotlm6grid(x = x, which = which, panel = panel, sub.caption = sub.caption,
                    main = main, ask = ask, id.n = id.n, labels.id = labels.id,
                    cex.id = cex.id, qqline = qqline, cook.levels = cook.levels,
                    add.smooth = add.smooth, label.pos = label.pos,
                    cex.caption = cex.caption, showBootstraps = showBootstraps, env = env,
                    ...)
        return(invisible(NULL))
    }

    smColour = "orangered"      # colour of data loess line
    bsmColour = "lightgreen"    # colour of bootstrap loess lines

    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            #warning("Not plotting observations with leverage one:\n  ",
            #        paste(which(isInf), collapse = ", "), call. = FALSE)
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
            labpos <- if (adj.x)
                label.pos[1 + as.numeric(x > mean(range(x)))]
            else 3
            text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
                 pos = labpos, offset = 0.25)
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
        cc <- deparse(cal, 90)
        ## nc <- nchar(cc[1], "c")
        ## abbr <- length(cc) > 1 || nc > 90
        ## sub.caption <- if (abbr)
        ##     paste(substr(cc[1], 1, min(90, nc)), "...")
        ## else cc[1]
        sub.caption <- cc[1]
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
        bsModels = try(bootstrapModels(x, env = env), TRUE)

        if (inherits(bsModels, "try-error")) {
            ## turn off bootstrapping if it fails
            showBootstraps <- FALSE
            warning("Could not generate boostraps.")
        } else {
            try({
                nBootstraps = length(bsModels)
                ## New bootstrapped values (bs suffix stands for bootstrap)
                rbs = rsbs = rbs.w = wbs = wbsind = yhbs = sbs = hiibs =
                    sbs = vector("list", nBootstraps)
                for (i in 1:nBootstraps) {
                    rbs[[i]] = residuals(bsModels[[i]])
                    yhbs[[i]] = predict(bsModels[[i]])
                    wbs[i] = list(weights(bsModels[[i]]))  # list() prevents
                    ## deletion if NULL
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
            }, TRUE) ## end try()
        } ## end else
    } ## end if showBootstraps

    ## If we want to show all of the plots, assume "all" is the
    ## seventh plot
    showAllPlots = all(show)

    ## Ensure par is not globally modified
    origpar = par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    on.exit(par(origpar), add = TRUE)

    for (plotNum in 1:7) {
        # dev.hold()
        # on.exit(dev.flush(), add = TRUE)

        if (showAllPlots & plotNum == 1) {
            ## We are showing all plots
            showPlot = rep(TRUE, 6)
            par(mfrow = c(2, 3), oma = c(2, 0, 0, 0))
        } else {
            ## If we are only showing the summary plot
            ## skip any specific plot
            if (onlyShowAll)
                next

            ## Just show a specific plot
            showPlot = rep(FALSE, 6)
            ## See if we need to plot the "current" plot
            showPlot[plotNum - 1] = show[plotNum - 1]
            par(mfrow = c(1, 1))
        }

        if (showPlot[1]) tryOrErrorPlot({
            ylim <- range(r, na.rm = TRUE)
            if (id.n > 0)
                ylim <- extendrange(r = ylim, f = 0.08)
            plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
                 ylim = ylim, ...)

            if (showBootstraps) {
                ## Draw bootstrap sample loess lines
                for (i in 1:nBootstraps) {
                    bsm = suppressWarnings(loess(rbs[[i]] ~ yhbs[[i]]))
                    bsmOrd = order(bsm$x)
                    lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd],
                          col = bsmColour)
                }
            }
            if (ncol(x$model) > 1) {
                ## Draw loess line for original data set
                sm = suppressWarnings(loess(r ~ yh))
                smOrd = order(sm$x)
                lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)
            }

            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            mtext(getCaption(1), 3, 0.25, cex = cex.caption)
            if (id.n > 0) {
                y.id <- r[show.r]
                y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
                text.id(yh[show.r], y.id, show.r)
            }
            abline(h = 0, lty = 3, col = "gray")
        }, "Unable to plot Residuals vs Fitted :(")

        if (showPlot[2]) tryOrErrorPlot({
            sqrtabsr <- sqrt(abs(rs))
            ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
            yl <- as.expression(substitute(sqrt(abs(YL)),
                list(YL = as.name(ylab23))))
            yhn0 <- if (is.null(w))
                        yh
                    else yh[w != 0]
            plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main,
                 ylim = ylim, ...)

            if (showBootstraps) {
                ## Draw bootstrap sample loess lines
                for (i in 1:nBootstraps) {
                    bsm = suppressWarnings(
                        loess(sqrt(abs(rsbs[[i]])) ~ yhbs[[i]])
                    )
                    bsmOrd = order(bsm$x)
                    lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd],
                          col = bsmColour)
                }
            }
            ## Draw loess line for original data set
            sm = suppressWarnings(loess(sqrtabsr ~ yhn0))
            smOrd = order(sm$x)
            lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)

            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            mtext(getCaption(2), 3, 0.25, cex = cex.caption)
            if (id.n > 0)
                text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
        }, "Unable to plot Scale-Location :(")

        if (showPlot[3]) tryOrErrorPlot({
            ylab5 <- if (isGlm(x))
                "Std. Pearson resid."
            else "Standardized residuals"
            r.w <- residuals(x, "pearson")
            if (!is.null(w))
                r.w <- r.w[wind]
            rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)

            if (showBootstraps) {
                ## bootstrap rsp
                rspbs = vector("list", nBootstraps)
                for (i in 1:nBootstraps) {
                    pearsonResid = residuals(bsModels[[i]], "pearson")
                    rspbs[[i]] =  dropInf(
                        pearsonResid / (sbs[[i]] * sqrt(1 - hiibs[[i]])),
                        hiibs[[i]])
                }
            }

            ylim <- range(rsp, na.rm = TRUE)
            if (id.n > 0) {
                ylim <- extendrange(r = ylim, f = 0.08)
                show.rsp <- order(-cook)[iid]
            }
            do.plot <- TRUE

            xx <- hii
            xx[xx >= 1] <- NA
            plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)),
                 ylim = ylim, main = main, xlab = "Leverage",
                 ylab = ylab5, ...)
            if (showBootstraps) {
                ## Bootstrap smooths
                for (i in 1:nBootstraps) {
                    xxbs = hiibs[[i]]
                    xxbs[xxbs >= 1] = NA
                    bsm = suppressWarnings(loess(rspbs[[i]] ~ xxbs))
                    bsmOrd = order(bsm$x)
                    lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd],
                          col = bsmColour)
                }
            }
            ## Original data smooth
            sm = suppressWarnings(loess(rsp ~ xx))
            smOrd = order(sm$x)
            lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)

            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            if (length(cook.levels)) {
                p <- length(coef(x))
                usr <- par("usr")
                hh <- seq.int(min(r.hat[1L], r.hat[2L]/100),
                              usr[2L], length.out = 101)
                for (crit in cook.levels) {
                    cl.h <- sqrt(crit * p * (1 - hh)/hh)
                    lines(hh, cl.h, lty = 2, col = 2)
                    lines(hh, -cl.h, lty = 2, col = 2)
                }
                legend("bottomleft", legend = "Cook's distance",
                       lty = 2, col = 2, bty = "n")
                xmax <- min(0.99, usr[2L])
                ymult <- sqrt(p * (1 - xmax)/xmax)
                aty <- c(-sqrt(rev(cook.levels)) * ymult,
                         sqrt(cook.levels) * ymult)
                axis(4, at = aty,
                     labels = paste(c(rev(cook.levels),
                         cook.levels)), mgp = c(0.25, 0.25, 0), las = 2,
                     tck = 0, cex.axis = cex.id, col.axis = 2)
            }

            if (do.plot) {
                mtext(getCaption(3), 3, 0.25, cex = cex.caption)
                if (id.n > 0) {
                    y.id <- rsp[show.rsp]
                    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
                    text.id(xx[show.rsp], y.id, show.rsp)
                }
            }
        }, "Unable to plot Residuals vs Leverage :(")

        if (showPlot[4]) tryOrErrorPlot({
            ## cooks distance
            cdx <- cooks.distance(x)
            show.mx <- order(-cdx)[1:3]
            plot(1:length(cdx), cdx, type = "h", main = main,
                 xlab = "observation number", ylab = "cook's distance")
            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            mtext(getCaption(4), 3, 0.25, cex = cex.caption)
            text(show.mx, cdx[show.mx] + 0.4 * 0.75 * strheight(" "),
                 show.mx)
        }, "Unable to plot Cook's distance :(")

        if (showPlot[5]) tryOrErrorPlot({
            ylim <- range(rs, na.rm = TRUE)
            ylim[2] <- ylim[2] + diff(ylim) * 0.075
            qq <- normCheck(rs, main = main, ylab = ylab23,
                            ylim = ylim, ...)
            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            mtext(getCaption(5), 3, 0.25, cex = cex.caption)
            # print(show.rs)
            if (id.n > 0)
                text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
        }, "Unable to plot Normal QQ plot :(")

        if (showPlot[6]) tryOrErrorPlot({
            ## Histogram
            h <- hist(r, plot = FALSE)
            xlab <- "Residuals"
            mx <- mean(r)
            sx <- sd(r)
            rx <- range(r)
            xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
            xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])
            ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05
            hist(r, prob = TRUE, ylim = c(0, ymax), xlim = c(xmin, xmax),
                 xlab = xlab, col = "light blue",
                 main = main)
            if (!onlyShowAll)
                title(sub = sub.caption, ...)
            mtext(getCaption(6), 3, 0.25, cex = cex.caption)
            box()
            x1 <- seq(xmin, xmax, length = 100)
            y1 <- dnorm(x1, mx, sx)
            lines(x1, y1, lwd = 1.5, lty = 3)
        }, "Unable to plot Histogram :(")
    }

    if (onlyShowAll) #!one.fig && par("oma")[3] >= 1)
        mtext(sub.caption, 1, outer = TRUE, cex = 1)

    invisible()
}

# A modified version of the s20x::normcheck default method
normCheck <-
    function (x, col = NULL, shapiroWilk = TRUE, plot = TRUE, ...) {
     ## Note: this is a bit nasty, consider rewriting
     ## If 'main' has been passed in, use it, otherwise leave as
     ## empty
        moreargs <- list(...)
        qqp =
            if ("main" %in% names(moreargs))
                qqnorm(x, plot.it = plot, ...)
            else
                qqnorm(x, plot.it = plot, main = "", ...)

        if (plot) {
            mx <- mean(x, na.rm = TRUE)
            sx <- sd(x, na.rm = TRUE)
            abline(c(mx, sx), col = "gray50")
            if (shapiroWilk) {
                if (length(x) <= 5000) {
                    stest <- shapiro.test(x)
                    txt <- paste("Shapiro-Wilk normality test",
                                 "\n", "W = ", round(stest$statistic, 4),
                                 "\n", "P-value = ",
                                 round(stest$p.value, 3), sep = "")

                }
                else {
                    txt <- paste('Shapiro-Wilk normality test requires',
                                 'sample size less than 5000.', sep = '\n')
                }
                text(sort(qqp$x)[2], 0.99 * max(qqp$y, na.rm = TRUE),
                     txt, adj = c(0, 1))
            }
        }
        qqp
    }
