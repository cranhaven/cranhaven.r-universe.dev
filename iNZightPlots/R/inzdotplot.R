#' @export
create.inz.dotplot <- function(obj, hist = FALSE, ...) {
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    features <- opts$plot.features
    if (!is.null(features$order.first)) {
        ## can specify value = -1 to ignore reordering
        if (all(features$order.first > 0)) {
            ord <- (1:nrow(df))
            wi <- which(ord %in% features$order.first)
            ord <- c(ord[-wi], ord[wi])
            df <- df[ord, ]
        }
    }

    boxplot <- opts$boxplot
    mean_indicator <-
        if (is.logical(opts$mean_indicator)) {
            opts$mean_indicator
        } else {
            opts$mean_indicator %in% c("grand", "group")
        }
    if (!is.null(opts$inference.par) && !is.null(opts$inference.type)) {
        if (opts$inference.par == "mean") {
            boxplot <- FALSE
            mean_indicator <- TRUE
        }
        if (opts$inference.par == "median") {
            boxplot <- TRUE
            mean_indicator <- FALSE
        }
    }

    v <- colnames(df)
    vn <- xattr$varnames

    # trimX <- xattr$trimX

    if (xattr$class != "inz.simple") {
        hist <- TRUE
    }

    if (xattr$class == "inz.survey") {
        df <- df$variables
    }

    # May need to switch around X and Y:
    if (all(c("x", "y") %in% v)) {
        if (is.factor(df$x) & is.numeric(df$y)) {
            X <- df$y
            df$y <- df$x
            df$x <- X
            Xn <- vn$y
            vn$y <- vn$x
            vn$x <- Xn
            xattr$xrange <- xattr$yrange
            xattr$yrange <- NULL

            obj$df <- df
            xattr$varnames <- vn
        }
    }

    # 'trim' X values
    # if (!is.null(trimX)) {
    #    df <- df[df$x > min(trimX) & df$x < max(trimX), , drop = FALSE]
    # }

    # first need to remove missing values
    missing <- is.na(df$x)
    if ("y" %in% colnames(df)) {
        y.levels <- levels(df$y) # need to save these before we remove missing ...
        missing <- missing | is.na(df$y)
    }

    n.missing <- sum(missing)

    if ("y" %in% colnames(df)) {
        attr(n.missing, "levels") <- tapply(missing, df$y, sum)
    }

    df <- df[!missing, , drop = FALSE]

    ## The plotting symbol:
    if ("symbolby" %in% v) {
        df$pch <- (21:25)[as.numeric(df$symbolby)]
        df$pch[is.na(df$pch)] <- 3
    } else {
        df$pch <- rep(ifelse(opts$pch == 1, 21, opts$pch), nrow(df))
    }
    if (opts$fill.pt == "transparent" & opts$alpha < 1) {
        opts$fill.pt <- "fill"
    }

    ## Return a LIST for each level of y
    if ("y" %in% colnames(df)) {
        out <- vector("list", length(levels(df$y)))
        names(out) <- levels(df$y)
        id <- df$y
    } else {
        out <- list(all = NULL)
        id <- rep("all", nrow(df))
    }

    for (i in unique(id)) {
        if (xattr$class == "inz.freq") {
            di <- svydesign(ids = ~1, weights = dfi$freq, data = dfi)
        } else if (xattr$class == "inz.survey") {
            if ("y" %in% colnames(obj$df$variables)) {
                ss <- obj$df$variables$y == i & !is.na(obj$df$variables$y)
                di <- obj$df[ss]
            } else {
                di <- obj$df
            }
        } else {
            ss <- id == i & !is.na(id)
            dfi <- df[ss, , drop = FALSE]
            dfi$y <- NULL
            di <- dfi
        }

        out[[i]] <- di
    }

    makeHist <- function(d, nbins, xlim, bins = NULL) {
        if (is.null(d)) {
            return(NULL)
        }

        if (is.null(nbins)) {
            range <- range(bins)
            cuts <- bins
        } else {
            ## Create even cut points in the given data range:
            range <- xlim
            range <- extendrange(range, f = 0.01) ## is this necessary?
            cuts <- seq(range[1] - 0.1, range[2] + 0.1, length = nbins + 1)
        }

        bin.min <- cuts[-(nbins + 1)]
        bin.max <- cuts[-1]

        ## Cut the data and calculate counts:
        if (is_survey(d)) {
            ## To do this, we will pretty much grab stuff from the `survey` package, however it
            ## cannot be used separately to produce the bins etc without plotting it; so copyright
            ## for the next few lines goes to Thomas Lumley.
            h <- hist(x <- d$variables$x, breaks = cuts, plot = FALSE)

            ## We can run into problems with PSUs have single clusters, so:
            oo <- options()$survey.lonely.psu
            options(survey.lonely.psu = "certainty")
            probs <- coef(
                svymean(
                    ~ cut(d$variables$x, h$breaks,
                        include.lowest = TRUE,
                        deff = FALSE,
                        estimate.only = TRUE
                    ),
                    d,
                    na.rm = TRUE
                )
            )
            options(survey.lonely.psu = oo)

            h$density <- probs / diff(h$breaks)
            h$counts <- probs * sum(get_weights(d))
        } else {
            x <- d$x
            h <- tryCatch(
                hist(x, breaks = cuts, plot = FALSE),
                error = function(e) hist(x, breaks = length(cuts), plot = FALSE)
            )
        }

        ret <- list(
            breaks = cuts,
            counts = as.numeric(h$counts),
            density = as.numeric(h$density),
            mids = h$mids,
            x = sort(x)
        )

        if (!hist) {
            ret$y <- unlist(
                sapply(h$counts[h$counts != 0], function(c) 1:c)
            )
            if ("colby" %in% colnames(d)) {
                ret$colby <- d$colby[order(x)]
            }
            if ("pch" %in% colnames(d)) {
                ret$pch <- d$pch[order(x)]
            }
        }

        ret$extreme.ids <- NULL

        if ("extreme.label" %in% v) {
            eLab <- as.character(d$extreme.label)[order(x)]

            nx <- rep(xattr$nextreme, length = 2L)

            if (sum(nx) >= nrow(d)) {
                # just label all the points!
                text.labels <- eLab
                ret$extreme.ids <- d$pointIDs[order(x)]
            } else {
                min <- 1:nx[1]
                max <- (nrow(d) - nx[2] + 1):nrow(d)

                text.labels <- character(nrow(d))
                if (nx[1] > 0) {
                    text.labels[min] <- eLab[min]
                }
                if (nx[2] > 0) {
                    text.labels[max] <- eLab[max]
                }

                pointIDs <- d$pointIDs[order(x)]
                # now - are we labelling points with same-level-of??
                if (!is.null(d$locate.same.level)) {
                    w <- which(text.labels != "")
                    loc.lvls <- as.character(unique(d$locate.same.level[order(x)][w]))
                    wi <- d$locate.same.level[order(x)] %in% loc.lvls
                    text.labels <- ifelse(wi, eLab, "")
                    ret$extreme.ids <- d$pointIDs[order(x)][wi]
                } else {
                    ret$extreme.ids <- pointIDs[text.labels != ""]
                }
            }
        } else {
            text.labels <- as.character(d$locate)[order(x)]
        }

        ret$text.labels <- text.labels

        if ("highlight" %in% colnames(d)) {
            ret$highlight <- d$highlight[order(x)]
        }

        attr(ret, "order") <- order(x)
        ret
    }


    boxinfo <- if (boxplot & (!"mean" %in% opts$inference.par) & nrow(df) > 5) {
        boxSummary(out, opts)
    } else {
        NULL
    }

    meaninfo <- if (mean_indicator) {
        meanSummary(out, opts)
    } else {
        NULL
    }


    nbins <- bins <- NULL
    if (hist) {
        ## some option here to adjust the number of bins (e.g., sample size < 100?)
        nbins <- if (is.null(opts$hist.bins)) {
            wd <- convertWidth(
                unit(1.2 * opts$cex.dotpt, "char"),
                "npc",
                valueOnly = TRUE
            )
            floor(1 / (wd))
        } else {
            opts$hist.bins
        }
    } else {
        ## compute the smallest non-zero difference, and deduce if it is a common
        ## factor of all the differences:
        # diffs <- do.call(c, lapply(out, function(d) {
        #    if (is.null(d$x)) return(NULL)
        #
        #    diffs <- diff(sort(d$x))
        #    diffs[diffs > 0]
        # }))

        # mdiff <- if (length(diffs) > 0) min(diffs) else 0
        # fdiff <- diffs / mdiff
        # isDiscrete <- all(round(fdiff) == fdiff)

        # xr <- diff(range(sapply(out, function(d) if (is.null(d$x)) 0 else range(d$x))))

        # mult.width <- ifelse(isDiscrete, 1, 1.2)

        dp <- xattr$dotplotstuff
        mdiff <- dp$mdiff
        xr <- dp$xr
        isDiscrete <- dp$isDiscrete
        mult.width <- dp$mult.width

        if ("symbol.width" %in% names(xattr)) {
            symbol.width <- xattr$symbol.width * xr
        } else {
            symbol.width <- convertWidth(
                unit(opts$cex.dotpt, "char"),
                "native",
                valueOnly = TRUE
            )
        }

        if (symbol.width < mdiff) {
            ## If the symbols are smaller than the smallest differences,
            ## then just use all of the values as bins!
            xx <- unique(
                do.call(
                    c,
                    lapply(
                        out,
                        function(d) unique(d$x)
                    )
                )
            )
            if (is.null(xx)) {
                bins <- NA
            } else if (mdiff == 0) {
                bins <- xx[1]
            } else {
                bins <- seq(
                    min(xx, na.rm = TRUE) - 0.5 * mdiff,
                    max(xx, na.rm = TRUE) + 0.5 * mdiff,
                    by = mdiff
                )
            }
        } else {
            wd <- xattr$symbol.width * 1.2

            nbins <- floor(1 / (wd))
            if (nbins == 0) {
                nbins <- if (is.null(opts$hist.bins)) {
                    wd <- convertWidth(
                        unit(1.2 * opts$cex.dotpt, "char"),
                        "npc",
                        valueOnly = TRUE
                    )
                    floor(1 / (wd))
                } else {
                    opts$hist.bins
                }
            }
        }
    }

    plist <- lapply(out, makeHist,
        nbins = nbins,
        xlim = xattr$xrange,
        bins = bins
    )


    ## Generate a list of the inference information for each plot:
    inflist <- dotinference(obj)

    out <- list(
        toplot = plist,
        n.missing = n.missing,
        boxinfo = boxinfo,
        inference.info = inflist,
        fill.pt = opts$fill.pt,
        meaninfo = meaninfo,
        nacol =
            if ("colby" %in% v) {
                any(
                    sapply(
                        plist,
                        function(T) {
                            if (is.null(T$colby)) FALSE else any(is.na(T$colby))
                        }
                    )
                )
            } else {
                FALSE
            },
        xlim =
            if (nrow(df) > 0) {
                range(df$x, na.rm = TRUE)
            } else {
                c(-Inf, Inf)
            },
        ylim = c(
            0,
            max(
                sapply(
                    plist,
                    function(p) if (is.null(p)) 0 else max(p$counts)
                )
            )
        ),
        n.label =
            if (is.null(xattr$nextreme)) {
                NULL
            } else {
                rep(xattr$nextreme, length = 2)
            }
    )

    class(out) <- ifelse(hist, "inzhist", "inzdot")

    out
}

plot.inzdot <- function(obj, gen, hist = FALSE) {
    # First step is to grab stuff:
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args
    boxplot <- opts$boxplot
    mean_indicator <- !is.null(obj$meaninfo)

    expand.points <- 1 # if (is.null(opts$expand.points)) 1 else opts$expand.points

    addGrid(x = TRUE, gen = gen, opts = opts)

    toplot <- obj$toplot
    boxinfo <- obj$boxinfo
    meaninfo <- obj$meaninfo
    inflist <- obj$inference.info

    nlev <- length(toplot)
    pushViewport(
        viewport(
            layout = grid.layout(nrow = nlev),
            name = "VP:dotplot-levels",
            clip = "on"
        )
    )
    Hgts <- if (!is.null(boxinfo) || !is.null(meaninfo) || !is.null(inflist)) {
        c(3, 1)
    } else {
        c(1, 0)
    }
    dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))

    # we need to make the dots stack nicely, if they fit
    maxcount <- gen$maxcount
    seekViewport("VP:dotplot-levels")
    pushViewport(viewport(layout.pos.row = 1))
    pushViewport(viewport(layout = dpLayout))
    pushViewport(viewport(layout.pos.row = 1)) # this is where dots will go

    ht <- convertHeight(
        unit(opts$cex.dotpt, "char"),
        "npc",
        valueOnly = TRUE
    )
    ny <- floor(
        convertHeight(
            unit(1, "npc"),
            "npc",
            valueOnly = TRUE
        ) / ht
    )

    maxdots <- max(ny, maxcount)
    ylim <- c(0, maxdots * 1.05)

    GROUP.names <-
        if (nlev > 1 & opts$internal.labels) {
            names(toplot)
        } else {
            NULL
        }

    for (i in 1:nlev) {
        pp <- toplot[[i]]

        seekViewport("VP:dotplot-levels")
        pushViewport(viewport(layout.pos.row = i))
        pushViewport(viewport(layout = dpLayout))

        pushViewport(
            viewport(
                layout.pos.row = 2,
                xscale = xlim,
                clip = "on"
            )
        )

        if (boxplot) {
            addBoxplot(boxinfo[[i]], opts, i)
        }

        if (mean_indicator) {
            addMean(meaninfo[[i]], opts, i)
        }

        if (!is.null(inflist)) {
            addUnivarInference(inflist, i, opts)
        }

        upViewport()

        vpname <- ifelse(nlev == 1,
            "VP:plotregion",
            paste0("VP:plotregion-", i)
        )

        pushViewport(
            viewport(
                layout.pos.row = 1,
                xscale = xlim,
                yscale = ylim,
                name = vpname
            )
        )

        ptCols <- colourPoints(pp$colby, col.args, opts)
        if (length(ptCols) == 1) {
            ptCols <- rep(ptCols, length(pp$x))
        }

        ptPch <- rep(opts$pch, length(pp$x))

        if (!is.null(pp$text.labels)) {
            locID <- which(pp$text.labels != "")
            if (!is.null(col.args$locate.col)) {
                ptCols[locID] <- col.args$locate.col
                ptPch[locID] <- 19
            }
        }

        if (length(pp$x) > 0) {
            NotInView <- pp$x < min(xlim) | pp$x > max(xlim)
            ptPch[NotInView] <- NA
            grid.points(pp$x, pp$y,
                pch = pp$pch,
                gp = gpar(
                    col = ptCols,
                    cex = opts$cex.dotpt / expand.points,
                    lwd = opts$lwd.pt,
                    alpha = opts$alpha,
                    fill = if (obj$fill.pt == "fill") ptCols else obj$fill.pt
                ),
                name = paste("inz-DOTPOINTS", opts$rowNum, opts$colNum, i, sep = ".")
            )

            ## Highlighting:
            if (!is.null(pp$highlight) & length(ptCols) > 1) {
                hl <- as.logical(pp$highlight)
                if (sum(hl) > 0) {
                    hcol <-
                        if (opts$highlight.col == "shade") {
                            shade(ptCols[hl], 0.6)
                        } else {
                            opts$highlight.col
                        }

                    grid.points(pp$x[hl], pp$y[hl],
                        pch = 19,
                        gp = gpar(
                            col = hcol,
                            cex = opts$cex.dotpt * 1.4,
                            lwd = opts$lwd.pt
                        ),
                        name = paste(
                            "inz-point-highlight-out",
                            opts$rowNum,
                            opts$colNum,
                            i,
                            sep = "."
                        )
                    )

                    grid.points(pp$x[hl], pp$y[hl],
                        pch = 19,
                        gp = gpar(
                            col = ptCols[hl],
                            cex = opts$cex.dotpt,
                            lwd = opts$lwd.pt
                        ),
                        name = paste(
                            "inz-point-highlight-in",
                            opts$rowNum,
                            opts$colNum,
                            i,
                            sep = "."
                        )
                    )
                }
            }
        }

        ## Label extremes
        if (!is.null(pp$text.labels)) {
            if (sum(!is.na(pp$text.labels) > 0) & (length(locID) > 0)) {
                grid.text(
                    paste0("  ", pp$text.labels[locID]),
                    pp$x[locID],
                    pp$y[locID],
                    default.units = "native",
                    just = c("left"),
                    rot = 45,
                    gp = gpar(cex = 0.6),
                    name = paste("inz-label-extreme", i, sep = ".")
                )
            }
        }

        ## Label group
        if (!is.null(GROUP.names)) {
            grid.text(
                GROUP.names[i],
                x = 0.01,
                y = 0.98,
                just = c("left", "top"),
                gp = gpar(cex = 0.8),
                name = paste("inz-group-label", i, sep = ".")
            )
        }
    }

    seekViewport("VP:dotplot-levels")
    upViewport()

    if (!is.null(inflist)) {
        if ("comp" %in% names(inflist[[1]])) {
            addUnivarCompLines(inflist)
        }
    }
}





boxSummary <- function(obj, opts) {
    lapply(
        obj,
        function(o) {
            if (is.null(o)) {
                return(NULL)
            }

            if (is_survey(o)) {
                if (nrow(o$variables) < 5) {
                    return(NULL)
                }
                svyobj <- o
                if (utils::packageVersion("survey") >= "4.1") {
                    quant <- svyquantile(~x, svyobj,
                        quantiles = c(0.25, 0.5, 0.75),
                        na.rm = TRUE,
                        ci = FALSE
                    )[[1]]
                } else {
                    quant <- svyquantile(~x, svyobj,
                        quantiles = c(0.25, 0.5, 0.75),
                        na.rm = TRUE
                    )
                }

                min <- min(svyobj$variables$x, na.rm = TRUE)
                max <- max(svyobj$variables$x, na.rm = TRUE)
            } else {
                quant <- quantile(o$x,
                    probs = c(0.25, 0.5, 0.75),
                    na.rm = TRUE
                )
                min <- min(o$x, na.rm = TRUE)
                max <- max(o$x, na.rm = TRUE)
            }

            list(
                quantiles = quant,
                min = min,
                max = max,
                inference = FALSE,
                opts = opts
            )
        }
    )
}



addBoxplot <- function(x, opts, i) {
    r <- opts$rowNum
    c <- opts$colNum
    opts <- x$opts

    if (is.null(x)) {
        return()
    }

    xx <- rep(x$quantiles, each = 4)[3:10]
    yy <- rep(c(0.2, 0.8, 0.8, 0.2), 2)
    id <- rep(1:2, each = 4)
    grid.polygon(unit(xx, "native"), unit(yy, "npc"),
        id = id,
        gp = gpar(
            lwd = opts$box.lwd[1],
            fill = opts$box.fill
        ),
        name = paste("inz-box", r, c, i, sep = ".")
    )
    grid.polyline(
        unit(c(x$min, x$quantiles[1], x$quantiles[3], x$max), "native"),
        rep(0.5, 4),
        id = rep(1:2, each = 2),
        gp = gpar(lwd = opts$box.lwd[2]),
        name = paste("inz-box-line", r, c, i, sep = ".")
    )
}

meanSummary <- function(obj, opts) {
    lapply(
        obj,
        function(o) {
            if (is.null(o)) {
                return(NULL)
            }

            if (is_survey((o))) {
                xhat <- svymean(~x, o, na.rm = TRUE)
            } else {
                xhat <- mean(o$x, na.rm = TRUE)
            }

            list(
                mean = xhat,
                opts = opts
            )
        }
    )
}

addMean <- function(x, opts, i) {
    r <- opts$rowNum
    c <- opts$colNum
    opts <- x$opts

    if (is.null(x)) {
        return()
    }

    grid.points(unit(x$mean, "native"), unit(0.4, "npc"),
        gp = gpar(
            fill = "black",
            cex = opts$cex * 0.75
        ),
        pch = 24,
        name = paste("inz-mean", r, c, i, sep = ".")
    )
}



dotinference <- function(obj) {
    ## obj: list of data broken down by subsets
    ## opts: various options (inzpar)

    opts <- obj$opts
    xattr <- obj$xattr
    inf.par <- opts$inference.par
    inf.type <- opts$inference.type
    if (!is.null(inf.type) & is.null(inf.par)) {
        inf.par <- c("mean", "median", "iqr")
    }
    bs <- opts$bs.inference

    if (is.null(inf.par) & is.null(inf.type)) {
        return(NULL)
    }

    if (nrow(obj$df) < opts$min.count) {
        return(NULL)
    }

    ## for simplicity, if no 'y' factor, just make all the same level for tapply later:
    if (obj$xattr$class == "inz.simple") {
        svy <- FALSE
        dat <- obj$df
        if (!"y" %in% colnames(dat)) {
            dat <- data.frame(dat, y = factor("all"), stringsAsFactors = TRUE)
            inf.type <- inf.type[inf.type != "comp"]
        } else if (bs) {
            dat <- dat[!is.na(dat$y), ]
        }
    } else {
        ## survey structure ...
        svy <- TRUE
        dat <- obj$df
        if (!"y" %in% colnames(dat$variables)) {
            inf.type <- inf.type[inf.type != "comp"]
        } else if (bs) {
            dat <- dat[!is.na(dat$y), ]
        }
    }

    # CI interval width:
    alpha <- 1 - (1 - opts$ci.width) / 2

    if (!is.null(inf.par)) {
        result.list <- lapply(
            inf.par,
            function(ip) {
                result <- switch(ip,
                    "mean" = {
                        lapply(
                            inf.type,
                            function(type) {
                                switch(type,
                                    "conf" = {
                                        if (bs) {
                                            ## ci.width% bootstrap confidence interval
                                            if (svy) {
                                                if ("y" %in% colnames(dat$variables)) {
                                                    NULL
                                                } else {
                                                    NULL
                                                }
                                            } else {
                                                b <- boot(dat,
                                                    strata = dat$y,
                                                    function(d, f) {
                                                        tapply(d[f, 1], d[f, 2], mean, na.rm = TRUE)
                                                    },
                                                    R = opts$n.boot
                                                )
                                                ci <- cbind(
                                                    t(
                                                        apply(b$t, 2, quantile,
                                                            probs = c(1 - alpha, alpha),
                                                            na.rm = TRUE
                                                        )
                                                    ),
                                                    colMeans(b$t)
                                                )
                                                dimnames(ci) <- list(
                                                    levels(dat$y),
                                                    c("lower", "upper", "mean")
                                                )
                                                ci
                                            }
                                        } else {
                                            ## ci.width% confidence interval (normal theory)
                                            if (svy) {
                                                if ("y" %in% colnames(dat$variables)) {
                                                    ci <- svyby(~x, ~y,
                                                        design = dat,
                                                        svymean,
                                                        level = opts$ci.width,
                                                        drop.empty.groups = FALSE,
                                                        na.rm = TRUE
                                                    )
                                                    ci <- cbind(
                                                        coef(ci),
                                                        confint(ci, level = opts$ci.width)
                                                    )
                                                    dimnames(ci) <- list(
                                                        levels(dat$variables$y),
                                                        c("mean", "lower", "upper")
                                                    )
                                                } else {
                                                    fit <- svymean(~x, dat, na.rm = TRUE)
                                                    ci <- rbind(c(fit[1], confint(fit, level = opts$ci.width)))
                                                    dimnames(ci) <- list("all", c("mean", "lower", "upper"))
                                                }
                                                ci
                                            } else {
                                                n <- tapply(
                                                    dat$x, dat$y,
                                                    function(z) sum(!is.na(z))
                                                )
                                                n <- ifelse(n < 5, NA, n)
                                                wd <- qt(alpha, df = n - 1) *
                                                    tapply(dat$x, dat$y, sd,
                                                        na.rm = TRUE
                                                    ) / sqrt(n)
                                                mn <- tapply(dat$x, dat$y, mean, na.rm = TRUE)
                                                cbind(
                                                    lower = mn - wd,
                                                    upper = mn + wd,
                                                    mean = mn
                                                )
                                            }
                                        }
                                    },
                                    "comp" = {
                                        if (bs) {
                                            if (svy) {
                                                NULL
                                            } else {
                                                b <- boot(dat,
                                                    strata = dat$y,
                                                    function(d, f) {
                                                        tapply(d[f, 1], d[f, 2], mean, na.rm = TRUE)
                                                    },
                                                    R = opts$n.boot
                                                )
                                                cov <- cov(b$t)
                                                ses <- suppressWarnings(iNZightMR::seCovs(cov))
                                                ci <- suppressWarnings(
                                                    iNZightMR::moecalc(
                                                        ses,
                                                        est = tapply(dat$x, dat$y, mean, na.rm = TRUE)
                                                    )
                                                )
                                                cbind(
                                                    lower = ci$compL,
                                                    upper = ci$compU
                                                )
                                            }
                                        } else {
                                            if (svy) {
                                                fit <- svyglm(x ~ y, design = dat)
                                                est <- predict(fit,
                                                    newdata = data.frame(
                                                        y = levels(dat$variables$y),
                                                        stringsAsFactors = TRUE
                                                    )
                                                )
                                                mfit <- suppressWarnings(
                                                    iNZightMR::moecalc(fit, factorname = "y", est = est)
                                                )
                                                cbind(
                                                    with(
                                                        mfit,
                                                        cbind(
                                                            lower = compL,
                                                            upper = compU
                                                        )
                                                    ) + coef(fit)[1],
                                                    mean = est
                                                )
                                            } else {
                                                ## ########################################################## ##
                                                ## This is the old method, an approximately 75% CI ...        ##
                                                ## ########################################################## ##
                                                ## n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))      ##
                                                ## wd <- tapply(dat$x, dat$y, sd, na.rm = TRUE) / sqrt(2 * n) ##
                                                ## mn <- tapply(dat$x, dat$y, mean, na.rm = TRUE)             ##
                                                ## cbind(lower = mn - wd, upper = mn + wd)                    ##
                                                ## ########################################################## ##

                                                ## The new method uses iNZightMR:

                                                if (any(is.na(tapply(dat$x, dat$y, length)))) {
                                                    NULL
                                                } else {
                                                    ycounts <- with(
                                                        dat,
                                                        tapply(x, y, function(x) sum(!is.na(x)))
                                                    )
                                                    if (any(ycounts < 5)) {
                                                        wi <- which(ycounts >= 5)
                                                        if (length(wi) == 1) {
                                                            NULL
                                                        } else {
                                                            ylevi <- levels(dat$y)[wi]
                                                            newdat <- dat[dat$y %in% ylevi, ]
                                                            newdat$y <- factor(newdat$y)
                                                            fit <- lm(x ~ y - 1, data = newdat)
                                                            est <- predict(fit,
                                                                newdata = data.frame(
                                                                    y = levels(newdat$y),
                                                                    stringsAsFactors = TRUE
                                                                )
                                                            )
                                                            ses <- iNZightMR::seIndepSes(summary(fit)$coef[, 2])
                                                            mfit <- suppressWarnings(
                                                                iNZightMR::moecalc(ses, est = est, base = FALSE)
                                                            )
                                                            coef.mat <- matrix(NA,
                                                                ncol = 3,
                                                                nrow = length(levels(dat$y))
                                                            )
                                                            coef.mat[wi, ] <- cbind(
                                                                with(
                                                                    mfit,
                                                                    cbind(
                                                                        lower = compL,
                                                                        upper = compU
                                                                    )
                                                                ),
                                                                mean = est
                                                            )
                                                            dimnames(coef.mat) <- list(
                                                                levels(dat$y),
                                                                c("lower", "upper", "mean")
                                                            )
                                                            coef.mat
                                                        }
                                                    } else {
                                                        fit <- lm(x ~ y - 1, data = dat)
                                                        est <- predict(fit,
                                                            newdata = data.frame(
                                                                y = levels(dat$y),
                                                                stringsAsFactors = TRUE
                                                            )
                                                        )
                                                        ses <- iNZightMR::seIndepSes(summary(fit)$coef[, 2])
                                                        mfit <- suppressWarnings(
                                                            iNZightMR::moecalc(ses, est = est, base = FALSE)
                                                        )
                                                        mat <- cbind(
                                                            with(
                                                                mfit,
                                                                cbind(lower = compL, upper = compU)
                                                            ),
                                                            mean = est
                                                        )
                                                        rownames(mat) <- levels(dat$y)
                                                        mat
                                                    }
                                                }
                                            }
                                        }
                                    }
                                )
                            }
                        )
                    },
                    "median" = {
                        lapply(
                            inf.type,
                            function(type) {
                                switch(type,
                                    "conf" = {
                                        if (bs) {
                                            if (svy) {
                                                NULL
                                            } else {
                                                b <- boot(dat,
                                                    strata = dat$y,
                                                    function(d, f) {
                                                        tapply(d[f, 1], d[f, 2], quantile,
                                                            probs = 0.5,
                                                            na.rm = TRUE
                                                        )
                                                    },
                                                    R = 2 * opts$n.boot
                                                )
                                                ci <- cbind(
                                                    t(
                                                        apply(b$t, 2, quantile,
                                                            probs = c(1 - alpha, alpha),
                                                            na.rm = TRUE
                                                        )
                                                    ),
                                                    colMeans(b$t)
                                                )
                                                dimnames(ci) <- list(
                                                    levels(dat$y),
                                                    c("lower", "upper", "mean")
                                                )
                                                ci
                                            }
                                        } else {
                                            if (svy) {
                                                NULL
                                            } else {
                                                ## YEAR 12 INTERVALS
                                                # iqr <-

                                                n <- tapply(dat$x, dat$y, function(z) sum(!is.na(z)))
                                                n <- ifelse(n < 2, NA, n)
                                                wd <- 1.5 * # qt(0.975, df = n - 1) *
                                                    tapply(
                                                        dat$x, dat$y,
                                                        function(z) {
                                                            diff(quantile(z, c(0.25, 0.75), na.rm = TRUE))
                                                        }
                                                    ) / sqrt(n)
                                                mn <- tapply(dat$x, dat$y, median, na.rm = TRUE)
                                                cbind(lower = mn - wd, upper = mn + wd, mean = mn)
                                            }
                                        }
                                    },
                                    "comp" = {
                                        if (bs) {
                                            if (svy) {
                                                NULL
                                            } else {
                                                b <- boot(dat,
                                                    strata = dat$y,
                                                    function(d, f) {
                                                        tapply(d[f, 1], d[f, 2], median, na.rm = TRUE)
                                                    },
                                                    R = opts$n.boot
                                                )
                                                cov <- cov(b$t)
                                                ses <- suppressWarnings(iNZightMR::seCovs(cov))
                                                ci <- suppressWarnings(
                                                    iNZightMR::moecalc(ses,
                                                        est = tapply(dat$x, dat$y, median, na.rm = TRUE)
                                                    )
                                                )
                                                cbind(lower = ci$compL, upper = ci$compU)
                                            }
                                        } else {
                                            if (svy) {
                                                NULL
                                            } else {
                                                n <- tapply(
                                                    dat$x, dat$y,
                                                    function(z) sum(!is.na(z))
                                                )
                                                wd <- 1.5 *
                                                    tapply(
                                                        dat$x, dat$y,
                                                        function(z) {
                                                            abs(diff(quantile(z, c(0.25, 0.75), na.rm = TRUE)))
                                                        }
                                                    ) / sqrt(n)
                                                mn <- tapply(dat$x, dat$y, quantile,
                                                    probs = 0.5,
                                                    na.rm = TRUE
                                                )
                                                cbind(lower = mn - wd, upper = mn + wd)
                                            }
                                        }
                                    }
                                )
                            }
                        )
                    },
                    "iqr" = {
                        list(
                            conf =
                                if ("conf" %in% inf.type & bs) {
                                    if (svy) {
                                        NULL
                                    } else {
                                        b <- boot(dat,
                                            strata = dat$y,
                                            function(d, f) {
                                                tapply(
                                                    d[f, 1], d[f, 2],
                                                    function(x) {
                                                        diff(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE))
                                                    }
                                                )
                                            },
                                            R = 2 * opts$n.boot
                                        )
                                        iqr <- cbind(
                                            t(
                                                apply(b$t, 2, quantile,
                                                    probs = c(1 - alpha, alpha),
                                                    na.rm = TRUE
                                                )
                                            ),
                                            colMeans(b$t)
                                        )
                                        dimnames(iqr) <- list(
                                            levels(dat$y),
                                            c("lower", "upper", "mean")
                                        )
                                        iqr
                                    }
                                } else {
                                    NULL
                                },
                            comp = NULL
                        )
                    }
                )
                names(result) <- inf.type
                result
            }
        )
        names(result.list) <- inf.par
        result.list
    } else {
        return(NULL)
    }

    attr(result.list, "bootstrap") <- bs
    attr(result.list, "ci.width") <- opts$ci.width
    result.list
}
