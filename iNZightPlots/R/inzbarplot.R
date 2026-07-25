#' @export
create.inz.barplot <- function(obj, ...) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    counts <- opts$bar.counts
    relative.width <- opts$bar.relative.width
    xattr <- obj$xattr

    ZOOM <- xattr$zoom

    if (counts) {
        opts$inference.type <- NULL
    }
    inf.type <- opts$inference.type

    inf.par <- "proportion"
    bs <- opts$bs.inference

    if (xattr$class == "inz.survey") {
        des <- df
        df <- df$variables
    }

    ynull <- !"y" %in% colnames(df)

    # first need to remove missing values
    missing <- is.na(df$x)
    if ("y" %in% colnames(df)) {
        ## need to save these before we remove missing ...
        y.levels <- levels(df$y)
        missing <- missing | is.na(df$y)
    }

    n.missing <- sum(missing)
    df <- df[!missing, , drop = FALSE]

    svy <- switch(xattr$class,
        "inz.survey" = des,
        "inz.freq" = ,
        "inz.simple" = NULL
    )

    SEG <- FALSE
    if (ynull) {
        if (nrow(df) == 0) {
            tab <- table(df$x)
            phat <- matrix(tab, nrow = 1)
        } else if (!is.null(svy)) {
            tab <- svytable(~x, design = svy)
            phat <- matrix(svymean(~x, design = svy, na.rm = TRUE), nrow = 1)
        } else if (!is.null(df$freq)) {
            tab <- xtabs(df$freq ~ df$x)
            phat <- matrix(tab / sum(tab), nrow = 1)
        } else {
            tab <- table(df$x)
            phat <- matrix(tab / sum(tab), nrow = 1)
        }

        widths <- rep(1, length(tab))
        edges <- c(0, 1)

        ## colby: (segmented bar plot)
        SEG <- "colby" %in% colnames(df)
        if (SEG & !is.factor(df$colby)) {
            SEG <- FALSE
        }

        if (SEG) {
            tab2 <-
                if (!is.null(svy)) {
                    svytable(~ colby + x, design = svy)
                } else if (!is.null(df$freq)) {
                    xtabs(df$freq ~ df$colby + df$x)
                } else {
                    table(df$colby, df$x)
                }
            p2 <- sweep(tab2, 2, colSums(tab2), "/")
        }
    } else {
        if (nrow(df) == 0) {
            tab <- table(df$x, df$y)
            phat <- sweep(tab, 1, 1, "*")
            nn <- rowSums(tab)
        } else if (!is.null(svy)) {
            tab <- svytable(~ y + x, design = svy)
            phat <- svyby(~x, by = ~y, svy, FUN = svymean, drop.empty.groups = FALSE, na.rm = TRUE)
            phat <- phat[, 1 + 1:ncol(tab)]
            nn <- rowSums(tab)
        } else if (!is.null(df$freq)) {
            tab <- xtabs(df$freq ~ df$y + df$x)
            nn <- rowSums(tab)
            phat <- sweep(tab, 1, nn, "/")
        } else {
            tab <- table(df$y, df$x)
            nn <- rowSums(tab)
            phat <- sweep(tab, 1, nn, "/")
        }

        widths <-
            if (counts || !relative.width) {
                rep(1 / length(nn), length(nn))
            } else {
                nn / sum(nn)
            }
        edges <- c(0, cumsum(widths))
    }

    ## Cannot have inference on segmented plot (too complicated for now)
    inflist <-
        if (!SEG && !counts) {
            barinference(obj, tab, phat, counts)
        } else {
            NULL
        }

    ## y-axis limits are based on opts$bar.counts
    # true: use tab
    # false: use phat
    ymax <-
        if (counts) {
            max(tab, na.rm = TRUE)
        } else if (all(is.na(phat))) {
            0
        } else {
            max(phat, na.rm = TRUE)
        }
    if (!is.null(ZOOM)) {
        if (ZOOM[1] <= ncol(phat)) {
            ww <- ZOOM[1]:(sum(ZOOM) - 1)
            ww <- ww - ncol(phat) * (ww > ncol(phat))

            phat <- phat[, ww, drop = FALSE]
            if (ynull) {
                tab <- tab[ww]
                widths <- widths[ww]
            } else {
                tab <- tab[, ww, drop = FALSE]
            }
        }
    }



    out <- list(
        phat = phat,
        tab = if (!ynull) as.matrix(tab) else as.matrix(t(tab)),
        widths = widths,
        edges = edges,
        nx = ncol(phat),
        ntotal = sum(tab),
        full.height = opts$full.height, inference.info = inflist,
        xlim = c(0, if (ynull) length(tab) else ncol(tab)),
        ylim = c(
            0,
            max(
                ymax,
                if (!is.null(inflist)) attr(inflist, "max"),
                na.rm = TRUE
            )
        )
    )

    if (SEG) out$p.colby <- p2[nrow(p2):1, ]
    if (!is.null(ZOOM)) out$zoom.index <- ww

    class(out) <- "inzbar"

    out
}

plot.inzbar <- function(obj, gen) {
    opts <- gen$opts
    counts <- opts$bar.counts
    p <- if (counts) obj$tab else obj$phat
    nx <- obj$nx

    addGrid(y = TRUE, gen = gen, opts = opts)

    inflist <- obj$inference.info

    if (SEG <- !is.null(obj$p.colby)) {
        seg.cols <- gen$col.args$f.cols
    }

    edges <- rep(obj$edges * 0.9 + 0.05, each = 4)
    edges <- edges[3:(length(edges) - 2)]
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))

    if (SEG) {
        xx <- rep(xx, length(seg.cols))
        tops <- apply(p, 2, function(x) rbind(0, x, x, 0))

        yy <- rep(tops, length(seg.cols))
        ps <- rep(c(t(obj$p.colby)), each = 4)
        pT <- rep(c(t(apply(obj$p.colby, 2, cumsum))), each = 4)
        yy <- yy * pT

        ## reverse the order, so the short ones are drawn last!
        id <- rev(rep(1:prod(dim(obj$p.colby)), each = 4))
        colz <- rep(seg.cols, each = nx)

        grid.polygon(
            unit(xx, "native"), unit(yy, "native"),
            id = id,
            gp = gpar(
                fill = colz,
                col = "transparent",
                lwd = 0
            ),
            name = paste("inz-bar-rev", opts$rowNum, opts$colNum, sep = ".")
        )

        ## separating lines
        mat <- apply(sweep(obj$p.colby, 2, tops[2, ], "*"), 2, cumsum)
        mat <- mat[-nrow(mat), , drop = FALSE] # drop the last one

        yl <- rep(c(mat), each = 2)
        xl <- rep(edges[2:3], length = length(yl)) +
            rep(1:nx - 1, each = 2 * nrow(mat))
        id <- rep(1:length(c(mat)), each = 2)

        grid.polyline(xl, yl,
            default.units = "native",
            id = id,
            gp = gpar(col = opts$bar.col, lwd = opts$bar.lwd),
            name = paste("inz-bar-line", opts$rowNum, opts$colNum, sep = ".")
        )
    }

    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))
    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    yy <- c(tops)

    id <- rep(1:prod(dim(p)), each = 4)
    colz <-
        if (is.null(gen$col.args$b.cols)) {
            opts$bar.fill
        } else {
            rep(gen$col.args$b.cols, nx)
        }

    grid.polygon(unit(xx, "native"), unit(yy, "native"),
        id = id,
        gp =
            gpar(
                fill = if (SEG) "transparent" else colz,
                col = opts$bar.col,
                lwd = opts$bar.lwd
            ),
        name = paste("inz-BAR", opts$rowNum, opts$colNum, sep = ".")
    )

    center <- apply(
        matrix(xx, ncol = 4, byrow = TRUE), 1,
        function(x) x[2] + (x[3] - x[2]) / 2
    )
    bounds <- apply(matrix(xx, ncol = 4, byrow = TRUE), 1, function(x) x[2:3])

    if (!is.null(inflist)) {
        addBarInference(inflist, center, opts, obj$zoom.index)
        if (!is.null(inflist$comp)) {
            addBarCompLines(inflist$comp, bounds, p, opts, obj$zoom.index)
        }
    }
}

barinference <- function(obj, tab, phat, counts) {
    ## obj: list of data broken down by subsets
    ## opts: various options (inzpar)

    opts <- obj$opts
    xattr <- obj$xattr
    inf.par <- "proportion"
    inf.type <- opts$inference.type

    bs <- opts$bs.inference
    dat <- obj$df

    if (is.null(inf.type)) {
        return(NULL)
    }

    twoway <- length(dim(tab)) == 2 # two way comparison (two factors ...)
    svy <- obj$xattr$class == "inz.survey"

    if (length(dim(tab)) == 1) {
        twoway <- FALSE
        tab <- t(tab)
        phat <- t(phat)
    } else {
        twoway <- TRUE
    }

    # CI interval width:
    alpha <- 1 - (1 - opts$ci.width) / 2

    lapply(inf.type, function(type) {
        switch(type,
            "conf" = {
                if (bs) {
                    if (svy) {
                        NULL
                    } else {
                        if (twoway) {
                            ## For now, we will just all over and not return
                            ## intervals
                            ## IN FUTURE: might want to bootstrap another way?
                            inf <- try(
                                {
                                    n <- rowSums(tab)
                                    b <- boot(dat,
                                        function(d, f) {
                                            tt <- t(table(d[f, 1], d[f, 2]))
                                            sweep(tt, 1, n, "/")
                                        },
                                        R = 1000
                                    )
                                    cis <- apply(
                                        b$t, 2,
                                        function(x) {
                                            c(
                                                quantile(x, probs = c(1 - alpha, alpha)),
                                                mean(x)
                                            )
                                        }
                                    )
                                },
                                silent = TRUE
                            )
                            if (inherits(inf, "try-error")) {
                                NULL
                            } else {
                                list(
                                    lower =
                                        matrix(cis[1, ],
                                            nrow = nrow(tab),
                                            byrow = FALSE
                                        ),
                                    upper =
                                        matrix(cis[2, ],
                                            nrow = nrow(tab),
                                            byrow = FALSE
                                        ),
                                    estimate =
                                        matrix(cis[3, ],
                                            nrow = nrow(tab),
                                            byrow = FALSE
                                        )
                                )
                            }
                        } else {
                            n <- sum(tab)
                            if (n == 0) {
                                return(NULL)
                            }
                            b <- boot(dat,
                                function(d, f) table(d[f, 1]) / n,
                                R = opts$n.boot
                            )
                            cis <- apply(
                                b$t, 2,
                                function(x) {
                                    c(
                                        quantile(x, probs = c(1 - alpha, alpha)),
                                        mean(x)
                                    )
                                }
                            )
                            list(
                                lower = cis[1, , drop = FALSE],
                                upper = cis[2, , drop = FALSE],
                                estimate = cis[3, , drop = FALSE]
                            )
                        }
                    }
                } else {
                    if (svy) {
                        if (twoway) {
                            fit <- svyby(~x,
                                by = ~y, obj$df,
                                FUN = svymean,
                                drop.empty.groups = FALSE,
                                na.rm = TRUE
                            )
                            est <- coef(fit)
                            ci <- confint(fit, level = opts$ci.width)
                            nc <- length(levels(obj$df$variables$x))
                            list(
                                lower = matrix(ci[, 1], ncol = nc),
                                upper = matrix(ci[, 2], ncol = nc),
                                estimate = matrix(est, ncol = nc)
                            )
                        } else {
                            ci <- t(
                                confint(
                                    svymean(~x, obj$df, na.rm = TRUE),
                                    level = opts$ci.width
                                )
                            )
                            list(
                                lower = ci[1, , drop = FALSE],
                                upper = ci[2, , drop = FALSE],
                                estimate = t(phat)
                            )
                        }
                    } else {
                        ## Standard confidence interval:
                        size <- t(
                            apply(
                                tab, 1,
                                function(x) {
                                    n <- sum(x)
                                    p <- ifelse(x >= opts$min.count, x / n, NA)
                                    se <- sqrt(p * (1 - p) / n)
                                    se * qnorm(alpha)
                                }
                            )
                        )
                        if (!twoway) phat <- t(phat)
                        list(
                            lower = phat - size,
                            upper = phat + size,
                            estimate = phat
                        )
                    }
                }
            },
            "comp" = {
                if (bs) {
                    if (svy) {
                        NULL
                    } else {
                        NULL
                    }
                } else {
                    if (svy) {
                        NULL
                    } else {
                        if (twoway) {
                            ## several ways in which this can fall over;
                            ## rather than testing for each, just try and
                            ## iNZightMR will fail with an error
                            int <- try(
                                {
                                    n <- rowSums(tab)
                                    ## ii - only use rows that have at least 1 count
                                    ## (due to subsetting, can be 0 counts for row)
                                    ii <- n > 0

                                    lapply(1:ncol(tab), function(i) {
                                        if (sum(tab[ii, ] > 0) < 2) {
                                            return(list(compL = NA, compU = NA))
                                        }

                                        suppressWarnings(
                                            iNZightMR::moecalc(
                                                seBinprops(n[ii], phat[ii, i]),
                                                est = phat[ii, i]
                                            )
                                        )
                                    }) -> out

                                    low <- upp <- phat * 0
                                    low[ii, ] <- sapply(out, function(x) x$compL)
                                    upp[ii, ] <- sapply(out, function(x) x$compU)
                                },
                                silent = TRUE
                            )
                            if (inherits(int, "try-error")) {
                                NULL
                            } else {
                                list(lower = low, upper = upp)
                            }
                        } else {
                            if (sum(tab) == 0) {
                                return(NULL)
                            }
                            phat <- c(phat) # don't want matrix
                            res <- with(
                                suppressWarnings(
                                    iNZightMR::moecalc(
                                        seMNprops(sum(tab), phat),
                                        est = phat
                                    )
                                ),
                                list(lower = t(compL), upper = t(compU))
                            )
                            lapply(
                                res,
                                function(r) {
                                    colnames(r) <- colnames(tab)
                                    r[tab < opts$min.count] <- NA
                                    r
                                }
                            )
                        }
                    }
                }
            }
        )
    }) -> result
    names(result) <- inf.type

    # transform counts
    if (counts) {
        # loop over [conf, comp]
        Ns <- if (twoway) rowSums(tab) else sum(tab)
        result <- lapply(
            result,
            function(res) {
                if (is.null(res)) {
                    return(res)
                }
                # loop over [lower, upper, estimate]
                lapply(
                    res,
                    function(r) {
                        sweep(r, 1, Ns, "*")
                    }
                )
            }
        )
    }

    # make everything a matrix
    result <- lapply(
        result,
        function(res) {
            if (is.null(res)) {
                return(res)
            }
            lapply(
                res,
                function(r) {
                    unclass(r)
                }
            )
        }
    )

    attr(result, "bootstrap") <- bs
    attr(result, "max") <- max(
        sapply(
            result,
            function(r) {
                if (is.null(r)) {
                    0
                } else {
                    max(
                        sapply(
                            r,
                            function(x) {
                                if (is.null(x)) {
                                    0
                                } else {
                                    max(c(0, x), na.rm = TRUE)
                                }
                            }
                        ),
                        na.rm = TRUE
                    )
                }
            }
        ),
        na.rm = TRUE
    )
    attr(result, "ci.width") <- opts$ci.width

    result
}
