drawAxes <- function(x, which = "x", main = TRUE, label = TRUE, opts,
                     sub = 0, heightOnly = FALSE,
                     layout.only = FALSE, pos = NULL) {
    ## inzight has two basic axis types - numeric, and categorical
    fun <- ifelse(is.numeric(x), .numericAxis, .categoricalAxis)
    fun(x, which, main, label, opts, sub, heightOnly, layout.only, pos)
}

.numericAxis <- function(x, which = "x", main = TRUE, label = TRUE, opts,
                         sub = 0, heightOnly = FALSE,
                         layout.only = FALSE, pos = NULL) {
    x <- transform_axes(x, which, opts, label)
    at <- x$at
    labs <- x$labs
    if (!is.logical(labs)) labs <- format(labs)

    switch(which,
        "x" = {
            if (main) {
                grid.xaxis(
                    gp = gpar(cex = opts$cex.axis),
                    main = main,
                    at = at,
                    label = labs,
                    name = paste(
                        paste0("inz-xaxis-", pos), opts$rowNum, opts$colNum,
                        sep = "."
                    )
                )
            } else {
                xlim <- current.viewport()$xscale
                pushViewport(viewport(
                    x = 0.5, y = 1, height = unit(sub, "in"),
                    just = "bottom", xscale = xlim
                ))
                grid.xaxis(
                    gp = gpar(cex = opts$cex.axis),
                    at = at,
                    label = labs,
                    main = FALSE,
                    name = paste("inz-xaxis-top", opts$rowNum, opts$colNum,
                        sep = "."
                    )
                )
                upViewport()
            }
        },
        "y" = {
            yax <- yaxisGrob(
                gp = gpar(cex = opts$cex.axis),
                main = main,
                at = at,
                label = labs,
                name = paste(
                    paste0("inz-yaxis-", pos), opts$rowNum, opts$colNum,
                    sep = "."
                )
            )
            if (label) {
                yax <- editGrob(
                    yax,
                    edits = gEdit(
                        "labels",
                        rot = ifelse(main, 90, 270),
                        hjust = 0.5,
                        vjust = ifelse(main, 0, -0.5)
                    )
                )
            }
            grid.draw(yax)
        }
    )
}

.categoricalAxis <- function(x, which = "x", main = TRUE, label = TRUE, opts,
                             sub = 0, heightOnly = FALSE,
                             layout.only = FALSE, pos = NULL) {
    if (is.null(opts$ZOOM)) {
        x.lev <- levels(x)
    } else {
        ZOOM <- opts$ZOOM
        ww <- ZOOM[1]:(sum(ZOOM) - 1)
        nl <- length(levels(x))
        ww <- ww - nl * (ww > nl)
        x.lev <- levels(x)[ww]
    }

    switch(which,
        "x" = {
            rot <- opts$rot
            labText <- textGrob(
                x.lev,
                x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                y = if (rot) unit(-0.5, "mm") else unit(-1, "lines"),
                just = if (rot) c("right", "top") else "center",
                rot = ifelse(rot, 30, 0),
                gp = gpar(cex = opts$cex.axis * ifelse(rot, 0.8, 1)),
                name = "inz-labelText"
            ) # label is important!
            wm <- which.max(nchar(as.character(x.lev)))
            tt <- textGrob(levels(x)[wm])
            # save label widths
            labwid <- convertWidth(grobWidth(tt), "mm", valueOnly = TRUE)

            if (heightOnly) {
                return(grobHeight(labText))
            } else {
                grid.draw(labText)
            }
        },
        "y" = {
            if (!is.null(x) & !layout.only) {
                labels <- levels(x)
                Nlab <- length(labels)
                for (i in 1:Nlab) {
                    seekViewport(paste0("VP:plotregion-", i))
                    grid.text(
                        labels[i],
                        x = unit(-0.5, "lines"),
                        just = "right", gp = gpar(cex = opts$cex.axis)
                    )
                    upViewport()
                }
            }
        }
    )
}

addGrid <- function(x = FALSE, y = FALSE, gen, opts) {
    if (!opts$grid.lines) {
        return()
    }
    if (!any(x, y)) {
        return()
    }

    col.grid <- opts$col.grid
    if (col.grid == "default") {
        if (any(col2rgb(opts$bg) <= 230)) {
            col.grid <- "#00000020"
        } else {
            col.grid <- "#ffffff"
        }
    }

    if (x) {
        at.x <- pretty(gen$LIM[1:2])
        at.X <- rep(at.x, each = 2)
        at.Y <- rep(current.viewport()$yscale, length(at.x))
        grid.polyline(
            at.X, at.Y,
            id.lengths = rep(2, length(at.X) / 2),
            default.units = "native",
            gp = gpar(col = col.grid, lwd = 1),
            name = paste("inz-x-grid", opts$rowNum, opts$colNum, sep = ".")
        )
    }
    if (y) {
        at.y <- pretty(gen$LIM[3:4])
        at.Y <- rep(at.y, each = 2)
        at.X <- rep(current.viewport()$xscale, length(at.y))
        grid.polyline(
            at.X, at.Y,
            id.lengths = rep(2, length(at.Y) / 2),
            default.units = "native",
            gp = gpar(col = col.grid, lwd = 1),
            name = paste("inz-y-grid", opts$rowNum, opts$colNum, sep = ".")
        )
    }
}

transform_axes <- function(x, which, opts, label, adjust.vp = TRUE) {
    xt <- x
    breaks <- NULL

    ## put X into the correct format ...
    if (!is.null(opts$transform[[which]])) {
        ## we need to apply a transformation
        switch(opts$transform[[which]],
            "datetime" = {
                ## format labels for datetime
                xt <- as.POSIXct(x,
                    origin = "1970-01-01",
                    tz = opts$transform$extra[[which]]$tz
                )
            },
            "date" = {
                xt <- as.Date(x, origin = "1970-01-01")
            },
            "time" = {
                xt <- chron::chron(times. = x)
                # xt <- hms::hms(x)
                # xt <- as.POSIXct(xt)
                # breaks <- scales::breaks_pretty()(xt)
                # names(breaks) <- scales::label_time()(breaks)
            },
            "log" = {
                breaks <- scales::log_trans()$breaks(exp(x))
                breaks <- log(breaks)
                if (all(round(breaks) == breaks)) {
                    names(breaks) <- paste0("e^", breaks)
                } else {
                    names(breaks) <- round(exp(breaks))
                }
            },
            "log10" = {
                breaks <- scales::log10_trans()$breaks(10^x)
                names(breaks) <- breaks
                breaks <- log10(breaks)
            },
            "bar_percentage" = {
                breaks <- scales::pretty_breaks()(xt)
                names(breaks) <- breaks * 100
            },
            "bar_counts" = {
                # breaks <- scales::pretty_breaks()(xt * opts$bar.nmax)
                # names(breaks) <- breaks
                # print(opts$bar.nmax)
                # breaks <- breaks / opts$bar.nmax * 100
            },
            {
                warning(sprintf(
                    "Unsupported transformation `%s`",
                    opts$transform[[which]]
                ))
                xt <- x
            }
        )
    }

    if (is.null(breaks)) {
        breaks <- scales::breaks_pretty()(xt)
    }
    if (adjust.vp) {
        xl <- current.viewport()[[switch(which,
            "x" = "xscale",
            y = "yscale"
        )]]
        breaks <- breaks[breaks > xl[1] & breaks < xl[2]]
        if (length(breaks) == 0) {
            breaks <- seq(min(xl), max(xl), by = 1)
        }
    }
    at <- as.numeric(breaks)
    labs <- FALSE
    if (label) labs <- if (!is.null(names(breaks))) names(breaks) else at

    list(at = at, labs = labs)
}
