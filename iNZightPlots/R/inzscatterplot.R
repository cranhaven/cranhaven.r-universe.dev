#' @export
create.inz.scatterplot <- function(obj, ...) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    features <- opts$plot.features


    if (opts$join) {
        df <- df
    } else if (!is.null(features$order.first)) {
        ## can specify value = -1 or order = numeric() to ignore reordering
        if (length(features$order.first) > 0 && all(features$order.first > 0)) {
            ord <- (1:nrow(df))
            wi <- which(ord %in% features$order.first)
            ord <- c(ord[-wi], ord[wi])
            df <- df[ord, ]
        }
    } else {
        df <- df[sample(nrow(df)), ]
    }

    if (xattr$class == "inz.survey") {
        design <- df
        df <- as.data.frame(
            cbind(df$variables, weights = get_weights(df)),
            stringsAsFactors = TRUE
        )
    }

    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- apply(df[, v %in% c("x", "y")], 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]
    ## order should match the length of non-missing:
    ORDER <- as.numeric(rownames(df))

    # --- look through inzpar for settings

    ## The plotting symbol:
    if ("symbolby" %in% v) {
        pch <- (21:25)[as.numeric(df$symbolby)]
        pch[is.na(pch)] <- 3
    } else {
        pch <- rep(ifelse(opts$pch == 1, 21, opts$pch), nrow(df))
    }
    if (opts$fill.pt == "transparent" & opts$alpha < 1) {
        opts$fill.pt <- "fill"
    }

    ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
    # size of points
    resize <- TRUE
    if ("freq" %in% v) {
        propsize <- df$freq / xattr$max.freq * 4 + 0.5
    } else if ("weights" %in% v) {
        if (length(unique(df$weights)) == 1) {
            resize <- FALSE
            propsize <- opts$cex.pt
        } else {
            propsize <- df$weights / xattr$max.weight * 2 + 0.5
        }
    } else if ("sizeby" %in% v) {
        propsize <- df$.cex
    } else {
        propsize <- 1 # opts$cex.pt
        resize <- FALSE
    }

    propsize <- propsize * opts$cex.pt

    pch[is.na(propsize)] <- ifelse(pch[is.na(propsize)] == 3, 8, 4)
    propsize[is.na(propsize)] <- 0.6

    ext.ids <- NULL
    if ("extreme.label" %in% v) {
        eLab <- as.character(df$extreme.label)
        m <- cbind(df$x, df$y)
        if (sum(apply(m, 1, function(k) all(!is.na(k)))) > 0) {
            dist <- mahalanobis(
                m,
                colMeans(m, na.rm = TRUE),
                cov(m, use = "complete.obs")
            )
            o <- order(dist, decreasing = TRUE)
            text.labels <- eLab
            ext.ids <- o[1:min(sum(!is.na(dist)), xattr$nextreme)]
            if (!is.null(df$locate.same.level)) {
                loc.lvls <- as.character(unique(df$locate.same.level[ext.ids]))
                ext.ids <- which(df$locate.same.level %in% loc.lvls)
            }
            text.labels[-ext.ids] <- ""
            ext.ids <- df$pointIDs[ext.ids]
        } else {
            text.labels <- character(length(eLab))
        }
    } else {
        text.labels <- as.character(df$locate)
    }

    # Combine everything together into a classed list which will have a `plot` method
    out <- list(
        x = df$x,
        y = df$y,
        colby = df$colby,
        propsize = propsize,
        pch = pch,
        fill.pt = opts$fill.pt,
        n.missing = n.missing,
        nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
        nasize = if ("sizeby" %in% v) any(is.na(df$sizeby)) else FALSE,
        xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
        ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf),
        trend = opts$trend,
        trend.by = opts$trend.by,
        smooth = opts$trend,
        n.boot = opts$n.boot,
        text.labels = text.labels,
        extreme.ids = ext.ids,
        point.order = ORDER,
        ci.width = opts$ci.width
    )

    if (xattr$class == "inz.survey") {
        out$svy <- obj$df
    }
    if ("highlight" %in% colnames(df)) {
        out$highlight <- as.logical(df$highlight)
    }

    class(out) <- "inzscatter"

    out
}

plot.inzscatter <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    addGrid(x = TRUE, y = TRUE, gen = gen, opts = opts)

    if (length(obj$x) == 0) {
        return()
    }

    ## Jitter on x and y
    if ("x" %in% strsplit(opts$jitter, "")[[1]]) {
        obj$x <- jitter(obj$x)
    }
    if ("y" %in% strsplit(opts$jitter, "")[[1]]) {
        obj$y <- jitter(obj$y)
    }

    ptCols <- colourPoints(obj$colby, col.args, opts)

    ## If locating points:
    locating <- FALSE
    if ("text.labels" %in% names(obj)) {
        if (sum(obj$text.labels != "", na.rm = TRUE) > 0) {
            locating <- TRUE

            labID <- which(obj$text.labels != "")

            if (!is.null(col.args$locate.col)) {
                newCol <- col.args$locate.col

                if (length(ptCols) == 1) {
                    ptCols <- rep(ptCols, length(obj$x))
                }
                ptCols[labID] <- newCol

                ## make them solid:
                obj$pch[labID] <- 19
            }
        }
    }


    NotInView <-
        obj$x < min(xlim) |
            obj$x > max(xlim) |
            obj$y < min(ylim) |
            obj$y > max(ylim)

    obj$pch[NotInView] <- NA
    ptOrdering <-
        grid.points(obj$x, obj$y,
            pch = obj$pch,
            gp = gpar(
                col = ptCols,
                cex = obj$propsize,
                lwd = opts$lwd.pt,
                alpha = opts$alpha,
                fill = if (obj$fill.pt == "fill") ptCols else obj$fill.pt
            ),
            name = paste("inz-SCATTERPOINTS", opts$rowNum, opts$colNum, sep = ".")
        )

    ## Highlighting:
    if (!is.null(obj$highlight) & length(ptCols) > 1) {
        hl <- as.logical(obj$highlight)
        if (sum(hl) > 0) {
            hcol <-
                if (opts$highlight.col == "shade") {
                    shade(ptCols[hl], 0.6)
                } else {
                    opts$highlight.col
                }

            grid.points(obj$x[hl], obj$y[hl],
                pch = 19,
                gp = gpar(
                    col = hcol,
                    cex = obj$propsize * 1.4,
                    lwd = opts$lwd.pt
                )
            )

            grid.points(obj$x[hl], obj$y[hl],
                pch = 19,
                gp = gpar(
                    col = ptCols[hl],
                    cex = obj$propsize,
                    lwd = opts$lwd.pt
                )
            )
        }
    }


    if (locating) {
        labs <- obj$text.labels[labID]
        labx <- unit(obj$x[labID], "native")
        laby <- unit(obj$y[labID], "native") +
            (grobHeight(textGrob("0", gp = gpar(cex = obj$propsize))) +
                grobHeight(textGrob("0", gp = gpar(cex = 0.6)))) *
                ifelse(obj$y[labID] < mean(ylim), 1, -1)

        grid.text(labs, labx, laby, gp = gpar(cex = 0.6))
    }

    # Connect by dots if they want it ...
    if (opts$join) {
        if (length(unique(obj$colby)) == 1 | !opts$lines.by) {
            grid.lines(obj$x, obj$y,
                default.units = "native",
                gp = gpar(
                    lwd = opts$lwd,
                    lty = opts$lty,
                    col = opts$col.line
                )
            )
        } else {
            byy <- as.factor(obj$colby) # pseudo-by-variable
            xtmp <- lapply(
                levels(byy),
                function(c) obj$x[obj$colby == c]
            )
            ytmp <- lapply(
                levels(byy),
                function(c) obj$y[obj$colby == c]
            )

            for (b in 1:length(levels(byy))) {
                grid.lines(xtmp[[b]], ytmp[[b]],
                    default.units = "native",
                    gp = gpar(
                        lwd = opts$lwd,
                        lty = opts$lty,
                        col = col.args$f.cols[b]
                    )
                )
            }
        }
    }

    ## add rugs --- these only make sense for a scatter plot
    if ("x" %in% strsplit(opts$rug, "")[[1]]) {
        # Add marks on the x-axis at the location of every data point
        grid.polyline(
            x = unit(rep(obj$x, each = 2), "native"),
            y = unit(rep(c(0, 0.5), length(obj$x)), "char"),
            id.lengths = rep(2, length(obj$x))
        )
    }
    if ("y" %in% strsplit(opts$rug, "")[[1]]) {
        # Same, but for the y-axis
        grid.polyline(
            y = unit(rep(obj$y, each = 2), "native"),
            x = unit(rep(c(0, 0.5), length(obj$y)), "char"),
            id.lengths = rep(2, length(obj$y))
        )
    }

    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx,
            default.units = "native",
            gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE),
            name = paste("inz-line-LOE", opts$rowNum, opts$colNum, sep = ".")
        )
    }

    if (opts$trend.by) {
        if (is.null(col.args$f.cols)) {
            opts$trend.by <- FALSE
        }
    }

    # Add additional features to plot:
    addXYsmoother(obj, opts, col.args, xlim, ylim)
    addXYtrend(obj, opts, col.args, xlim, ylim)

    invisible(NULL)
}
