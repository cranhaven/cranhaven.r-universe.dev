#' @export
create.inz.hexplot <- function(obj, ...) {
    # make a plot using hexagonal binning

    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

    if (xattr$class == "inz.survey") {
        df <- df$variables
    }

    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- apply(df[, v %in% c("x", "y")], 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]

    xbins <- opts$hex.bins

    ## hexbin returns an S4 object, so need to use the @ operator
    if (nrow(df) == 0) {
        hb <- NULL
    } else {
        hb <- hexbin(df$x, df$y, IDs = TRUE, xbins = xbins)

        cellid <- hb@cID
        ## now manipulate the counts with the weight variable
        W <- switch(xattr$class,
            "inz.freq" = df$freq,
            "inz.survey" = get_weights(obj$df)[!missing],
            "inz.simple" = rep(1, nrow(df))
        )

        hb@count <- as.vector(tapply(W, cellid, sum))
        hb@xcm <- as.vector(
            tapply(
                1:length(df$x), cellid,
                function(i) weighted.mean(df$x[i], W[i])
            )
        )
        hb@ycm <- as.vector(
            tapply(
                1:length(df$y), cellid,
                function(i) weighted.mean(df$y[i], W[i])
            )
        )
    }

    xlim <- if (nrow(df) > 0) hb@xbnds else c(-Inf, Inf)
    ylim <- if (nrow(df) > 0) hb@ybnds else c(-Inf, Inf)

    out <- list(
        hex = hb,
        n.missing = n.missing,
        svy = obj$df,
        colby = if ("colby" %in% v) convert.to.factor(df$colby) else NULL,
        nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
        xlim = xlim,
        ylim = ylim,
        x = df$x,
        y = df$y,
        n.bins = xbins,
        trend = opts$trend,
        trend.by = opts$trend.by,
        smooth = opts$trend,
        n.boot = opts$n.boot,
        ci.width = opts$ci.width
    )
    class(out) <- "inzhex"

    out
}

plot.inzhex <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    addGrid(x = TRUE, y = TRUE, gen = gen, opts = opts)

    if (is.null(obj$hex)) {
        return()
    }

    try_hextri <- function() {
        if (requireNamespace("hextri", quietly = TRUE)) {
            return(TRUE)
        }
        warning("Please install the `hextri` package to use colby with hexbins.")
        FALSE
    }
    if (!is.null(obj$colby) && try_hextri()) {
        if (any(is.na(obj$colby))) {
            levels(obj$colby) <- c(levels(obj$colby), "missing")
            obj$colby[is.na(obj$colby)] <- "missing"
            colours <- c(col.args$f.cols, col.args$missing)
        } else {
            colours <- col.args$f.cols
        }
        hextri::panel.hextri(
            x = obj$x,
            y = obj$y,
            groups = factor(levels(obj$colby), levels = levels(obj$colby)),
            subscripts = as.numeric(obj$colby),
            colours = colours,
            nbins = obj$n.bins,
            style = opts$hex.style,
            diffuse = opts$hex.diffuse,
            shape =
                convertHeight(current.viewport()$height, "in", TRUE) /
                    convertWidth(current.viewport()$width, "in", TRUE)
        )
    } else {
        if (opts$hex.style == "alpha") {
            style <- "colorscale"
            colRGB <- col2rgb(opts$col.pt[1]) / 255
            colramp <- function(n) {
                rgb(
                    colRGB[1], colRGB[2], colRGB[3],
                    seq(0, 1, length = n + 1)[-1]
                )
            }
        } else {
            style <- "centroids"
            colramp <- NULL
        }
        grid.hexagons(
            obj$hex,
            style = style,
            maxcnt = gen$maxcount,
            border = 0, # if (style == "size") opts$col.pt else FALSE,
            pen = opts$col.pt[1],
            colramp = colramp
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
            name = paste("inz-loe", opts$rowNum, opts$colNum, sep = ".")
        )
    }

    addXYsmoother(obj, opts, col.args, xlim, ylim)
    addXYtrend(obj, opts, col.args, xlim, ylim)

    invisible(NULL)
}
