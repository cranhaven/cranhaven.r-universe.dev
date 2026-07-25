#' @export
create.inz.gridplot <- function(obj, ...) {
    ## The original "grid" style plot for large sample sizes. This will only be used for
    ## simple iid data, and instead hexagonal binning will be used for frequency and
    ## survey data plots.

    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

    if (xattr$class == "inz.survey") {
        warning("The grid plot doesn't account for survey and frequency weighting. Better to use plottype = \"hex\".")
        df <- df$variables
    }

    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- apply(df[, v %in% c("x", "y")], 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]

    ## Because this requires the xlimits to first be calculated, we can only create the
    ## plot AFTER we have done all the plots ...

    makeRects <- function(args, xlim, ylim) {
        df <- args$df
        opts <- args$opts
        xattr <- args$xattr

        # Set up the grid
        # (for now, we will scale the bin size by point size)
        Npt <- min(250, round(opts$scatter.grid.bins)) # / (opts$cex.pt * 2)))

        #  scatter.grid <- matrix(0, nrow = Npt, ncol = Npt)
        xbrk <- seq(xlim[1], xlim[2], length = Npt + 1)
        ybrk <- seq(ylim[1], ylim[2], length = Npt + 1)
        xx <- cut(df$x, xbrk)
        yy <- cut(df$y, ybrk)

        scatter.grid <- as.matrix(table(yy, xx))
        scatter.grid <- scatter.grid[Npt:1, ]

        # If a point has very high density, it dominates. So instead, shade by quantiles.
        nquants <- max(3, length(df$x) / 100)
        br <- unique(quantile(c(scatter.grid), seq(0, 1, length = nquants)))
        which.quant <- as.numeric(cut(c(scatter.grid), breaks = c(-1, br)))

        hcols <- hcl(0, 0, seq(100 * (1 - opts$alpha / 2), 0,
            length = length(br)
        ))
        shade <- matrix(hcols[which.quant], nrow = nrow(scatter.grid))

        is0 <- c(scatter.grid) == 0

        # centers of all grid boxes
        xv <- (rep(1:Npt, each = Npt) - 0.5) / Npt
        yv <- (rep(Npt:1, Npt) - 0.5) / Npt

        # We will attempt to use grid.polygon() to draw all of the
        # grid squares at the same time!
        wx <- 0.5 / Npt
        wy <- 0.5 / Npt
        xmat <- sapply(xv, function(x) x + c(-1, -1, 1, 1) * wx)
        ymat <- sapply(yv, function(y) y + c(-1, 1, 1, -1) * wy)
        id <- matrix(rep((1:Npt^2), each = 4), nrow = 4)

        # Remove zero-count cells:
        xmat <- xmat[, !is0]
        ymat <- ymat[, !is0]
        id <- id[, !is0]
        shade <- shade[!is0]

        list(
            xg = xmat,
            yg = ymat,
            id = id,
            col = shade
        )
    }

    out <- list(
        makeRects = makeRects,
        args = list(df = df, opts = opts, xattr = xattr),
        n.missing = n.missing,
        colby = df$colby,
        nacol = FALSE,
        xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
        ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf),
        x = df$x,
        y = df$y,
        trend = opts$trend,
        trend.by = opts$trend.by,
        smooth = opts$trend,
        n.boot = opts$n.boot
    )
    class(out) <- "inzgrid"

    out
}

plot.inzgrid <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    addGrid(TRUE, TRUE, gen, opts)

    gr <- obj$makeRects(obj$args, xlim, ylim)

    grid.polygon(gr$xg, gr$yg,
        id = gr$id,
        default.units = "npc",
        gp = gpar(fill = gr$col, col = gr$col)
    )

    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx,
            default.units = "native",
            gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE)
        )
    }

    # Add additional features to plot:
    addXYsmoother(obj, opts, col.args, xlim, ylim)
    addXYtrend(obj, opts, col.args, xlim, ylim)

    invisible(NULL)
}
