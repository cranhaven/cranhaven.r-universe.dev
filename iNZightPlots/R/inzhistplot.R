#' @export
create.inz.histplot <- function(obj, ...) {
    create.inz.dotplot(obj, hist = TRUE)
}

plot.inzhist <- function(obj, gen) {
    # First step is to grab stuff:
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    boxplot <- opts$boxplot
    mean_indicator <- FALSE
    if (!is.null(opts$mean_indicator)) {
        if (is.logical(opts$mean_indicator)) {
            mean_indicator <- opts$mean_indicator
        } else {
            mean_indicator <- opts$mean_indicator %in% c("grand", "group")
        }
    }

    addGrid(x = TRUE, gen = gen, opts = opts)

    toplot <- obj$toplot
    boxinfo <- obj$boxinfo
    meaninfo <- obj$meaninfo
    inflist <- obj$inference.info

    nlev <- length(toplot)
    pushViewport(
        viewport(
            layout = grid.layout(nrow = nlev),
            name = "VP:histplot-levels"
        )
    )
    Hgts <- if (boxplot || mean_indicator) c(3, 1) else c(1, 0)
    dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))

    ylim <- c(0, gen$maxcount * 1.05)

    GROUP.names <-
        if (nlev > 1 & opts$internal.labels) {
            names(toplot)
        } else {
            NULL
        }

    for (i in 1:nlev) {
        pp <- toplot[[i]]
        seekViewport("VP:histplot-levels")
        pushViewport(viewport(layout.pos.row = i))
        pushViewport(viewport(layout = dpLayout))

        pushViewport(viewport(layout.pos.row = 2, xscale = xlim))
        if (boxplot) addBoxplot(boxinfo[[i]], opts, i)
        if (mean_indicator) addMean(meaninfo[[i]], opts, i)
        if (!is.null(inflist)) addUnivarInference(inflist, i, opts)
        upViewport()

        pushViewport(
            viewport(
                layout.pos.row = 1,
                xscale = xlim,
                yscale = ylim,
                name = paste0("VP:plotregion-", i)
            )
        )

        if (length(pp$x) > 0) {
            bar.x <- rep(pp$breaks, each = 4)
            bar.x <- bar.x[-c(1:2, length(bar.x) - 0:1)]
            bar.y <- c(sapply(pp$counts, function(y) c(0, y, y, 0)))
            bar.id <- rep(1:length(pp$counts), each = 4)

            grid.polygon(bar.x, bar.y, bar.id,
                default.units = "native",
                gp = gpar(
                    fill = opts$bar.fill,
                    col = opts$bar.col,
                    lwd = opts$bar.lwd
                ),
                name = paste("inz-HISTBAR", opts$rowNum, opts$colNum, i, sep = ".")
            )
        }

        ## Label group
        if (!is.null(GROUP.names)) {
            grid.text(GROUP.names[i],
                x = 0.01,
                y = 0.98,
                just = c("left", "top"),
                gp = gpar(cex = 0.8)
            )
        }
    }

    seekViewport("VP:histplot-levels")
    upViewport()

    if (!is.null(inflist)) {
        if ("comp" %in% names(inflist[[1]])) {
            addUnivarCompLines(inflist)
        }
    }
}
