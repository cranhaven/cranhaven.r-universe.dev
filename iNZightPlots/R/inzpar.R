#' Plotting parameters for iNZight Plots
#'
#' A whole suite of parameters that can be used to fine-tune plots obtained from the
#' \code{iNZightPlot} function.
#' The parameters include both plot type, style, and appearance.
#'
#' @details
#'
#' \describe{
#' \item{'pch'}{the plotting symbol to be used; default is `21` (circle with fill)}
#' \item{'col.pt'}{the colour of points. this can either be a single value, or a vector of
#' colours if \code{colby} is specified}
#' \item{'col.fun'}{a function to use for colouring points, etc., or the name of a palette,
#'  see \code{inzpalette}
#' }
#' \item{'col.emph', 'col.emphn'}{emphasize the chosen level of a colour by variable. For numeric
#'  colour by, \code{col.emphn} specifies the number of quantiles to use.
#' }
#' \item{'emph.on.top'}{if \code{TRUE}, emphasised points will be positioned on top}
#' \item{'col.default'}{the default colour functions, containing a list with entries for 'cat' and 'cont' variables}
#' \item{'col.missing'}{the colour for missing values; default is a light grey}
#' \item{'reverse.palette'}{logical, if \code{TRUE} the palette will be reversed}
#' \item{'col.method'}{the method to use for colouring by a variable, one of 'linear' or 'rank'}
#' \item{'cex'}{the overall scaling for the entire plot; values less than 1 will make the text and points
#' smaller, while values larger than 1 will magnify everything}
#' \item{'cex.pt'}{the scaling value for points}
#' \item{'cex.dotpt'}{
#'      the scaling value for points in a dotplot. Note, this is not multiplicative with
#'      \code{'cex.pt'}
#' }
#' \item{'cex.lab'}{the scaling value for the plot labels}
#' \item{'cex.axis'}{the scaling value for the axis labels}
#' \item{'cex.main'}{the scaling value for the main plot title}
#' \item{'cex.text'}{the scaling value for text on the plot}
#' \item{'resize.method'}{one of 'proportional' (default) or 'emphasize'}
#' \item{'alpha'}{transparency setting for points; default is 1, 0 is fully transparent}
#' \item{'bg'}{the background colour for the plot}
#' \item{'grid.lines'}{logical to control drawing of axis grid lines}
#' \item{'col.grid'}{if 'grid.lines' is \code{TRUE}, this controls the colour of them.
#'      The default is 'default', which will choose a colour based on the value of 'bg')
#' }
#' \item{'fill.pt'}{the fill colour for points; default is \code{"transparent"}}
#' \item{'lwd'}{the line width of lines (for joining points)}
#' \item{'lty'}{the line type of lines (for joining points)}
#' \item{'lwd.pt'}{the line width used for points; default is 2}
#' \item{'col.line'}{the colour of lines used to join points}
#' \item{'col.sub'}{vector of up to two colours for the background of subplot labels.
#'      If only one specified, it is used for both.
#' }
#' \item{'locate.col.def'}{the default colour for locating points}
#' \item{'highlight.col'}{colour to use for highlighting points}
#' \item{'jitter'}{the axes to add jitter to. Takes values \code{"x"}, \code{"y"},
#'      or \code{"xy"} (default is en empty string, \code{""})}
#' \item{'rugs'}{the axes to add rugs to. Takes same values as \code{jitter}}
#' \item{'trend'}{a vector containing the trend lines to add to the plot. Possible values
#'      are \code{c("linear", "quadratic", "cubic")}}
#' \item{'smooth'}{the smoothing (lowess) for the points. Takes a value between 0 and 1
#'      (the default, 0, draws no smoother)}
#' \item{'smoothby.lty'}{the line type used for smoothers if \code{trend.by = TRUE}}
#' \item{'quant.smooth'}{if quantile smoothers are desired, they can be specified here as either
#'      the quantiles to smooth over (e.g., \code{c(0.25, 0.5, 0.75)}), or \code{"default"}, which
#'      uses the sample size to decide on an appropriate set of quantile smoothers}
#' \item{'LOE'}{logical, if \code{TRUE}, then a 1-1 line of equality is drawn}
#' \item{'join'}{logical, if \code{TRUE}, then points are joined by lines}
#' \item{'lines.by}{logical, if \code{join = TRUE} and \code{colby} is specified, points are joined
#'      by the specified variable}
#' \item{'col.trend'}{a named list of colours to be used for drawing the lines. The default is
#'      \code{list(linear = "blue", quadratic = "red", cubic = "green4")}}
#' \item{'lty.trend'}{a named list of line types for various types of trend lines. The default is
#'      \code{list(linear = 1, quadratic = 2, cubic = 3)}}
#' \item{'trend.by'}{logical, if \code{TRUE}, then trend lines are drawn separately for
#'      each group specified by \code{colby}}
#' \item{'trend.parallel'}{logical, if \code{TRUE}, the trend lines by group are given the same slope;
#'      otherwise they are fit independently}
#' \item{'col.smooth'}{the colour of the smoother}
#' \item{'col.LOE'}{the colour of the line of equality}
#' \item{'lty.LOE'}{the line type of the line of equality}
#' \item{'boxplot'}{logical, if \code{TRUE}, a boxplot is drawn with dotplots and histograms}
#' \item{'box.lwd', 'box.col', 'box.fill'}{the line width, colour, and fill colour for
#'      the box plot drawn}
#' \item{'bar.lwd', 'bar.col', 'bar.fill'}{the line width, colour, and fill colour of bars in a bar plot}
#' \item{'bar.counts'}{logical, if \code{TRUE} bar graphs will display counts instead of percentages (the default)}
#' \item{'bar.relative.width'}{logical, if \code{TRUE} the width of bars will be proportional to the number of observations in each group (colour)}
#' \item{'full.height'}{may no longer be necessary ...}
#' \item{'inf.lwd.comp', 'inf.lwd.conf'}{the line width of comparison and confidence intervals, respectively}
#' \item{'inf.col.comp', 'inf.col.conf'}{the colour of comparison and confidence intervals, respectively.
#'      These take a length 2 vector, where the first element is used for normal inference, while the second
#'      is used for bootstrap intervals}
#' \item{'inference.type'}{the type of inference added to the plot. Possible values
#'      are \code{c("comp", "conf")}}
#' \item{'inference.par'}{the parameter which we obtain intervals for. For a dotplot or histogram,
#'      this can be either \code{"mean"} or \code{"median"}; for bar plots it can be "proportion"}
#' \item{'ci.width'}{the width of confidence intervals, default 0.95 for a 95\% confidence interval}
#' \item{'bs.inference'}{logical, if \code{TRUE}, then nonparametric bootstrap simulation is used
#'      to obtain the intervals}
#' \item{'min.count'}{the min count for barplots inference; counts less than this are ignored}
#' \item{'n.boot'}{the number of bootstrap simulations to perform}
#' \item{'large.sample.size'}{sample sizes over this value will use a large-sample plot variant
#'      (i.e., scatter plots will become hex plots, dot plots become histograms)}
#' \item{'largesample'}{logical, if \code{TRUE}, then the large-sample plot variance is used}
#' \item{'scatter.grid.bins'}{the number, N, of bins to use for the scatter-grid plot,
#'      producing an N x N matrix}
#' \item{'hex.bins'}{the number of bins to use for hexagonal binning}
#' \item{'hex.style'}{the style of the hexagons, one of "size" or "alpha"}
#' \item{'hex.diffuse'}{logical, Pass on rounding error to nearest not-yet-drawn hexes so that
#'      rare classes get represented}
#' \item{'hist.bins'}{the number of bins to use for the histogram
#'      (The default \code{NULL} uses point size to approximate dot plot)}
#' \item{'quant.cutoff'}{if \code{quant.smooth = "default"}, these sample size values are used
#'      to determine which quantiles are drawn}
#' \item{'plottype'}{used to override the default plot type. Possible values, depending on data type,
#'      include \code{c("scatter"|"grid"|"hex"|"dot"|"hist")}}
#' \item{'matchplots'}{logical, if \code{TRUE}, then the type of plot is kept consistent between different
#'      subsets}
#' \item{'match.limits'}{a vector of two values used to decide whether to use all small-sample or all
#'      large-sample plots}
#' \item{'xlim'}{a vector defining the x axis limits (default NULL will use the data)}
#' \item{'ylim'}{a vector defining the y axis limits (default NULL will use the data)}
#' \item{'transform'}{a list of variable transformations (e.g., list(x = 'log'))}
#' \item{'plot.features'}{a list containing any additional features for new plots (e.g., maptype)}
#' \item{'round'}{integer specifying optional rounding of numerical output, default NA (ignored)}
#' \item{'round_percent'}{integer specifying rounding for percentages (default 2)}
#' \item{'signif'}{integer specifying number of significant figured in numeric output (default 2). Ignored if \code{round} is not NA.}
#' \item{'min.pval'}{numeric specifying the minimum p-value to display (default 0.0001)}
#' }
#'
#' @title iNZight Plotting Parameters
#' @param ... If arguments are supplied, then these values are set. If left empty, then
#' @param .viridis checks if the viridis package is installed; or can be turned off
#' the default list is returned.
#' @return an object of class \code{inzpar.list}
#' @export
#' @examples
#' # arguments can be passed directly to \code{iNZightPlot}
#' iNZightPlot(Sepal.Length,
#'     data = iris, col.pt = "red",
#'     box.col = "blue", box.fill = "green"
#' )
#'
#' # or stored and passed to it (only pars relevant to the current
#' # plot are used)
#' mypar <- inzpar(
#'     col.pt = "red", box.col = "blue", box.fill = "green",
#'     trend = "linear", trend.by = TRUE
#' )
#' inzplot(Sepal.Length ~ Species, data = iris, inzpar = mypar)
#' iNZightPlot(Sepal.Length, Sepal.Width,
#'     data = iris, inzpar = mypar,
#'     colby = Species
#' )
inzpar <- function(...,
                   .viridis = requireNamespace("viridis", quietly = TRUE)) {
    dots <- list(...)

    ip <- list(
        pch = 21L,
        col.pt = "grey50",
        col.fun = NULL,
        col.emph = 0L,
        col.emphn = 4L,
        emph.on.top = TRUE,
        col.default = default_palette(.viridis),
        col.missing = "#cccccc",
        reverse.palette = FALSE,
        col.method = "linear",
        cex = 1,
        cex.pt = 0.8,
        cex.dotpt = 0.5,
        cex.lab = 1,
        cex.axis = 0.8,
        cex.main = 1.2,
        cex.text = 1,
        resize.method = "proportional",
        alpha = 1,
        bg = "#eeeeee",
        grid.lines = TRUE,
        col.grid = "default",
        fill.pt = "transparent",
        lwd = 1,
        lty = 1,
        lwd.pt = 2,
        col.line = "blue",
        col.sub = c("#cccccc", "#444444"),
        locate.col.def = "red",
        highlight.col = "shade",
        jitter = "",
        rugs = "",
        trend = NULL,
        smooth = 0,
        smoothby.lty = 4, # MUST be numeric
        quant.smooth = NULL,
        LOE = FALSE,
        join = FALSE,
        lines.by = TRUE,
        col.trend =
            list(
                linear = "blue",
                quadratic = "red",
                cubic = "green4"
            ),
        lty.trend =
            list(
                linear = 1,
                quadratic = 2,
                cubic = 3
            ),
        trend.by = FALSE,
        trend.parallel = TRUE,
        col.smooth = c("magenta"),
        col.LOE = "black",
        lty.LOE = 2,
        mean_indicator = FALSE,
        boxplot = TRUE,
        box.lwd = c(2, 0.7),
        box.col = "black",
        box.fill = "grey90",
        bar.lwd = 1,
        bar.col = "black",
        bar.fill = "darkgreen",
        bar.counts = FALSE,
        bar.relative.width = TRUE,
        full.height = FALSE,
        inf.lwd.comp = 4,
        inf.lwd.conf = 2,
        inf.col.comp = c("black", "green4"),
        inf.col.conf = c("red", "orange"),
        inference.type = NULL,
        inference.par = NULL,
        ci.width = 0.95,
        bs.inference = FALSE,
        min.count = 5,
        n.boot = 1500,
        large.sample.size = 5001,
        largesample = NULL,
        scatter.grid.bins = 50,
        hex.bins = 20,
        hex.style = "size",
        hex.diffuse = TRUE,
        hist.bins = NULL,
        quant.cutoff = c(200, 1000),
        plottype = "default",
        matchplots = TRUE,
        match.limits = c(500, 10000),
        internal.labels = TRUE,
        xlim = NULL,
        ylim = NULL,
        transform = list(),
        plot.features = list(),
        epi.out = FALSE,
        round = NA_integer_,
        round_percent = 2L,
        signif = 4L,
        min_pval = 1e-4
    )

    # update any user has specified
    if (length(dots) > 0) {
        for (i in names(dots)) {
            ip[[i]] <- dots[[i]]
        }
    }

    class(ip) <- "inzpar.list"
    ip
}

# Used internally
valid_par <- function(par, plot_type, what = c("plot", "summary", "inference")) {
    what <- substr(match.arg(what), 1, 1)
    res <- rep(TRUE, length(par))
    if (!plot_type %in% colnames(plot_types)) {
        warning("Invalid or unknown plot type")
        return(res)
    }
    v <- par %in% rownames(plot_types)
    res[v] <- grepl(what, plot_types[par[v], plot_type])
    res
}

gg_defaults <- list(
    adjust = 1,
    alpha_densitygroup = 0.6,
    gg_lwd = 1,
    gg_size = 6,
    ordered = FALSE,
    gg_barSize = 16,
    gg_bins = 30,
    gg_height = 0.5,
    gg_width = 1,
    gg_perN = NULL,
    gg_swarmwidth = 0.4,
    gg_method = "quasirandom",
    gg_cutpoint = "Default",
    gg_theme = "grey",
    fill_colour = "",
    rotation = FALSE,
    rotate_labels = list(x = FALSE, y = FALSE),
    caption = "",
    desc = FALSE,
    palette = "default"
)

default_palette <- function(use_viridis = TRUE) {
    use_viridis <- use_viridis && requireNamespace("viridis", quietly = TRUE)

    cat_pal <- c(
        "#E69F00", "#56AAE9", "#D55E00", "#0072B2",
        "#F0D705", "#ADD9FF", "#9BCD9B", "#CC79A7",
        "#68468C", "#8B0000"
    )
    cont_pal <- function(n) {
        hcl((1:n) / n * 320 + 60, c = 100, l = 50)
    }

    opt_cat <- getOption("inzight.default.palette.cat")
    opt_cont <- getOption("inzight.default.palette.num")

    v_cat <- use_viridis
    if (!is.null(opt_cat) && is.character(opt_cat)) {
        v_cat <- FALSE
        cat_pal <- opt_cat
    }
    v_cont <- use_viridis
    if (!is.null(opt_cont) && is.function(opt_cont)) {
        t <- try(opt_cont(5), silent = TRUE)
        if (!inherits(t, "try-error") && is.character(t) && length(t) == 5) {
            v_cont <- FALSE
            cont_pal <- opt_cont
        }
    }

    list(
        cat =
            if (v_cat) {
                function(n) {
                    if (n > length(cat_pal)) {
                        viridis::viridis(n)
                    } else {
                        cat_pal[1:n]
                    }
                }
            } else {
                function(n) {
                    if (n > length(cat_pal)) {
                        hcl((1:n) / n * 360, c = 80, l = 50)
                    } else {
                        cat_pal[1:n]
                    }
                }
            },
        cont =
            if (v_cont) {
                viridis::viridis
            } else {
                cont_pal
            }
    )
}
