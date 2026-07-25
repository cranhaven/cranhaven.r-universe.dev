#' A general plotting function that automatically detects variable type and
#' draws the appropriate plot.
#' It also provides facilities to add inference information to plots, colour-
#' and size-by variables, and can handle survey data.
#'
#' The main goal of 'iNZightPlots' is to make it easy to beginners to
#' explore a dataset graphically, using a suite of simple arguments
#' to add features to their graph.
#'
#' The second use of this function is within the companion software
#' 'iNZight', providing a single function call with arguments
#' controlled by the user through a GUI.
#'
#' @title iNZight Plot
#' @param x a vector (numeric or factor), or the name of a column in the
#'        supplied \code{data} or \code{design} object
#' @param y a vector (numeric or factor), or the name of a column in the
#'        supplied \code{data} or \code{design} object
#' @param g1 a vector (numeric or factor), or the name of a column in the
#'        supplied \code{data} or \code{design} object.
#'        This variable acts as a subsetting variable.
#' @param g1.level the name (or numeric position) of the level of \code{g1}
#'        that will be used instead of the entire data set
#' @param g2 a vector (numeric or factor), or the name of a column in the
#'        supplied \code{data} or \code{design} object.
#'        This variable acts as a subsetting variable, similar to \code{g1}
#' @param g2.level same as \code{g1.level}, however takes the additional value
#'        \code{"_MULTI"}, which produces a matrix of \code{g1} by \code{g2}
#' @param varnames a list of variable names, with the list named using
#'        the appropriate arguments
#'        (i.e., \code{list(x = "height", g1 = "gender")})
#' @param colby the name of a variable (numeric or factor) to colour points by.
#'        In the case of a numeric variable, a continuous colour scale is used,
#'        otherwise each level of the factor is assigned a colour
#' @param sizeby the name of a (numeric) variable,
#'        which controls the size of points
#' @param symbolby the name of a factor variable to code point symbols
#' @param extra.vars the names of any additional variables to be passed through
#'        the internal functions to the create and plot methods.
#' @param locate variable to label points
#' @param locate.id id of points (row numbers) to label, or
#'        an expression that evaluates as a logical vector (e.g., \code{x > 5})
#' @param locate.col the colour to locate points if a variable is not specified
#' @param locate.extreme \code{numeric}, the number of extreme points to label
#'        (using Mahalanobis' distance)
#' @param locate.same.level name of a variable to label points with same level
#'        of as those specified with `locate.id`
#' @param highlight \code{numeric} vector consisting of the row numbers/IDs of
#'        points to highlight
#' @param data the name of a data set
#' @param design the name of a survey object, obtained from the \code{survey}
#'        package
#' @param freq the name of a frequency variable if the data are frequencies
#' @param missing.info logical, if \code{TRUE}, information regarding
#'        missingness is displayed in the plot
#' @param xlab the text for the x-label
#' @param ylab the text for the y-label
#' @param show_units logical, if `TRUE` (default) units will be shown beside axies and legend variable labels
#' @param new logical, used for compatibility
#' @param df compatibility argument
#' @param env compatibility argument
#' @param ... additional arguments, see \code{inzpar}
#' @param inzpars allows specification of iNZight plotting parameters over
#'        multiple plots
#' @param layout.only logical, if \code{TRUE}, only the layout is drawn
#'        (useful if a custom plot is to be drawn)
#' @param plot logical, if \code{FALSE}, the plot is not drawn
#'        (used by \code{summary})
#' @param xaxis logical, whether or not to draw the x-axis
#' @param yaxis logical, whether or not to draw the y-axis
#' @param xlim specify the x limits of the plot
#' @param ylim specify the y limits of the plot
#' @param zoombars numeric, length 2; when drawing a bar plot, if the number of
#'        bars is too large, the user can specify a subset.
#'        The first value is the starting point (1 is the first bar, etc),
#'        while the second number is the number of bars to show.
#' @param hide.legend logical, if TRUE, the \code{legend} will not be drawn
#' @return An \code{inzightplotoutput} object, which contains the information
#'         displayed in the plot
#'
#' @import stats grid grDevices boot survey quantreg survey hexbin iNZightMR
#'         colorspace dichromat
#' @importFrom utils capture.output browseURL capture.output modifyList
#' @importFrom iNZightTools is_num is_cat is_dt is_survey is_svydesign is_svyrep
#' @author Tom Elliott
#' @export
#' @examples
#' iNZightPlot(Species, data = iris)
#' iNZightPlot(Petal.Width, g1 = Species, data = iris)
#'
#' iNZightPlot(Sepal.Length, Sepal.Width,
#'     data = iris,
#'     colby = Species
#' )
#' iNZightPlot(Sepal.Length, Sepal.Width,
#'     data = iris,
#'     colby = Species, trend = c("linear", "quadratic"),
#'     trend.by = TRUE, trend.parallel = FALSE
#' )
#'
#' # add inference information
#' iNZightPlot(Petal.Width,
#'     data = iris,
#'     inference.type = "conf", inference.par = "mean"
#' )
#' iNZightPlot(Petal.Width,
#'     data = iris,
#'     inference.type = "conf", inference.par = "mean",
#'     bootstrap = TRUE
#' )
#'
#' # alternatively, use the formula interface
#' inzplot(Sepal.Length ~ Sepal.Width | Species, data = iris)
iNZightPlot <- function(x,
                        y = NULL,
                        g1 = NULL,
                        g1.level = NULL,
                        g2 = NULL,
                        g2.level = NULL,
                        varnames = list(),
                        colby = NULL,
                        sizeby = NULL,
                        symbolby = NULL,
                        extra.vars,
                        locate = NULL,
                        locate.id = NULL,
                        locate.col = NULL,
                        locate.extreme = NULL,
                        locate.same.level = NULL,
                        highlight = NULL,
                        data = NULL,
                        design = NULL,
                        freq = NULL,
                        missing.info = TRUE,
                        xlab,
                        ylab,
                        show_units = TRUE,
                        new = TRUE,
                        inzpars = inzpar(),
                        layout.only = FALSE,
                        plot = TRUE,
                        xaxis = TRUE,
                        yaxis = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        zoombars = NULL,
                        hide.legend = FALSE,
                        df,
                        env = parent.frame(),
                        ...) {
    # ---------------------------------------------------------------------------- #
    #   iNZightPlots v2.0, written by Tom Elliott (2014, University of Auckland)
    #
    # This function will `attempt` to take a large variety of data configurations
    # and attempt to make a suitable plot. It can take into account the data
    # structure (for example, frequency or survey data), and make use of these in
    # the final plot. It also contains a suite of additional options such a trend
    # lines and inference information which can also be added to plots as required.
    #
    # A Summary and Inference method will be associated with the output of this
    # file, so users can easily get numerical information about any particular
    # plot they produce. The inference information will be either theoretically or
    # bootstrap based.
    #
    # ---------------------------------------------------------------------------- #
    # ++++++++ FOR (future) DEVELOPERS ++++++++
    #
    # First of all, welcome to the iNZight team!3
    # Second, have fun reading through all the code :D
    #
    # +++ How iNZightPlots works +++
    #
    # I'll write this later ...
    # ... update (21/01/2020) - not yet written :'D
    #
    # Have fun coding!
    # ---------------------------------------------------------------------------- #
    #          Original author: Tom Elliott <tom.elliott@auckland.ac.nz>           #
    # ---------------------------------------------------------------------------- #

    ################################################################################
    ################################################################################

    # ---------------------------------------------------------------------------- #
    # 1. The data step
    # ----------------

    # grab the arguments and the data frame is supplied:
    m <- match.call(expand.dots = FALSE)

    ## getSummary and other wrappers will pass an inz.data object
    if (missing(df)) {
        if (!is.null(design)) {
            if (any(grepl("as.svrepdesign", design$call[[1]], fixed = TRUE))) {
                stop("Objects created with `as.svrepdesign` not yet supported.")
            }
            md <- eval(m$design, env)
        } else {
            md <- eval(m$data, env)
        }

        ## we now want to create a data object which contains ALL of the
        ## necessary information, including survey design,
        ## or frequency information:
        df <- inzDataframe(
            m,
            data = md,
            names = varnames,
            g1.level,
            g2.level,
            env = env
        )
    }

    slf <- single_level_factors(df$data)
    if (!is.null(design) && any(slf)) {
        slf_vars <- df$varnames[slf]
        stop(
            paste(
                sep = "\n",
                "The following variables in the survey design only have",
                "  a single level, which is not supported:",
                sprintf("     %s", paste(slf_vars, collapse = ", "))
            )
        )
    }

    ## For the time being, just use `ggplot2` for multiple-variable plots:
    multi_var <- any(
        sapply(
            df$data,
            function(x) tibble::is_tibble(x) && ncol(x) > 1L
        )
    )

    dots <- list(...)
    dots$plot <- plot
    if (multi_var) {
        return(multiplot(df, dots))
    }

    df$data <- as.data.frame(df$data)

    ## FIGURE OUT PLOTTYPE
    DEFAULT_plottypes <- getOption("inzight.default.plottypes")
    # list(num = '', cat = '', catcat = '', numcat = '', numnum = '')
    if ((is.null(dots$plottype) || dots$plottype == "default") &&
        !is.null(DEFAULT_plottypes)) {
        # check variable types:
        plottype <- NULL

        xnum <- is_num(df$data[["x"]])
        if ("y" %in% names(m)) {
            # two-variable plots
            ynum <- is_num(df$data[["y"]])
            pt <- paste0(ifelse(xnum, "num", "cat"), ifelse(ynum, "num", "cat"))
            if (pt == "catnum") pt <- "numcat"
            # num x cat plots default to the numeric plot
            if (pt == "numcat" && is.null(DEFAULT_plottypes[["numcat"]])) pt <- "num"
            plottype <- DEFAULT_plottypes[[pt]]
        } else {
            # single-variable plots
            plottype <- DEFAULT_plottypes[[ifelse(xnum, "num", "cat")]]
        }

        if (!is.null(plottype)) dots$plottype <- plottype
        rm(plottype)
    }

    if (isTRUE(grepl("^gg_", dots$plottype))) {
        # Required, general packages = 1, other pkgs for specific plots = 0.
        gg_pkgs <- c(
            "ggplot2",
            "ggtext",
            "dplyr",
            "tidyr",
            "forcats",
            "ggmosaic",
            "waffle",
            "ggthemes",
            "ggbeeswarm",
            "ggridges"
        )
        gg_pkgs_check <- sapply(gg_pkgs, requireNamespace, quietly = TRUE)

        if (any(!gg_pkgs_check)) {
            gg_pkgs_needed <- gg_pkgs[!gg_pkgs_check]
            message(
                "In order to use this (and other) plot types, you must install\n",
                "the following packages:"
            )
            message(
                "\n    ",
                paste(
                    gg_pkgs_needed,
                    collapse = ", "
                ),
                "\n\n"
            )

            message("You can do this by running the following command:")
            message(
                sprintf(
                    "\n    install.packages(c(%s))\n\n",
                    paste("\"", gg_pkgs_needed, "\"",
                        collapse = ", ",
                        sep = ""
                    )
                )
            )

            warning(
                sprintf(
                    "Ignoring `plottype = %s`",
                    dots$plottype
                )
            )
            dots$plottype <- NULL
        } else {
            # Remove xlab and ylab from varnames list (for lite)
            varnames <- varnames[!(names(varnames) %in% c("xlab", "ylab"))]

            if (!("data_name" %in% names(list(...)))) {
                data_name <- as.character(match.call()[["data"]])
            } else {
                data_name <- list(...)$data_name
            }

            if ("x" %in% names(m) && !("x" %in% names(varnames))) {
                varnames[["x"]] <- as.character(m[["x"]])
            }

            if ("y" %in% names(m) && !("y" %in% names(varnames))) {
                varnames[["y"]] <- as.character(m[["y"]])
            }

            # If Y is num, X is cat, flip
            if ("y" %in% names(m) &&
                is_num(df$data[["x"]]) &&
                is_cat(df$data[["y"]])) {
                xn <- varnames[["y"]]
                varnames[["y"]] <- varnames[["x"]]
                varnames[["x"]] <- xn

                xx <- m$x
                m$y <- m$x
                m$x <- xx
            }

            if ("g1" %in% names(varnames)) {
                g1 <- varnames[["g1"]]
            } else {
                g1 <- m$g1
            }

            if ("g2" %in% names(varnames)) {
                g2 <- varnames[["g2"]]
            } else {
                g2 <- m$g2
            }

            vn <- unlist(
                modifyList(
                    as.list(df$varnames),
                    as.list(df$labels)
                )
            )
            # vn <- stringr::str_trunc(vn, 50, "center")
            ret.plot <- do.call(
                iNZightPlotGG,
                c(
                    list(
                        setNames(df$data, vn),
                        type = dots$plottype,
                        data_name = data_name,
                        main = list(...)$main,
                        xlab = if (missing(xlab)) NULL else xlab,
                        ylab = if (missing(ylab)) NULL else ylab,
                        extra_args = c(list(plottype = dots$plottype), list(...)),
                        palette = list(...)$palette,
                        gg_theme = list(...)$gg_theme,
                        caption = list(...)$caption
                        # g1 = as.character(g1),
                        # g2 = as.character(g2)
                    ),
                    vn,
                    list(
                        g1.level = g1.level,
                        g2.level = g2.level
                    )
                )
            )

            attr(ret.plot, "varnames") <-
                modifyList(
                    as.list(vn),
                    as.list(df$varnames)
                )

            return(ret.plot)
        }
    }


    total.missing <- sum(apply(df$data, 1, function(x) any(is.na(x))))
    total.obs <- nrow(df$data)

    ## df will have a class: inz.simple, inz.freq, inz.survey
    ## each of these classes will have appropriate methods for
    ## extracting the information

    varnames <- as.list(df$varnames)

    ## Any varnames supplied that AREN'T needed must be removed,
    ## otherwise errors:
    nullVars <- sapply(as.list(m)[names(varnames)], is.null)
    varnames[nullVars] <- NULL

    ## In some cases, arguments are removed and must be continued on other error
    ## (e.g., too many factor levels, etc)
    varnames[!names(varnames) %in% colnames(df$data)] <- NULL
    vartypes <- lapply(
        df$data[, names(varnames), drop = FALSE],
        function(x) ifelse(is.factor(x), "factor", "numeric")
    )
    names(vartypes) <- unlist(varnames)
    df.vs <- colnames(df$data)
    missing <- list() # a container to save missing value information

    ## ensure it matches what comes back from `inzDataframe()`
    g.level <- df$glevels
    g1.level <- g.level$g1.level
    g2.level <- g.level$g2.level

    # do some type checks
    xfact <- is.factor(df$data$x)
    ynull <- !"y" %in% df.vs
    yfact <- if (ynull) NULL else is.factor(df$data$y)

    ## check the number of levels for a barchart:
    if (!is.null(zoombars)) {
        if (zoombars[2] == 0) {
            zoombars <- NULL
        }
    }

    if (xfact) {
        if (ynull) {
            if (length(levels(df$data$x)) > params("max.levels") &
                is.null(zoombars)) {
                msg <- paste0(
                    "Too many levels in ",
                    varnames$x,
                    " to draw a barchart.\n",
                    "(",
                    varnames$x,
                    " has ",
                    length(levels(df$data$x)),
                    " levels.)"
                )
                stopPlot(msg)
                plot <- FALSE
            }
        } else if (yfact) {
            if (length(levels(df$data$x)) * length(levels(df$data$y)) >
                params("max.levels") & is.null(zoombars)) {
                msg <- paste0(
                    "Too many levels in ",
                    varnames$x,
                    " and ",
                    varnames$y,
                    " to draw a barchart.\n",
                    "(",
                    varnames$x,
                    " has ",
                    length(levels(df$data$x)),
                    " levels, ",
                    varnames$y,
                    " has ",
                    length(levels(df$data$y)),
                    "levels.)"
                )
                stopPlot("Too many levels in x and y to draw a barchart.")
                plot <- FALSE
            }
        }
    }

    # dots <- list(...)  # capture the additional arguments
    opts <- inzpars
    wopt <- names(dots) %in% names(opts) # which additional settings specified
    opts <- utils::modifyList(opts, dots[wopt])

    ## store transformation information (for axes, etc)
    if (!is.null(df$transformations)) {
        opts$transform <- utils::modifyList(
            opts$transform,
            df$transformations
        )
    }
    ## apply transformations
    df <- inztransform(df, opts$transform)

    if (!is.null(opts$transform)) {
        if (!is.null(opts$transform$x)) {
            if (!is.null(xlim)) {
                xlim <- switch(opts$transform$x,
                    "log" = log(xlim),
                    "log10" = log10(xlim),
                    xlim
                )
            }
        }
        if (!is.null(opts$transform$y)) {
            if (!is.null(ylim)) {
                ylim <- switch(opts$transform$y,
                    "log" = log(ylim),
                    "log10" = log10(ylim),
                    ylim
                )
            }
        }
    }

    # colour-by function
    if (!is.null(opts$col.fun) && is.character(opts$col.fun)) {
        cpal <- opts$col.fun
        cfun <- try(
            {
                inzpalette(opts$col.fun)
            },
            silent = TRUE
        )
        if (inherits(cfun, "try-error")) {
            warning("Invalid palette name, please supply a palette listed in 'inzpalette()'")
            opts$col.fun <- NULL
        } else {
            opts$col.fun <- cfun
        }
    }

    ## --- colour by
    if (!is.null(df$data$colby)) {
        if (!is.numeric(df$data$colby)) {
            opts$col.method <- "linear"
        }

        if (opts$col.method == "rank") {
            ranks <- rank(df$data$colby, na.last = "keep") - 1
            df$data$colby <- ranks * 100 / max(ranks, na.rm = TRUE)
            rm(ranks)
        }

        # emphasize
        if (!is.null(opts$col.emph)) {
            if (is.character(opts$col.emph)) {
                opts$col.emph <- levels(df$data$colby)[opts$col.emph]
            }
            if (!is.na(opts$col.emph) && opts$col.emph > 0L &&
                (opts$col.emph <= length(levels(df$data$colby)) ||
                    opts$col.emph <= opts$col.emphn)) {
                opts$col.fun <- eval(
                    rlang::expr(
                        function(n) {
                            emphasize_pal_colour(
                                n,
                                opts$col.emph,
                                !!is.factor(df$data$colby),
                                !!opts$col.emphn,
                                !!opts$col.fun
                            )
                        }
                    )
                )
                if (opts$emph.on.top) {
                    if (is.null(opts$plot.features$order.first) ||
                        length(opts$plot.features$order.first) == 0) {
                        ## set point order so emphasised are on top (at bottom of data)
                        cby <- df$data$colby
                        if (is_num(cby)) {
                            Qs <- seq(min(cby, na.rm = TRUE), max(cby, na.rm = TRUE),
                                length = opts$col.emphn + 1
                            )
                            ord1st <- which(
                                cby >= Qs[opts$col.emph] &
                                    cby < Qs[opts$col.emph + 1]
                            )
                        } else {
                            ord1st <- which(as.integer(df$data$colby) == opts$col.emph)
                        }
                        opts$plot.features <- modifyList(
                            opts$plot.features,
                            list(order.first = ord1st)
                        )
                    }
                }
            }
        }
    }

    ## out of previous if() for case of barplots
    if (opts$reverse.palette) {
        opts$.colfun <- opts$col.fun
        opts$col.fun <- function(n) rev(opts$.colfun(n)[1:n])
    }

    ## --- SIZING
    if ("sizeby" %in% df.vs) {
        if ( # (all(df$data$sizeby >= 0) || all(df$data$sizeby <= 0)) &&
            opts$resize.method == "proportional") {
            cex.trans <- sqrt(df$data$sizeby)
            df$data$.cex <- cex.trans / mean(cex.trans, na.rm = TRUE)
        } else {
            if (opts$resize.method == "proportional") {
                warning("Using method `emphasize`.")
            }
            df$data$.cex <- sqrt(rescale(df$data$sizeby))
        }
    }


    # subset the data by g2 (keep everything, so xlims can be calculated)
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
    dfsub <- gSubset(df, g1.level, g2.level, df.vs, missing)
    matrix.plot <- dfsub$matrix
    missing <- dfsub$missing
    g1.level <- dfsub$g1.level
    g2.level <- dfsub$g2.level

    df.list <- dfsub$df

    # now, everything simply gets applied to the list of dataframes to
    # generate the necessary plots

    # ------------------------------------------------------------------------ #
    # 2. The plot setup step
    # ----------------------

    # The aim of this step is to produce a list of things to plot,
    # each element pertaining to a level of g1 and g2,
    # containing the necessary information.

    if (!xfact) xx <- df$data$x
    if (!ynull) if (!yfact) yy <- df$data$y

    xattr <- list(
        class = class(df), v = colnames(df$data),
        varnames = as.list(df$varnames),
        vartypes = structure(vartypes, .Names = names(varnames))
    )

    ## HERE IS THE SWTICH FOR CHANGING FROM DIFFERENT TYPES OF DOT PLOT ZOOMING
    if (!xfact) {
        xattr$xrange <- range(xx[is.finite(xx)])
    }
    if (!ynull) if (!yfact) xattr$yrange <- range(yy[is.finite(yy)])
    if (!is.null(df$max.freq)) {
        xattr$max.freq <- df$max.freq
    }
    if (!is.null(locate.extreme)) xattr$nextreme <- locate.extreme

    if (!is.null(zoombars)) xattr$zoom <- zoombars

    if (inherits(df.list, "inz.survey")) {
        xattr$max.weight <- max(get_weights(df$design))
    }


    if (opts$matchplots) {
        # this is the case where the data is subset by g1/g2, but we want the
        # plots to be the same across all levels

        # we just need to go through all plots and test if they should be
        # LARGESAMPLE or not:
        if (is.null(opts$largesample)) {
            sample.sizes <- do.call(
                c,
                lapply(
                    df.list,
                    function(df) {
                        sapply(
                            df,
                            function(a) {
                                if (is_survey(a)) {
                                    o <- nrow(a$variables)
                                } else {
                                    o <- nrow(a)
                                }
                                if (is.null(o)) 0 else o
                            }
                        )
                    }
                )
            )
            smallest.sample <- min(sample.sizes, na.rm = TRUE)
            largest.sample <- max(sample.sizes, na.rm = TRUE)

            ## grab parameters
            N.LARGE <- opts$large.sample.size
            N.LIMITS <- opts$match.limits

            ## Do we need different plots?
            if (smallest.sample > N.LARGE) {
                ## all sample sizes are big enough
                opts$largesample <- TRUE
            } else if (largest.sample < N.LARGE) {
                ## all sample sizes are small enough
                opts$largesample <- FALSE
            } else if (smallest.sample > N.LIMITS[1]) {
                ## the smallest sample is bigger than the lower limit
                opts$largesample <- TRUE
            } else if (largest.sample < N.LIMITS[2]) {
                ## the largest sample is smaller than the upper limit
                opts$largesample <- FALSE
            } else {
                ## sample sizes range outside both upper and lower limits
                opts$largesample <-
                    as.logical(round(mean(sample.sizes > N.LARGE)))
            }
        }
    }



    ## if creating a dot plot, must figure out the size of a symbol:
    itsADotplot <- FALSE
    if (ynull & !xfact) {
        itsADotplot <- TRUE
    } else if (!ynull) {
        if ((!xfact & yfact) | (xfact & !yfact)) {
            itsADotplot <- TRUE
        }
    }

    if (itsADotplot) {
        if (opts$plottype != "dot") {
            if (opts$plottype != "default" |
                (opts$plottype == "default" & opts$largesample)) {
                itsADotplot <- FALSE
            }
        }
    }

    if (itsADotplot) {
        if (!plot || is.null(dev.list())) {
            xattr$symbol.width <- 1
        } else {
            xattr$symbol.width <- convertWidth(unit(opts$cex.dotpt, "char"),
                "native",
                valueOnly = TRUE
            )
        }

        ## sort out bin sizing:
        allX <- if (xfact) df$data$y else df$data$x
        allX <- allX[!is.na(allX)]
        diffs <- diff(sort(allX))
        if (all(diffs == 0)) {
            diffs <- 1L
            mdiff <- 1L
            xr <- 1L
            isDiscrete <- TRUE
            mult.width <- 1L
        } else {
            diffs <- diffs[diffs > 0]
            mdiff <- min(diffs)
            fdiff <- diffs / mdiff
            isDiscrete <- all(round(fdiff) == fdiff)
            xr <- diff(range(allX, na.rm = TRUE))
            mult.width <- ifelse(isDiscrete, 1, 1.2)
        }

        xattr$dotplotstuff <- list(
            mdiff = mdiff,
            xr = xr,
            isDiscrete = isDiscrete,
            mult.width = mult.width
        )
    }

    # for now, disable barplot counts if two-way table
    if (xattr$class == "inz.survey" && opts$bar.counts) {
        warning("Showing counts on survey bar plots is currently unavailable")
        opts$bar.counts <- FALSE
    }


    ## createPlot - uses various things such as "grobWidth" which causes
    ## a new device to open so create a NULL device and delete it afterwards ...
    if (plot) {
        # The Main Viewport: this one is simply the canvas, and global CEX value
        dd <- dev.flush(dev.flush()) # flush everything ...

        dev.hold()
        grid.newpage()
        pushViewport(
            viewport(
                gp = gpar(cex = opts$cex),
                name = "container"
            )
        )
    } else {
        try(
            {
                jpeg(FILE <- tempfile())
            },
            silent = TRUE
        )
    }

    plot.list <- lapply(
        df.list,
        function(df) lapply(df, createPlot, opts, xattr)
    )

    plot.class <- class(plot.list[[1]][[1]])

    if (!plot) {
        try(
            {
                dev.off()
                unlink(FILE)
            },
            silent = TRUE
        )
    }

    xlim.raw <- range(
        sapply(
            plot.list,
            function(x) sapply(x, function(y) y$xlim)
        ),
        finite = TRUE
    )
    ylim.raw <- range(
        sapply(
            plot.list,
            function(x) sapply(x, function(y) y$ylim)
        ),
        finite = TRUE
    )
    if (!is.null(xlim)) xlim.raw <- xlim
    if (!is.null(ylim)) ylim.raw <- ylim


    ## Allow plot create methods to turn off axes:
    if (!is.null(plot.list[[1]][[1]]$draw.axes)) {
        if (!plot.list[[1]][[1]]$draw.axes) {
            xaxis <- yaxis <- FALSE
        }
    }

    ## Allow plot create methods to reserve a global object
    if (!is.null(plot.list[[1]][[1]]$global.object)) {
        global.object <- plot.list[[1]][[1]]$global.object
    }

    if (is.null(xlim) | any(plot.class == "inzbar")) {
        xlim <- xlim.raw
    }
    if (is.null(ylim) | "inzbar" %in% plot.class) {
        ylim <- ylim.raw
    }

    TYPE <- gsub("inz", "", class(plot.list[[1]][[1]]))
    if (!any(TYPE %in% c("bar"))) xlim <- extendrange(xlim)
    ylim <-
        if (any(TYPE %in% c("scatter", "grid", "hex"))) {
            extendrange(ylim)
        } else {
            c(0, extendrange(ylim)[2])
        }
    barplot <- any(TYPE == "bar")

    if (barplot) {
        BARPLOT.N <- lapply(
            plot.list,
            function(x) lapply(x, function(y) y$ntotal)
        )
    }

    if (diff(range(xlim)) == 0) {
        xlim <- xlim + c(-1, 1)
    }
    if (diff(range(ylim)) == 0) {
        ylim <- ylim + c(-1, 1)
    }

    maxcnt <- NULL
    if (any(TYPE %in% c("grid", "hex"))) {
        # if there is a `counts` need to get the max:
        maxcnt <- switch(TYPE[which(TYPE %in% c("grid", "hex"))],
            "grid" = {
                warning(
                    "Frequency density not constant scale ",
                    "across multiple plots yet."
                )
            },
            "hex" = {
                max(
                    sapply(
                        plot.list,
                        function(x) {
                            sapply(
                                x,
                                function(y) {
                                    if (inherits(y, "inzhex") && !is.null(y$hex)) {
                                        max(y$hex@count, 0, na.rm = TRUE)
                                    } else {
                                        0
                                    }
                                }
                            )
                        }
                    )
                )
            }
        )
    } else if (any(TYPE %in% c("dot", "hist"))) {
        maxcnt <- ylim[2] # .raw[2]
    }

    if (any(plot.class %in% c("inzdot", "inzhist"))) {
        if (any(plot.class == "inzhist")) {
            nOutofview <- 0
        } else {
            nOutofview <- sum(
                sapply(
                    plot.list,
                    function(x) {
                        sapply(
                            x,
                            function(y) {
                                sapply(
                                    y$toplot,
                                    function(z) {
                                        sum(z$x < min(xlim) | z$x > max(xlim))
                                    }
                                )
                            }
                        )
                    }
                )
            )
        }
    } else if (all(plot.class != "inzbar")) {
        nOutofview <- sum(
            sapply(
                plot.list,
                function(x) {
                    sapply(
                        x,
                        function(z) {
                            sum(
                                z$x < min(xlim) |
                                    z$x > max(xlim) |
                                    z$y < min(ylim) |
                                    z$y > max(ylim)
                            )
                        }
                    )
                }
            )
        )
    } else {
        nOutofview <- 0
    }

    if (is.numeric(df$data$colby)) {
        opts$trend.by <- FALSE
    }

    # Set up the plot layout

    if (plot) {
        # essentially the height of the window
        PAGE.height <- convertHeight(current.viewport()$height, "in", TRUE)

        ## --- there will be some fancy stuff here designing and implementing
        ## a grid which adds titles, labels, and optionally legends

        ## --- first, need to make all of the labels/legends/etc:
        VT <- vartypes
        names(VT) <- names(varnames)

        if (all(c("x", "y") %in% names(VT))) {
            ## switch X/Y for dotplots

            if (VT$y == "numeric" & VT$x == "factor") {
                xn <- varnames$y
                varnames$y <- varnames$x
                varnames$x <- xn
                VT$x <- "numeric"
                VT$y <- "factor"

                my <- missing$y
                missing$y <- missing$x
                missing$x <- my

                # swap labels, if they exist
                l_x <- df$labels$x
                l_y <- df$labels$y
                df$labels$x <- l_y
                df$labels$y <- l_x
            }
        }
        if (missing(xlab) || is.null(xlab)) {
            xlab <- df$labels$x %||% varnames$x
        }
        if (missing(ylab) || is.null(ylab)) {
            ylab <- df$labels$y %||% varnames$y
        }

        titles <- list()
        titles$main <-
            if (!is.null(dots$main)) {
                makeTitle(df$labels, VT, g1.level, g2.level,
                    template = dots$main
                )
            } else {
                makeTitle(df$labels, VT, g1.level, g2.level)
            }
        titles$xlab <- xlab
        if (!ynull) {
            titles$ylab <-
                if (xfact & yfact) {
                    ifelse(opts$bar.counts, "Count", "Percentage (%)")
                } else {
                    ylab
                }
        } else if (xfact) {
            titles$ylab <- ifelse(opts$bar.counts, "Count", "Percentage (%)")
        }
        if ("colby" %in% df.vs) {
            titles$legend <- df$labels$colby %||% varnames$colby
        }

        if (show_units) {
            titles$xlab <- add_units(titles$xlab, df$units$x)
            titles$ylab <- add_units(titles$ylab, df$units$y)
            titles$colby <- add_units(titles$legend, df$units$colby)
        }

        ## plot.list still contains all the levels of g1 that wont be plotted
        ## - for axis scaling etc
        ## so figure this one out somehow ...
        ng1 <- ifelse("g1" %in% names(df$data), length(g1.level), 1)
        ng2 <- ifelse(
            "g2" %in% names(df$data),
            ifelse(
                matrix.plot,
                ifelse(
                    g2.level == "_MULTI",
                    length(plot.list),
                    length(g2.level)
                ),
                1
            ),
            1
        )
        N <- ng1 * ng2 # length(plot.list) * length(g1.level)
        NN <- if (matrix.plot) length(plot.list) * length(plot.list[[1]]) else N
        # this has absolutely no theoretical reasoning,
        # it just does a reasonably acceptable job (:
        multi.cex <- max(1.2 * sqrt(sqrt(NN) / NN), 0.5)


        # --- WIDTHS of various things
        # first we need to know HOW WIDE the main viewport is, and then
        # split the title text into the appropriate number of lines,
        # then calcualate the height of it.
        VPcontainer.width <- convertWidth(unit(1, "npc"), "in", TRUE)
        main.grob <- textGrob(titles$main,
            gp = gpar(cex = opts$cex.main),
            name = "inz-main-title"
        )
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
        MAIN.lnheight <- convertWidth(grobHeight(main.grob), "in", TRUE)
        if (MAIN.width > 0.9 * VPcontainer.width) {
            titles$main <- gsub(",", ",\n", titles$main)
            main.grob <- textGrob(titles$main,
                gp = gpar(cex = opts$cex.main),
                name = "inz-main-title"
            )
            MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
        }
        if (MAIN.width > 0.9 * VPcontainer.width) {
            titles$main <- gsub("subset", "\nsubset", titles$main)
            main.grob <- textGrob(titles$main,
                gp = gpar(cex = opts$cex.main),
                name = "inz-main-title"
            )
            MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
        }
        if (MAIN.width > 0.9 * VPcontainer.width) {
            titles$main <- gsub(" (size prop", "\n (size prop",
                titles$main,
                fixed = TRUE
            )
            main.grob <- textGrob(titles$main,
                gp = gpar(cex = opts$cex.main),
                name = "inz-main-title"
            )
            MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
        }
        MAIN.height <- convertHeight(
            grobHeight(main.grob),
            "in",
            TRUE
        ) + MAIN.lnheight

        # -- xaxis labels
        xlab.grob <- textGrob(titles$xlab,
            y = unit(0.6, "lines"),
            gp = gpar(cex = opts$cex.lab),
            name = "inz-xlab"
        )
        XLAB.height <- convertHeight(grobHeight(xlab.grob), "in", TRUE) * 3
        # -- yaxis labels
        if (!is.null(titles$ylab)) {
            ylab.grob <- textGrob(titles$ylab,
                x = unit(0.6, "lines"),
                name = "inz-ylab",
                rot = 90,
                gp = gpar(cex = opts$cex.lab)
            )
            YLAB.width <- convertWidth(grobWidth(ylab.grob), "in", TRUE) * 3
        } else {
            YLAB.width <- 0
        }

        ## -- xaxis marks
        XAX.height <-
            convertWidth(unit(1, "lines"), "in", TRUE) * 2 *
                opts$cex.axis * xaxis

        ## -- yaxis marks
        YAX.default.width <-
            convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis

        YAX.width <- if (any(TYPE %in% c("dot", "hist")) &
            !ynull & !opts$internal.labels) {
            ## need to grab the factoring variable -> might be x OR y
            yf <- if (is.factor(df$data$y)) df$data$y else df$data$x
            yl <- levels(yf)
            yWidths <- sapply(
                yl,
                function(L) {
                    convertWidth(
                        grobWidth(
                            textGrob(L,
                                gp = gpar(cex = opts$cex.axis * multi.cex)
                            )
                        ),
                        "in",
                        TRUE
                    )
                }
            )
            max(yWidths)
        } else if (any(TYPE %in% c("scatter", "hex", "grid"))) {
            ax <- transform_axes(df$data$y, "y", opts,
                label = TRUE, adjust.vp = FALSE
            )
            convertWidth(
                grobWidth(
                    textGrob(ax$labs,
                        gp = gpar(cex = opts$cex.axis * multi.cex)
                    )
                ),
                "in",
                TRUE
            )
        } else {
            0
        }

        YAX.width <- ifelse(yaxis, YAX.width + YAX.default.width, 0.1)

        ## -- legend(s)
        leg.grob1 <- leg.grob2 <- leg.grob3 <- leg.grob4 <- NULL
        cex.mult <- ifelse(
            "g1" %in% df.vs,
            1,
            ifelse(
                "g1.level" %in% df.vs,
                ifelse(
                    length(levels(df$g1.level)) >= 6,
                    0.7,
                    1
                ),
                1
            )
        )


        xnum <- !xfact
        yfact <- if (ynull) FALSE else yfact
        ynum <- if (ynull) FALSE else !yfact

        col.args <- list(missing = opts$col.missing)
        if ("colby" %in% names(varnames) &&
            (any(TYPE %in% c("dot", "scatter", "hex")) ||
                (any(TYPE %in% c("grid", "hex")) && !is.null(opts$trend) &&
                    opts$trend.by) ||
                (any(TYPE == "bar") && ynull && is.factor(df$data$colby)))) {
            if (any(TYPE == "hex")) {
                df$data$colby <- convert.to.factor(df$data$colby)
            }

            if (is.factor(df$data$colby)) {
                nby <- length(levels(as.factor(df$data$colby)))
                if (length(opts$col.pt) >= nby) {
                    ptcol <- opts$col.pt[1:nby]
                } else {
                    ptcol <-
                        if (!is.null(opts$col.fun)) {
                            opts$col.fun(nby)
                        } else {
                            opts$col.default$cat(nby)
                        }
                }

                if (all(TYPE != "bar")) {
                    misscol <- any(
                        sapply(
                            plot.list,
                            function(x) sapply(x, function(y) y$nacol)
                        )
                    )
                } else {
                    misscol <- FALSE
                }

                legPch <-
                    if (barplot) {
                        22
                    } else if (!is.null(varnames$symbolby)) {
                        if (varnames$colby == varnames$symbolby) {
                            tmp <- (21:25)[1:length(levels(df$data$symbolby))]
                            if (any(is.na(df$data$symbolby))) {
                                tmp <- c(tmp, 3)
                            }
                            tmp
                        } else {
                            opts$pch
                        }
                    } else {
                        opts$pch
                    }

                leg.grob1 <- drawLegend(
                    f.levels <- levels(as.factor(df$data$colby)),
                    col = ptcol,
                    pch = legPch,
                    title = df$labels$colby %||% varnames$colby,
                    any.missing = misscol,
                    opts = opts
                )

                if (misscol) {
                    ptcol <- c(ptcol, opts$col.missing)
                    f.levels <- c(f.levels, "missing")
                }
                col.args$f.cols <- structure(ptcol, .Names = f.levels)
            } else {
                misscol <- any(
                    sapply(
                        plot.list,
                        function(x) sapply(x, function(y) y$nacol)
                    )
                )
                leg.grobL <- drawContLegend(
                    df$data$colby,
                    title = add_units(
                        df$short_labels$colby %||% varnames$colby,
                        df$units$colby
                    ),
                    height = 0.4 * PAGE.height,
                    cex.mult = cex.mult,
                    any.missing = misscol,
                    opts = opts
                )
                leg.grob1 <- leg.grobL$fg
                col.args$n.range <- range(df$data$colby, na.rm = TRUE)
                col.args$n.cols <- leg.grobL$n.cols
            }
        } else if (xfact & yfact) {
            nby <- length(levels(as.factor(df$data$y)))
            if (length(opts$col.pt) >= nby) {
                barcol <- opts$col.pt[1:nby]
            } else {
                barcol <-
                    if (!is.null(opts$col.fun)) {
                        opts$col.fun(nby)
                    } else {
                        opts$col.default$cat(nby)
                    }
            }

            leg.grob1 <- drawLegend(
                levels(as.factor(df$data$y)),
                col = barcol, pch = 22,
                title = df$short_labels$y %||% varnames$y,
                opts = opts
            )
            col.args$b.cols <- barcol
        }

        if (!is.null(locate.col)) col.args$locate.col <- locate.col

        if ("sizeby" %in% names(varnames) & any(TYPE %in% c("scatter"))) {
            misssize <- any(
                sapply(
                    plot.list,
                    function(x) sapply(x, function(x2) x2$nasize)
                )
            )
            if (misssize) {
                misstext <- paste0("missing ", varnames$sizeby)
                leg.grob2 <- drawLegend(
                    misstext,
                    col = "grey50",
                    pch = 4,
                    cex.mult = cex.mult * 0.8,
                    opts = opts
                )
            }
        }

        if (xnum & ynum) {
            df.lens <- lapply(
                plot.list,
                function(a) {
                    mm <- sapply(
                        a,
                        function(b) {
                            sum(
                                apply(
                                    cbind(b$x, b$y), 1,
                                    function(c) all(!is.na(c))
                                )
                            )
                        }
                    )
                    A <- a[[which.max(mm)]]
                    cbind(A$x, A$y)
                }
            )
            ddd <- df.lens[[which.max(sapply(df.lens, nrow))]]
            leg.grob3 <- drawLinesLegend(ddd,
                opts = opts,
                cex.mult = cex.mult * 0.8
            )
        }

        if ("symbolby" %in% names(varnames) &
            any(TYPE %in% c("scatter", "dot"))) {
            skip <- FALSE
            if (!is.null(varnames$colby)) {
                if (varnames$colby == varnames$symbolby) {
                    skip <- TRUE
                }
            }

            if (!skip) {
                legPch <- (21:25)[1:length(levels(df$data$symbolby))]
                legLvls <- levels(df$data$symbolby)
                if (any(is.na(df$data$symbolby))) {
                    legPch <- c(legPch, 3)
                    legLvls <- c(legLvls, "missing")
                }
                leg.grob4 <- drawLegend(legLvls,
                    col = rep("#333333", length(legLvls)),
                    pch = legPch,
                    title = varnames$symbolby,
                    opts = opts
                )
            }
        }

        hgts <- numeric(4)
        wdth <- 0

        if (hide.legend) {
            leg.grob1 <- leg.grob2 <- leg.grob3 <- leg.grob4 <- NULL
        }
        if (!is.null(leg.grob1)) {
            hgts[1] <- convertHeight(grobHeight(leg.grob1), "in", TRUE)
            wdth <- max(wdth, convertWidth(grobWidth(leg.grob1), "in", TRUE))
        }
        if (!is.null(leg.grob2)) {
            hgts[2] <- convertHeight(grobHeight(leg.grob2), "in", TRUE)
            wdth <- max(wdth, convertWidth(grobWidth(leg.grob2), "in", TRUE))
        }
        if (!is.null(leg.grob3)) {
            hgts[3] <- convertHeight(grobHeight(leg.grob3), "in", TRUE)
            wdth <- max(wdth, convertWidth(grobWidth(leg.grob3), "in", TRUE))
        }
        if (!is.null(leg.grob4)) {
            hgts[4] <- convertHeight(grobHeight(leg.grob4), "in", TRUE)
            wdth <- max(wdth, convertWidth(grobWidth(leg.grob4), "in", TRUE))
        }

        ## --- Figure out a subtitle for the plot:

        if (!is.null(dots$subtitle)) {
            SUB <- textGrob(
                dots$subtitle,
                gp = gpar(cex = opts$cex.text * 0.8),
                name = "inz-main-sub-bottom"
            )
        } else {
            subtitle <- ""
            if (missing.info & length(missing) > 0) {
                POS.missing <- missing[missing != 0]
                names(POS.missing) <- unlist(
                    varnames[match(names(POS.missing), names(varnames))]
                )
                missinfo <-
                    if (length(missing) > 1) {
                        paste0(
                            " (",
                            paste0(
                                POS.missing,
                                " in ",
                                names(POS.missing),
                                collapse = ", "
                            ),
                            ")"
                        )
                    } else {
                        ""
                    }

                if (total.missing > 0) {
                    subtitle <- paste0(
                        total.missing,
                        " missing values",
                        missinfo
                    )
                }
            }

            if (nOutofview > 0) {
                subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
                subtitle <- paste0(subtitle, nOutofview, " points out of view")
            } else if (!is.null(zoombars)) {
                subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
                subtitle <- paste0(
                    subtitle, zoombars[2], " out of ",
                    length(levels(df$data$x)),
                    " levels of ", varnames$x, " visible"
                )
            }

            if (any(TYPE == "bar") && !ynull &&
                opts$bar.relative.width && !opts$bar.counts) {
                subtitle <- paste(subtitle,
                    sprintf("Bar widths relative to %s counts", varnames$y),
                    "",
                    sep = ifelse(subtitle == "", "", ". ")
                )
            }

            if (subtitle == "") {
                SUB <- NULL
            } else {
                SUB <- textGrob(
                    subtitle,
                    gp = gpar(cex = opts$cex.text * 0.8),
                    name = "inz-main-sub-bottom"
                )
            }
        }


        ## --- CREATE the main LAYOUT for the titles + main plot window
        MAIN.hgt <- unit(MAIN.height, "in")
        XAX.hgt <- unit(XAX.height, "in")
        XLAB.hgt <- unit(XLAB.height, "in")
        PLOT.hgt <- unit(1, "null")
        SUB.hgt <-
            if (is.null(SUB)) {
                unit(0, "null")
            } else {
                convertUnit(grobHeight(SUB) * 2, "in")
            }

        YLAB.wd <- unit(YLAB.width, "in")
        YAX.wd <- unit(YAX.width, "in")
        PLOT.wd <- unit(1, "null")
        LEG.wd <-
            if (wdth > 0) {
                unit(wdth, "in") + unit(1, "char")
            } else {
                unit(0, "null")
            }

        TOPlayout <- grid.layout(
            nrow = 6, ncol = 5,
            heights = unit.c(
                MAIN.hgt, XAX.hgt, PLOT.hgt,
                XAX.hgt, XLAB.hgt, SUB.hgt
            ),
            widths = unit.c(
                YLAB.wd, YAX.wd, PLOT.wd,
                if (any(TYPE %in% c("scatter", "grid", "hex"))) {
                    YAX.wd
                } else {
                    unit(0.5, "in")
                }, LEG.wd
            )
        )

        ## Send the layout to the plot window
        pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))

        ## Sort out XAX height:
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
        plotWidth <- convertWidth(current.viewport()$width, "in", TRUE)
        upViewport()

        if (any(TYPE == "bar")) {
            ## If the labels are too wide, we rotate them (and shrink slightly)
            x.lev <- levels(df$data$x)
            nLabs <- length(x.lev)
            maxWd <- 0.8 * plotWidth / nLabs
            rot <- any(
                sapply(
                    x.lev,
                    function(l) {
                        convertWidth(
                            grobWidth(textGrob(l, gp = gpar(cex = opts$cex.axis))),
                            "in",
                            TRUE
                        ) > maxWd
                    }
                )
            )
            opts$rot <- rot

            # transform?
            opts$transform$y <-
                ifelse(opts$bar.counts, "bar_counts", "bar_percentage")
            if (opts$bar.counts) {
                opts$bar.n <- nrow(df$data)
            }

            if (rot) {
                ## Unable to update the viewport, so just recreate it:
                XAXht <- drawAxes(
                    df$data$x,
                    which = "x",
                    main = TRUE,
                    label = TRUE, opts,
                    heightOnly = TRUE,
                    layout.only = layout.only
                )
                XAX.hgt2 <- convertWidth(XAXht, "in")

                ## destroy the old one
                popViewport()
                TOPlayout <- grid.layout(
                    nrow = 6,
                    ncol = 5,
                    heights = unit.c(
                        MAIN.hgt,
                        XAX.hgt,
                        PLOT.hgt,
                        XAX.hgt2,
                        XLAB.hgt,
                        SUB.hgt
                    ),
                    widths = unit.c(
                        YLAB.wd,
                        YAX.wd,
                        PLOT.wd,
                        YAX.wd,
                        LEG.wd
                    )
                )

                ## Send the layout to the plot window
                pushViewport(
                    viewport(
                        layout = TOPlayout,
                        name = "VP:TOPlayout"
                    )
                )
            }
        }

        ## place the title
        pushViewport(viewport(layout.pos.row = 1))
        grid.draw(main.grob)

        ## place axis labels
        if (!is.null(titles$ylab)) {
            seekViewport("VP:TOPlayout")
            pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
            grid.draw(ylab.grob)
        }
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
        grid.draw(xlab.grob)

        ## place the legend
        if (wdth > 0) {
            seekViewport("VP:TOPlayout")
            pushViewport(viewport(layout.pos.col = 5, layout.pos.row = 3))
            leg.layout <- grid.layout(4, heights = unit(hgts, "in"))
            pushViewport(viewport(layout = leg.layout, name = "VP:LEGlayout"))

            if (hgts[1] > 0) {
                seekViewport("VP:LEGlayout")
                pushViewport(viewport(layout.pos.row = 1))
                grid.draw(leg.grob1)
            }
            if (hgts[2] > 0) {
                seekViewport("VP:LEGlayout")
                pushViewport(viewport(layout.pos.row = 2))
                grid.draw(leg.grob2)
            }
            if (hgts[3] > 0) {
                seekViewport("VP:LEGlayout")
                pushViewport(viewport(layout.pos.row = 3))
                grid.draw(leg.grob3)
            }
            if (hgts[4] > 0) {
                seekViewport("VP:LEGlayout")
                pushViewport(viewport(layout.pos.row = 4))
                grid.draw(leg.grob4)
            }
        }

        ## --- next, it will break the plot into subregions for g1
        ## (unless theres only one, then it wont)

        ## break up plot list
        if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
        if (!matrix.plot & !is.null(g2.level)) {
            plot.list <- plot.list[g2.level]
        }

        plot.list <- lapply(plot.list, function(x) x[g1.level])

        ## and subtitle
        if (!is.null(SUB)) {
            seekViewport("VP:TOPlayout")
            pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 3))
            grid.draw(SUB)
        }

        ## create a layout
        if (matrix.plot) {
            nr <- length(g2.level)
            nc <- length(g1.level)
        } else {
            dim1 <- floor(sqrt(N))
            dim2 <- ceiling(N / dim1)

            if (dev.size()[1] < dev.size()[2]) {
                nr <- dim2
                nc <- dim1
            } else {
                nr <- dim1
                nc <- dim2
            }
        }

        ## if the plots are DOTPLOTS or BARPLOTS, then leave a little bit of
        ## space between each we will need to add a small amount of space
        ## between the columns of the layout
        hspace <- ifelse(any(TYPE %in% c("scatter", "grid", "hex")), 0, 0.01)
        wds <- rep(unit.c(unit(hspace, "npc"), unit(1, "null")), nc)[-1]

        subt <- textGrob(
            "dummy text",
            gp = gpar(cex = opts$cex.lab, fontface = "bold"),
            name = "inz-dummy-txt"
        )
        sub.hgt <- unit(convertHeight(grobHeight(subt), "in", TRUE) * 1.2, "in")
        vspace <- if (matrix.plot) sub.hgt else unit(0, "in")
        hgts <- rep(unit.c(vspace, unit(1, "null")), nr)

        PLOTlayout <- grid.layout(
            nrow = length(hgts),
            ncol = length(wds),
            heights = hgts,
            widths = wds
        )
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
        pushViewport(viewport(layout = PLOTlayout, name = "VP:PLOTlayout"))

        ## --- within each of these regions, we simply plot!
        ax.gp <- gpar(cex = opts$cex.axis)

        ## --- START from the BOTTOM and work UP; LEFT and work RIGHT
        ## (mainly makes sense for continuous grouping variables)
        g1id <- 1 # keep track of plot levels
        g2id <- 1
        NG2 <- length(plot.list)
        NG1 <- length(plot.list[[1]])

        if (xfact & ynum) {
            X <- df$data$y
            Y <- df$data$x
        } else {
            X <- df$data$x
            Y <- df$data$y
        }

        for (r in nr:1) {
            R <- r * 2 # skip the gaps between rows
            if (matrix.plot) {
                ## add that little thingy
                seekViewport("VP:PLOTlayout")
                pushViewport(
                    viewport(
                        layout.pos.row = R - 1,
                        gp = gpar(cex = multi.cex, fontface = "bold")
                    )
                )
                grid.rect(
                    gp = gpar(
                        fill = rep(opts$col.sub, length = 2)[2]
                    )
                )
                grid.text(
                    paste(varnames$g2, "=", g2.level[g2id]),
                    gp = gpar(
                        cex = opts$cex.lab,
                        col = "#ffffff",
                        fontface = "bold"
                    )
                )
            }

            for (c in 1:nc) {
                ## store row and column number
                opts$rowNum <- r
                opts$colNum <- c

                if (g2id > NG2) next()
                C <- c * 2 - 1

                ## This is necessary to delete the "old" viewport so we can
                ## create a new one of the same name, but retain it long enough
                ## to use it for drawing the axes
                if (any(TYPE %in% c("dot", "hist")) & !layout.only) {
                    vp2rm <- try(
                        switch(TYPE,
                            "dot" = {
                                seekViewport("VP:dotplot-levels")
                                popViewport()
                            },
                            "hist" = {
                                seekViewport("VP:histplot-levels")
                                popViewport()
                            }
                        ),
                        silent = TRUE
                    )
                }

                seekViewport("VP:PLOTlayout")
                pushViewport(
                    viewport(
                        layout.pos.row = R,
                        layout.pos.col = C,
                        xscale = xlim,
                        yscale = ylim,
                        gp = gpar(cex = multi.cex)
                    )
                )
                ## grid.rect(gp = gpar(fill = "transparent"))

                subt <- g1.level[g1id]

                ## calculate the height of the subtitle if it is specified
                p.title <- if (subt == "all") NULL else subt
                hgt <- unit.c(
                    if (!is.null(p.title)) {
                        subt <- textGrob(
                            p.title,
                            gp = gpar(cex = opts$cex.lab, fontface = "bold"),
                            name = paste("inz-sub", r, c, sep = ".")
                        )
                        if (matrix.plot) {
                            sub.hgt
                        } else {
                            unit(convertHeight(
                                grobHeight(subt), "in", TRUE
                            ) * 2, "in")
                        }
                    } else {
                        unit(0, "null")
                    },
                    unit(1, "null")
                )
                pushViewport(
                    viewport(
                        layout = grid.layout(2, 1, heights = hgt)
                    )
                )

                ## I found "VP:locate.these.points" so far is just using here
                ## and no other depencies so I think giving the its a
                ## uniqe name would be a good idea here.
                nameVP <-
                    if (NG1 == 1 && NG2 == 1) {
                        "VP:locate.these.points"
                    } else {
                        paste0("VP:locate.these.points", g2id, g1id)
                    }
                pushViewport(
                    viewport(
                        layout.pos.row = 2,
                        xscale = xlim,
                        yscale = ylim,
                        clip = "on",
                        name = nameVP
                    )
                )

                if (!layout.only) {
                    ## background color:
                    grid.rect(
                        gp = gpar(fill = opts$bg, lty = 0),
                        name = paste("inz-plot-bg", r, c, sep = ".")
                    )
                    plot(
                        plot.list[[g2id]][[g1id]],
                        gen = list(
                            opts = opts,
                            mcex = multi.cex,
                            col.args = col.args,
                            maxcount = maxcnt,
                            LIM = c(xlim.raw, ylim.raw)
                        )
                    )
                }
                upViewport()

                if (!is.null(p.title)) {
                    pushViewport(viewport(layout.pos.row = 1))
                    grid.rect(
                        gp = gpar(fill = opts$col.sub[1]),
                        name = paste("inz-sub-bg", r, c, sep = ".")
                    )
                    grid.draw(subt)
                    upViewport()
                }

                grid.rect(
                    gp = gpar(fill = "transparent"),
                    name = paste("inz-rect-tp", r, c, sep = ".")
                )


                ## add the appropriate axes:
                ## Decide which axes to plot:

                ## -------------
                ## For dotplots + histograms: the axis are at the bottom of
                ## every column, and on the far left
                ##
                ## For scatterplots + gridplots + hexplots: the axis
                ## alternative on both axis, left and right
                ##
                ## For barplot: the axis is on the bottom of every column,
                ## and left and right of every row - also, must rotate
                ## if too big!
                ## ------------


                if (barplot) {
                    opts$bar.nmax <- BARPLOT.N[[g2id]][[g1id]]
                }

                pushViewport(
                    viewport(
                        layout.pos.row = 2,
                        xscale = xlim,
                        yscale = ylim
                    )
                )
                opts$ZOOM <- zoombars
                if (r == nr & xaxis) { # bottom
                    drawAxes(
                        X, "x", TRUE,
                        c %% 2 == 1 |
                            !any(TYPE %in% c("scatter", "grid", "hex")),
                        opts,
                        layout.only = layout.only,
                        pos = "bottom"
                    )
                }

                if (c == 1 & (!opts$internal.labels |
                    !any(TYPE %in% c("dot", "hist"))) & yaxis) { # left column
                    drawAxes(
                        if (any(TYPE == "bar")) ylim else Y,
                        "y",
                        TRUE,
                        (nr - r) %% 2 == 0,
                        opts,
                        layout.only = layout.only,
                        pos = "left"
                    )
                }

                if (!any(TYPE %in% c("dot", "hist")) & yaxis) {
                    # right column (or last plot in top row)
                    if (c == nc | g1id == NG1) {
                        drawAxes(
                            if (any(TYPE == "bar")) ylim else Y,
                            "y",
                            FALSE,
                            (nr - r) %% 2 == 1,
                            opts,
                            layout.only = layout.only,
                            pos = "right"
                        )
                    }
                }
                upViewport()

                if (any(TYPE %in% c("scatter", "grid", "hex")) & xaxis) {
                    pushViewport(
                        viewport(
                            layout.pos.row = 1,
                            xscale = xlim,
                            yscale = ylim
                        )
                    )
                    if (r == 1) {
                        drawAxes(X, "x", FALSE, c %% 2 == 0,
                            opts,
                            sub = vspace,
                            layout.only = layout.only
                        )
                    }
                    upViewport()
                }
                opts$ZOOM <- NULL

                ## update the counters
                if (g1id < NG1) {
                    g1id <- g1id + 1
                } else {
                    g1id <- 1
                    g2id <- g2id + 1
                }
            }
        }

        dev.flush()
    } else {
        ## break up plot list
        if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
        if (!matrix.plot & !is.null(g2.level)) {
            plot.list <- plot.list[g2.level]
        }

        plot.list <- lapply(plot.list, function(x) x[g1.level])
    }

    if (plot) {
        plot.list$gen <- list(
            opts = opts,
            mcex = multi.cex,
            col.args = col.args,
            maxcount = maxcnt
        )
        plot.list$xlim <- xlim
        plot.list$ylim <- ylim
    } else {
        attr(plot.list, "plotargs") <- list(
            gen = list(
                opts = opts,
                maxcount = maxcnt
            ),
            xlim = xlim,
            ylim = ylim
        )
    }

    attr(plot.list, "varnames") <- varnames
    attr(plot.list, "glevels") <- g.level
    attr(plot.list, "vartypes") <- vartypes
    attr(plot.list, "missing") <- missing
    attr(plot.list, "total.missing") <- total.missing
    attr(plot.list, "total.obs") <- total.obs
    attr(plot.list, "bootstrap") <- opts$bs.inference
    attr(plot.list, "nboot") <- opts$n.boot
    attr(plot.list, "inzclass") <- xattr$class
    attr(plot.list, "nplots") <- if (exists("N")) N else NULL

    if (xattr$class == "inz.survey") {
        attr(plot.list, "main.design") <- design
        attr(plot.list, "design") <- df.list
    }

    attr(plot.list, "plottype") <- gsub("inz", "", plot.class)
    if (any(attr(plot.list, "plottype") %in% c("dot", "hist"))) {
        attr(plot.list, "nbins") <-
            length(plot.list[[1]][[1]]$toplot[[1]]$counts)
    }

    if (itsADotplot && plot) {
        ## some recursion instructions
        ## i.e., [original.size, new.size]
        attr(plot.list, "dotplot.redraw") <-
            round(xattr$symbol.width, 5) !=
                round(convertWidth(unit(opts$cex.dotpt, "char"),
                    "native",
                    valueOnly = TRUE
                ), 5)
    }

    # plot_code <- paste(capture.output(m), collapse = "\n")
    # plot_code <- gsub("iNZightPlot(x = ", "iNZightPlot(",
    #     plot_code, fixed = TRUE)
    # attr(plot.list, "code") <- plot_code

    class(plot.list) <- "inzplotoutput"
    return(invisible(plot.list))
}
