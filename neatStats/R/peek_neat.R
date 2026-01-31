#'@title Cursory Summaries and Plots per Group
#'
#'@description Cursory summaries and plots per group.
#'@param dat Data frame (or name of data frame as string).
#'@param values String, or vector of strings: the name(s) of the column(s) in
#'  the \code{dat} data frame, containing the vector(s) of values.
#'@param group_by String, or vector of strings: the name(s) of the column(s) in
#'  the \code{dat} data frame, containing the vector(s) of factors by which the
#'  statistics are grouped.
#'@param filt An expression to filter, by column values, the entire \code{dat}
#'  data frame before performing the aggregation. The expression should use
#'  column names alone; see Examples.
#'@param sep String (comma by default) for separating group names.
#'@param collapse Decides how to handle multiple columns of \code{values}. If
#'  \code{NULL} (default), displays each column of values as separate groups.
#'  Alternatively, any function can be given using which the columns are
#'  collapsed into a single column. For example, if \code{mean} is given for
#'  this parameter, a single column will be calculated based on the means of all
#'  given values columns. (\code{NA} is always ignored.)
#'@param f_print Printing function; see details.
#'@param f_plot Plotting function; see details. (Provide string to skip
#'  plotting.)
#'@param iqr_times The multiplication of IQR to calculate Tukey's fences, when
#'  using default \code{f_print} function (set to \code{TRUE}); see Details. The
#'  default is \code{3} to spot Tukey's "far outliers" (e.g. by comparing the
#'  fences with min and max values). (Note that the usual fences, e.g. for box
#'  plots, use \code{1.5}).
#'@param round_to Number of \code{\link[neatStats:ro]{significant fractional
#'  digits to round to}}, when using default \code{f_print} function (set to
#'  \code{TRUE}).
#'@param group_n Logical. If \code{TRUE}, adds sample sizes (\code{n}) per group
#'  to plots when using default \code{f_plot} (\code{NULL}).
#'@param ... Any arguments to be passed to the \code{f_plot} function.
#'
#'@details
#'
#'If set to \code{TRUE}, prints to console the following data (per group):
#'\code{mean}; 95% CI of the mean as \code{ci_low} and \code{ci_upp}; \code{sd};
#'\code{median}; \code{quantile_1st} and \code{quantile_3rd} (first and third
#'quantiles); "Tukey's fences" as \code{fence_low} and \code{fence_upp}; minimum
#'and maximum values (\code{min}, \code{max}); number of \code{NA}s (\code{na}).
#'Tukey's fences are the upper and lower limits with distances of \code{X} times
#'the \code{\link[stats]{IQR}} from the actual IQR, where \code{X} is specified
#'via the \code{iqr_times} parameter. Returns (invisibly) the same values,
#'unrounded, via a data frame. If alternative \code{f_print} is given, prints
#'whatever value is returned from the given function (and attempts, if possible,
#'to create a data frame).
#'
#'Creates and displays box plot(s) (per group) by default, along with overlayed
#'violin plot (densities proportionate to sample sizes). If alternative
#'\code{f_plot} is given, the first argument will be the values per group, and
#'all plots will be \code{\link[ggpubr:ggarrange]{arranged}} into a single plot
#'and displayed together. To skip plotting, just give any character as argument
#'(e.g. \code{"none"} or just \code{""}).
#'
#'@return Data frame with the printed values (if possible).
#'
#' @examples
#'
#' data("mtcars") # load base R example dataset
#'
#' # overall info for wt (Weight)
#' peek_neat(mtcars, 'wt', f_print = TRUE)
#' #'
#' # now groupped by cyl (Number of cylinders)
#' peek_neat(mtcars, 'wt', group_by = 'cyl')
#'
#' # grouped by cyl and gear
#' peek_neat(mtcars,
#'           'wt',
#'           group_by = c('cyl', 'gear'),
#'           f_print = TRUE)
#'
#' # filter to only have cyl larger than  4
#' peek_neat(mtcars, 'wt', group_by = 'cyl', filt = cyl > 4)
#'
#' # without plots
#' peek_neat(mtcars,
#'           'wt',
#'           group_by = 'cyl',
#'           f_plot = "",
#'           f_print = TRUE)
#'
#' # with histogramms etc, using plot_neat
#' peek_neat(mtcars, 'wt', group_by = 'cyl', f_plot = plot_neat)
#'
#' # with Q-Q plots, via ggpubr
#' peek_neat(mtcars,
#'           'wt',
#'           group_by = 'cyl',
#'           f_plot = ggpubr::ggqqplot)
#'
#' # skewness and kurtosis data via psych
#' \dontrun{
#' info_df = peek_neat(
#'     mtcars,
#'     'wt',
#'     group_by = 'cyl',
#'     f_print = psych::describe,
#'     f_plot = ""
#' )
#' info_df # contains all data returns by psych::describe
#'}
#'
#' @export
peek_neat = function(dat,
                     values,
                     group_by = NULL,
                     filt = NULL,
                     sep = ", ",
                     collapse = NULL,
                     f_print = FALSE,
                     f_plot = NULL,
                     iqr_times = 3,
                     round_to = 4,
                     group_n = TRUE,
                     ...) {
    if (typeof(dat) == "character") {
        dat = eval(parse(text = dat))
    }
    validate_args(
        match.call(),
        list(
            val_arg(dat, c('df')),
            val_arg(group_by, c('null', 'char')),
            val_arg(sep, c('char')),
            val_arg(collapse, c('function', 'null')),
            val_arg(f_print, c('function', 'bool')),
            val_arg(f_plot, c('function', 'null', 'char')),
            val_arg(iqr_times, c('num'), 1),
            val_arg(round_to, c('num'), 1),
            val_arg(group_n, c('bool'), 1)
        )
    )
    name_taken('..neat_values', dat)
    values = values[!values %in% group_by]
    if (length(values) > 1) {
        if (is.null(collapse)) {
            dat = stats::reshape(
                dat,
                direction = 'long',
                varying = values,
                timevar = "within_factor",
                v.names = "..neat_values",
                times = values
            )
            if (is.null(group_by)) {
                group_by = "within_factor"
            } else {
                group_by = c(group_by, "within_factor")
            }
        } else {
            dat$..neat_values = apply(subset(dat, select = values),
                                      1, collapse, na.rm = TRUE)
        }
    } else if (values %in% names(dat)) {
        dat$..neat_values = dat[[values]]
    } else {
        stop("Column name specified for values not found.")
    }
    filt = paste(deparse(substitute(filt)), collapse = "")
    if (filt != "NULL") {
        if (startsWith(filt, "'") | startsWith(filt, '"')) {
            stop('The argument "filt" must be an expression (not string).')
        }
        filt_vec = eval(parse(text = paste0('with(data = dat, ',
                                            filt,
                                            ')')))
        na_sum = sum(is.na(filt_vec))
        if (na_sum > 0) {
            message(
                'Note: ',
                na_sum,
                ' NA values were replaced as FALSE for filtering.',
                ' You may want to double-check your filtering expression.'
            )
            filt_vec[is.na(filt_vec)] = FALSE
        }
        dat = dat[filt_vec,]

    }
    if (!is.null(group_by)) {
        group_by = as.factor(eval(parse(
            text = paste0(
                'with(data = dat, paste(',
                paste(group_by, collapse = ','),
                ', sep = "',
                sep,
                '"))'
            )
        )))
    } else {
        group_by = as.factor(rep('0', nrow(dat)))
    }
    dat_merg = data.frame()
    vals = dat$..neat_values
    plot_list = list()
    for (grp in unique(group_by)) {
        valstemp = vals[group_by == grp]
        if (!isFALSE(f_print)) {
            if (length(unique(group_by)) > 1) {
                cat(grp, ':', fill = TRUE, sep = '')
            }
            if (isTRUE(f_print)) {
                to_merg = sum_neat(valstemp,
                                   iqr_times = iqr_times,
                                   round_to = round_to)
            } else {
                to_merg = f_print(valstemp)
                print(to_merg)
            }
            if (inherits(to_merg, "data.frame") |
                length(names(to_merg)[(names(to_merg) != "")]) == length(to_merg)) {
                dat_merg = rbind(dat_merg, data.frame(as.list((
                    to_merg
                ))))
            }
        } else {
            dat_merg = NULL
        }
        valstemp = valstemp[!is.na(valstemp)]
        if (!is.null(f_plot) &&
            length(valstemp) > 0 &&
            (!inherits(f_plot, "character"))) {
            if (group_n == TRUE) {
                xtitl = paste0(grp, '\n(n = ',
                               length(valstemp), ')')
            } else {
                xtitl = grp
            }
            plot_list[[length(plot_list) + 1]] = f_plot(valstemp, ...) +
                xlab(xtitl)
        }
    }
    if (is.null(f_plot)) {
        graphics::plot(box_neat(vals, group_by, group_n = group_n))
    } else if (!inherits(f_plot, "character")) {
        graphics::plot(ggpubr::ggarrange(plotlist = plot_list))
    }
    invisible(dat_merg)
}

sum_neat = function(numvec, iqr_times, round_to) {
    quantile_1st = as.numeric(stats::quantile(numvec, .25, na.rm = TRUE))
    quantile_3rd = as.numeric(stats::quantile(numvec, .75, na.rm = TRUE))
    mycis = neatStats::mean_ci(numvec, distance_only = FALSE)
    out = c(
        n = length(numvec),
        mean = mean(numvec, na.rm = TRUE),
        ci_low = as.numeric(mycis[1]),
        ci_upp = as.numeric(mycis[2]),
        sd = stats::sd(numvec, na.rm = TRUE),
        median = stats::median(numvec, na.rm = TRUE),
        quantl_1st = quantile_1st,
        quantl_3rd = quantile_3rd,
        fence_low = quantile_1st - iqr_times * (quantile_3rd - quantile_1st),
        fence_upp = iqr_times * (quantile_3rd - quantile_1st) + quantile_3rd,
        min = min(numvec, na.rm = TRUE),
        max = max(numvec, na.rm = TRUE),
        na = sum(is.na(numvec))
    )
    to_print = as.numeric(ro(out, round_to, signi = TRUE))
    names(to_print) = names(out)
    print(to_print)
    invisible(out)
}

box_neat = function(values, group, group_n = TRUE) {
    plot_dat = data.frame(values = values, group = group)
    plot_dat = plot_dat[!is.na(plot_dat$values),]
    if (group_n == TRUE) {
        plot_dat$group = paste0(plot_dat$group, '\n(n = ',
                                table(plot_dat$group)[plot_dat$group], ')')
    }
    ggplot(plot_dat, aes(x = group,
                         y = values)) +
        stat_boxplot(geom = 'errorbar', width = 0.5) +
        geom_boxplot(fill = '#cdebd0', outlier.shape = NA) +
        geom_violin(color = '#9f9fcc',
                    alpha = 0.2,
                    scale = 'count') +
        geom_boxplot(
            outlier.shape = 4,
            outlier.size = 3,
            outlier.stroke = 0.8,
            alpha = 1,
            fill = NA
        ) +
        theme_bw() + xlab(NULL) + ylab(NULL)
}
