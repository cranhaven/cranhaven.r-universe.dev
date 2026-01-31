#'@title Plots of Means and of Dispersion
#'
#'@description Primarily for line and bar \code{\link[ggplot2:ggplot]{plots}}
#'  for factorial designs. Otherwise (if no \code{data_per_subject} is given)
#'  descriptive dispersion plots (histogram, density, or box plots) for a
#'  continuous variable. (For the latter, only the parameters \code{values},
#'  \code{parts}, \code{part_colors}, and \code{binwidth} are used, the rest are
#'  ignored.)
#'@param data_per_subject Data frame containing all values
#'  (measurements/observations for a factorial design) in a single row per each
#'  subject. Otherwise, if no data frame is given (default: \code{NULL}),
#'  histogram, density, or box plots will be returned for a continuous variable
#'  (numeric vector).
#'@param values For plots of means (factorial designs): vector of strings;
#'  column name(s) in the \code{data_per_subject} data frame. Each column should
#'  contain a single dependent variable: thus, to plot repeated (within-subject)
#'  measurements, each specified column should contain one measurement. For
#'  descriptive dispersion plots (if \code{data_per_subject} is \code{NULL}), a
#'  numeric vector is expected.
#'@param within_ids \code{NULL} (default), string, or named list. In case of no
#'  within-subject factors, leave as \code{NULL}. In case of a single within
#'  subject factor, a single string may be given to optionally provide custom
#'  name for the within-subject factor (note: this is a programming variable
#'  name, so it should not contain spaces, etc.); otherwise (if left
#'  \code{NULL}) this one within-subject factor will always just be named
#'  \code{"within_factor"}. In case of multiple within-subject factors, each
#'  factor must be specified as a named list element, each with a vector of
#'  strings that distinguish the levels within that factors. The column names
#'  given as \code{values} should always contain one (and only one) of these
#'  strings within each within-subject factor, and thus they will be assigned
#'  the appropriate level. For example, \code{values = 'rt_s1_neg, rt_s1_pos,
#'  rt_s2_neg, rt_s2_pos'} could have \code{within_ids = list( session = c('s1',
#'  's2'), valence =  c('pos', 'neg')}. (Note: the strings for distinguishing
#'  must be unambiguous. E.g., for values \code{apple_a} and \code{apple_b}, do
#'  not set levels \code{c('a','b')}, because \code{'a'} is also found in
#'  \code{apple_b}. In this case, you could choose levels \code{c('_a','_b')} to
#'  make sure the values are correctly distinguished.) See also Examples.
#'@param between_vars \code{NULL} (default; in case of no between-subject
#'  factors) or vector of strings; column name(s) in the \code{data_per_subject}
#'  data frame. Each column should contain a single between-subject independent
#'  variable (representing between-subject factors).
#'@param factor_names \code{NULL} or named vector. In a named vector, factor
#'  names (either within or between) can be given a different name for display,
#'  in a dictionary style, using original factor name as the name of a vector
#'  element, and the element's value (as string) for the new name. For example,
#'  to change a factor named \code{"condition"} to \code{"High vs. low
#'  arousal"}, the vector may be given (in this case with a single element) as
#'  \code{factor_names = c(condition = "High vs. low arousal")}.
#'@param value_names \code{NULL} or named vector. Same as \code{factor_names},
#'  but regarding the factor values. For example, to change values
#'  \code{"high_a"} and \code{"low_a"} to \code{"High"} and \code{"Low"} for
#'  display, the vector may be given as \code{value_names = c(high_a = "High",
#'  low_a = "Low")}.
#'@param y_title \code{NULL} (default) or string. Optionally given title for the
#'  \code{y} axis.
#'@param reverse Logical (default: \code{FALSE}). If \code{TRUE}, reverses the
#'  default grouping of variables within the figure, or within each panel, in
#'  case of multiple panels. (The default grouping is decided automatically by
#'  given factor order, but always starting, when applicable, with
#'  within-subject factors: first factor is split to adjacent bars, or
#'  vertically aligned dots in case of line plot.)
#'@param panels \code{NULL} or string. Optionally gives the factor name by which
#'  the plot is to be split into different panels, in case of three factors. (By
#'  default, the third given factor is used.)
#'@param type Strong: \code{"line"} (default) or \code{"bar"}. The former gives
#'  line plot, the latter gives bar plot.
#'@param dodge Number. Specifies the amount by which the adjacent bars or dots
#'  '\code{\link[ggplot2:position_dodge]{dodge}}' each other (i.e., are
#'  displaced compared to each other). (Default is \code{0.1} for \code{line}
#'  plots, and \code{0.9} for \code{bar} plots.)
#'@param bar_colors Vector of strings, specifying colors from which all colors
#'  for any number of differing adjacent bars are interpolated. (If the number
#'  of given colors equal the number of different bars, the precise colors will
#'  correspond to each bar.) The default \code{'viridis'} gives a color gradient
#'  based on \code{\link[viridis:viridis]{viridis}}. (In case of a single
#'  factor, the first given colors is taken.)
#'@param line_colors Vector of strings, specifying colors from which all colors
#'  for any number of differing vertically aligned dots and corresponding lines
#'  are interpolated. The default \code{'viridis'} gives a color gradient based
#'  on \code{\link[viridis:viridis]{viridis}}. (In case of a single factor, the
#'  first given colors is taken.)
#'@param row_number Number. In case of multiple panels, the number of rows in
#'  which the panels should be arranged. For example, with the default
#'  \code{row_number = 1}, all panels will be displayed in one vertically
#'  aligned row.
#'@param method A function (default: \code{mean}) for the calculation of the
#'  main statistics (bar or dot heights).
#'@param eb_method A function (default: \code{\link{mean_ci}} for 95% CI) for
#'  the calculation of the error bar size (as a single value used for both
#'  directions of the error bar). If set to \code{NULL}, no error bar is
#'  displayed.#'
#'@param numerics If \code{FALSE} (default), returns
#'  \code{\link[ggplot2]{ggplot}} object. If set to \code{TRUE}, returns only
#'  the numeric aggregated data per grouping factors, as specified by
#'  \code{method} and \code{eb_method} functions. If set to any string (e.g.
#'  \code{"both"}), returns the numeric aggregated data and at the same time
#'  \code{\link[graphics:plot]{draws}} the plot.
#'@param hush Logical. If \code{TRUE}, prevents printing aggregated values.
#'@param parts For dispersion plots only (if no \code{data_per_subject} is
#'  given). A vector of characters that specify which types of overlayed types
#'  to plot: \code{"h"} for histogram, \code{"d"} for density, \code{"n"}
#'  normally distributed density (using the mean and standard deviation of the
#'  given variable), \code{"b"} for boxplot. (All are included by default:
#'  \code{parts = c("h", "d", "n", "b")}).
#'@param part_colors For dispersion plots only (if no \code{data_per_subject} is
#'  given). A named that can specify and thereby override default colors and
#'  alpha (transparency) of each plot type. Colors can be given by adding "c" to
#'  the plot type letter, e.g. \code{c(hc = "blue")} for blue histogram. Alpha
#'  can be given by adding "a" to the plot type letter, e.g. \code{c(ha = 0)}
#'  for completely transparent histogram. Any number may be given: e.g. a dark
#'  red transparent histogram with green boxplot would be \code{part_colors =
#'  c(hc = "#cc0000", ha = 0.1, bc = "green")}.
#'@param binwidth For dispersion plots only (if no \code{data_per_subject} is
#'  given). Binwidth for histograms. If \code{NULL} (default), Freedman–Diaconis
#'  rule is used if it produces at least 10 bins – otherwise 1bandwidth is
#'  calculated for 10 bins.
#'
#'@return By default, a \code{\link[ggplot2]{ggplot}} plot object. (This object
#'  may be further modified or adjusted via regular
#'  \code{\link[ggplot2]{ggplot}} methods.) If so set (\code{numerics}),
#'  aggregated values as specified by the methods.
#'
#' @note More than three factors is not allowed: it would make little sense and
#'   it would be difficult to clearly depict in a simple figure. (However, you
#'   can build an appropriate graph using \code{\link[ggplot2]{ggplot}}
#'   directly; but you can also just divide the data to produce several
#'   three-factor plots, after which you can use e.g. \code{ggpubr}'s
#'   \code{ggarrange} to easily collate the plots.)
#'
#' @seealso \code{\link{anova_neat}}, \code{\link{mean_ci}}, \code{\link{se}}
#' @examples
#'
#' # assign random data in a data frame for illustration
#' # (note that the 'subject' is only for illustration; since each row contains the
#' # data of a single subject, no additional subject id is needed)
#' dat_1 = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
#'     grouping1 = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
#'     grouping2 = c(1, 2, 1, 2, 2, 1,2, 1,2,1, 1, 1, 2, 1),
#'     value_1_a = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1,
#'                   31, 16.9, 40.1, 42.1, 41, 12.9),
#'     value_2_a = c(-14.1, 58.5,-25.5, 42.2,-13, 4.4, 55.5,-28.5,
#'                   25.6,-37.1, 55.1,-38.5, 28.6,-34.1),
#'     value_1_b = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85,
#'                   132, 121, 151, 95),
#'     value_2_b = c(8.024,-14.162, 3.1,-2.1,-1.5, 0.91, 11.53,
#'                   18.37, 0.3,-0.59, 12.53, 13.37, 2.3,-3),
#'     value_1_c = c(27.4, -17.6, -32.7, 0.4, 37.2, 1.7, 18.2, 8.9,
#'                   1.9, 0.4, 2.7, 14.2, 3.9, 4.9),
#'     value_2_c = c(7.7,-0.8, 2.2, 14.1, 22.1,-47.7,-4.8, 8.6,
#'                   6.2, 18.2,-6.8, 5.6, 7.2, 13.2)
#' )
#' head(dat_1) # see what we have
#'
#' # plot for factors 'grouping1', 'grouping2'
#' plot_neat(
#'     data_per_subject = dat_1,
#'     values = 'value_1_a',
#'     between_vars = c('grouping1', 'grouping2')
#' )
#'
#' # same as above, but with bars and renamed factors
#' plot_neat(
#'     data_per_subject = dat_1,
#'     values = 'value_1_a',
#'     between_vars = c('grouping1', 'grouping2'),
#'     type = 'bar',
#'     factor_names = c(grouping1 = 'experimental condition', grouping2 = 'gender')
#' )
#'\donttest{
#' # same, but with different (lighter) gray scale bars
#' plot_neat(
#'     dat_1,
#'     values = 'value_1_a',
#'     between_vars = c('grouping1', 'grouping2'),
#'     type = 'bar',
#'     factor_names = c(grouping1 = 'experimental condition', grouping2 = 'gender'),
#'     bar_colors = c('#555555', '#BBBBBB')
#' )
#'
#' # same, but with red and blue bars
#' plot_neat(
#'     dat_1,
#'     values = 'value_1_a',
#'     between_vars = c('grouping1', 'grouping2'),
#'     type = 'bar',
#'     factor_names = c(grouping1 = 'experimental condition', grouping2 = 'gender'),
#'     bar_colors = c('red', 'blue') # equals c('#FF0000', '#0000FF')
#' )
#'
#' # within-subject factor for 'value_1_a' vs. 'value_1_b' vs. 'value_1_c'
#' # (automatically named 'within_factor'), between-subject factor 'grouping1'
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = c('grouping1', 'grouping2')
#' )
#'}
#' # same, but panelled by 'within_factor'
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = c('grouping1', 'grouping2'),
#'     panels = 'within_factor'
#' )
#'\donttest{
#' # same, but SE for error bars instead of (default) SD
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = c('grouping1', 'grouping2'),
#'     panels = 'within_factor',
#'     eb_method = se
#' )
#'
#' # same, but 95% CI for error bars instead of SE
#' # (arguably more meaningful than SEs)
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = c('grouping1', 'grouping2'),
#'     panels = 'within_factor',
#'     eb_method = mean_ci
#' )
#'
#' # same, but using medians and Median Absolute Deviations
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = c('grouping1', 'grouping2'),
#'     panels = 'within_factor',
#'     method = stats::median,
#'     eb_method = stats::mad
#' )
#'}
#' # within-subject factor 'number' for variables with number '1' vs. number '2'
#' # ('value_1_a' and 'value_1_b' vs. 'value_2_a' and 'value_2_b'), factor 'letter'
#' # for variables with final letter 'a' vs. final letter 'b' ('value_1_a' and
#' # 'value_2_a' vs. 'value_1_b' and 'value_2_b')
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     )
#' )
#'\donttest{
#' # same as above, but now including between-subject factor 'grouping2'
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     between_vars = 'grouping2'
#' )
#'}
#' # same as above, but renaming factors and values for display
#' plot_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     between_vars = 'grouping2',
#'     factor_names = c(numbers = 'session (first vs. second)'),
#'     value_names = c(
#'         '_1' = 'first',
#'         '_2' = 'second',
#'         '1' = 'group 1',
#'         '2' = 'group 2'
#'     )
#' )
#'\donttest{
#' # In real datasets, these could of course be more meaningful. For example, let's
#' # say participants rated the attractiveness of pictures with low or high levels
#' # of frightening and low or high levels of disgusting qualities. So there are
#' # four types of ratings:
#' # 'low disgusting, low frightening' pictures
#' # 'low disgusting, high frightening' pictures
#' # 'high disgusting, low frightening' pictures
#' # 'high disgusting, high frightening' pictures
#'
#' # this could be meaningfully assigned e.g. as below
#' pic_ratings = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     rating_fright_low_disgust_low = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
#'     rating_fright_high_disgust_low = c(-14.1, 58.5,-25.5, 42.2,-13, 4.4, 55.5,-28.5, 25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162, 3.1,-2.1,-1.5, 0.91, 11.53, 18.37, 0.3,-0.59)
#' )
#' head(pic_ratings) # see what we have
#'
#' # the same logic applies as for the examples above, but now the
#' # within-subject differences can be more meaningfully specified, e.g.
#' # 'disgust_low' vs. 'disgust_high' for levels of disgustingness, while
#' # 'fright_low' vs. 'fright_high' for levels of frighteningness
#' plot_neat(
#'     pic_ratings,
#'     values = c(
#'         'rating_fright_low_disgust_low',
#'         'rating_fright_high_disgust_low',
#'         'rating_fright_low_disgust_high',
#'         'rating_fright_high_disgust_high'
#'     ),
#'     within_ids = list(
#'         disgustingness = c('disgust_low', 'disgust_high'),
#'         frighteningness =  c('fright_low', 'fright_high')
#'     )
#' )
#'
#' # now let's say the ratings were done in two separate groups
#' pic_ratings = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     group_id = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
#'     rating_fright_low_disgust_low = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
#'     rating_fright_high_disgust_low = c(-14.1, 58.5,-25.5, 42.2,-13, 4.4, 55.5,-28.5, 25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162, 3.1,-2.1,-1.5, 0.91, 11.53, 18.37, 0.3,-0.59)
#' )
#'
#' # now include the 'group_id' factor in the plot
#' plot_neat(
#'     pic_ratings,
#'     values = c(
#'         'rating_fright_low_disgust_low',
#'         'rating_fright_high_disgust_low',
#'         'rating_fright_low_disgust_high',
#'         'rating_fright_high_disgust_high'
#'     ),
#'     within_ids = list(
#'         disgustingness = c('disgust_low', 'disgust_high'),
#'         frighteningness =  c('fright_low', 'fright_high')
#'     ),
#'     between_vars = 'group_id'
#' )
#'}
#'
#'## DISPERSION PLOTS
#'
#'plot_neat(values = rnorm(100))
#'
#'# with smaller binwidth (hence more bins)
#'plot_neat(values = rnorm(100), binwidth = 0.2)
#'
#'# without normal distribution line
#'plot_neat(values = rnorm(100), parts = c('h', 'd', 'b'))
#'
#'# without histrogram
#'plot_neat(values = rnorm(100), parts = c('d', 'n', 'b'))
#'
#'# blue density, fully opaque histogram
#'plot_neat(values = rnorm(100),
#'          part_colors = c(dc = 'blue', ha = 1))
#'
#' @export
plot_neat = function(data_per_subject = NULL,
                     values = NULL,
                     within_ids = NULL,
                     between_vars = NULL,
                     factor_names = NULL,
                     value_names = NULL,
                     y_title = NULL,
                     reverse = FALSE,
                     panels = NULL,
                     type = 'line',
                     dodge = NULL,
                     bar_colors = 'viridis',
                     line_colors = 'viridis',
                     row_number = 1,
                     method = mean,
                     eb_method = neatStats::mean_ci,
                     numerics = FALSE,
                     hush = FALSE,
                     parts = c('h', 'd', 'n', 'b'),
                     part_colors = NULL,
                     binwidth = NULL) {
    if (is.null(values) &&
        is.atomic(data_per_subject)) {
        values = data_per_subject
        data_per_subject = NULL
    }
    if (is.null(data_per_subject)) {
        return(
            neat_plot2(
                values = values,
                parts = parts,
                part_colors = part_colors,
                binwidth = binwidth
            )
        )
    }
    data_wide = data_per_subject
    validate_args(
        match.call(),
        list(
            val_arg(data_per_subject, c('df')),
            val_arg(values, c('char')),
            val_arg(within_ids, c('null', 'char', 'list'), 1),
            val_arg(between_vars, c('null', 'char')),
            val_arg(factor_names, c('null', 'char')),
            val_arg(value_names, c('null', 'char')),
            val_arg(y_title, c('null', 'char'), 1),
            val_arg(reverse, c('bool'), 1),
            val_arg(panels, c('null', 'char'), 1),
            val_arg(type, c('char'), 1, c('bar', 'line')),
            val_arg(dodge, c('null', 'num')),
            val_arg(bar_colors, c('char')),
            val_arg(line_colors, c('char')),
            val_arg(row_number, c('num'), 1),
            val_arg(method, c('function'), 1),
            val_arg(eb_method, c('null', 'function'), 1),
            val_arg(numerics, c('bool', 'char'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    str_meth = utils::tail(strsplit(paste(deparse(
        substitute(method)
    ), collapse = ""), '::', fixed = TRUE)[[1]], n = 1)
    str_ebmeth = utils::tail(strsplit(paste(deparse(
        substitute(eb_method)
    ), collapse = ""), '::', fixed = TRUE)[[1]], n = 1)

    cols_notfound = c()
    if (!is.null(between_vars)) {
        for (colname in between_vars) {
            if (!colname %in% names(data_per_subject)) {
                cols_notfound = c(cols_notfound, colname)
            }
        }
        between_vars = paste(between_vars, collapse = ',')
    }
    for (colname in values) {
        if (!colname %in% names(data_per_subject)) {
            cols_notfound = c(cols_notfound, colname)
        }
    }
    if (length(cols_notfound) > 0) {
        if (length(cols_notfound) ==  1) {
            stop(
                'The column "',
                cols_notfound,
                '" was not found in the data frame. Perhaps check for spelling mistakes.'
            )
        } else {
            stop(
                'The following columns were not found in the data frame: "',
                paste(cols_notfound,
                      collapse = '", "'),
                '". Perhaps check for spelling mistakes.'
            )
        }
    }
    val_levels = val_wi_id(match.call(), within_ids, values)
    # collapsing
    fac_dups = unique(val_levels[duplicated(val_levels)])
    if (length(fac_dups) > 0) {
        message('Columns with identical factors were found!',
                ' Make sure this is how you want it:')
        for (dup in fac_dups) {
            to_collapse = names(val_levels)[val_levels == dup]
            message(
                'The columns "',
                paste(to_collapse, collapse = '", "'),
                '" were collapsed into one column',
                ' (using their mean value per observation).'
            )
            data_wide[[dup]] = rowMeans(data_wide[, to_collapse], na.rm =
                                            TRUE)
            values = values[!(values %in% to_collapse)]
            values = c(values, dup)
        }
    }
    # end collapsing
    name_taken('..neat_values', data_wide)
    name_taken('..neat_id', data_wide)
    id_col = '..neat_id'
    data_wide[[id_col]] = as.character(seq.int(nrow(data_wide)))
    if (length(values) > 1) {
        data_reshaped = stats::reshape(
            data_wide,
            direction = 'long',
            varying = values,
            idvar = id_col,
            timevar = "within_factor",
            v.names = "..neat_values",
            times = values
        )
        if (length(within_ids) > 1) {
            for (fact_name in names(within_ids)) {
                data_reshaped[[fact_name]] = fact_name
                for (fact_x in within_ids[[fact_name]]) {
                    data_reshaped[[fact_name]][grepl(fact_x,
                                                     data_reshaped$within_factor,
                                                     fixed = TRUE)] = fact_x
                }
                data_reshaped[[fact_name]] = as.factor(data_reshaped[[fact_name]])
            }
            within_vars = paste(names(within_ids), collapse = ', ')
        } else if (is.list(within_ids)) {
            within_vars = names(within_ids)
            names(data_reshaped)[names(data_reshaped) == 'within_factor'] = names(within_ids)
        }  else if (is.character(within_ids)) {
            within_vars = within_ids
            names(data_reshaped)[names(data_reshaped) == 'within_factor'] = within_ids
        } else {
            within_vars = 'within_factor'
        }
        this_data = data_reshaped
    } else {
        this_data = data_wide
        colnames(this_data)[colnames(this_data) == values] = '..neat_values'
        within_vars = NULL
    }
    this_data[, id_col] = to_fact(this_data[[id_col]])

    if (is.null(between_vars)) {
        g_by = within_vars
    } else if (is.null(within_vars)) {
        g_by = between_vars
    } else {
        g_by = paste(within_vars, between_vars, sep = ',')
    }
    onefact = FALSE
    to_plot = mains_ebs(
        data_long = this_data,
        method = method,
        eb_method = eb_method,
        g_by = g_by
    )
    fact_names = to_c(g_by)
    names(to_plot)[1:length(fact_names)] = fact_names
    if (!is.null(value_names)) {
        i = sapply(to_plot, is.factor)
        to_plot[i] = lapply(to_plot[i], as.character)
        for (v_name in names(value_names)) {
            to_plot[to_plot == v_name] = value_names[v_name]
            values[values == v_name] = value_names[v_name]
            for (fact_n in names(within_ids)) {
                within_ids[[fact_n]][within_ids[[fact_n]] == v_name] = value_names[v_name]
            }
        }
    }
    if (!is.null(panels) &&
        panels %in% fact_names && length(fact_names) == 3) {
        fact_names = c(fact_names[!fact_names == panels], panels)
    }
    if (reverse == TRUE && onefact != TRUE) {
        fact_names[c(1, 2)] = fact_names[c(2, 1)]
    }
    if (!is.null(within_vars)) {
        if (length(within_ids) > 1) {
            for (fact_n in names(within_ids)) {
                to_plot[[fact_n]] = factor(to_plot[[fact_n]], levels = within_ids[[fact_n]])
            }
        } else if (is.list(within_ids)) {
            to_plot[[names(within_ids)]] = factor(to_plot[[names(within_ids)]], levels = values)
        } else if (is.character(within_ids)) {
            to_plot[[within_ids]] = factor(to_plot[[within_ids]], levels = values)
        } else {
            to_plot[['within_factor']] = factor(to_plot[['within_factor']], levels = values)
        }
    }
    tots = to_plot
    names(tots)[names(tots) == 'x.main'] = str_meth
    names(tots)[names(tots) == 'x.eb'] = str_ebmeth
    if (numerics == TRUE) {
        return(tots)
    }
    if (hush == FALSE) {
        print(tots)
    }
    if (length(to_c(g_by)) > 3) {
        message("Maximum three factors can be plotted. See help(plot_neat)")
        return(NULL)
    } else if (length(to_c(g_by)) < 2) {
        onefact = TRUE
    }
    p_close = fact_names[1]
    if (is.null(dodge)) {
        if (type == 'bar') {
            dodge = 0.9
        } else {
            dodge = 0.1
        }
    }
    if (onefact == TRUE) {
        p_mid = fact_names[1]
        if (type == 'line')  {
            if (substr(line_colors[1], 1, 1) == 'v')  {
                line_colors = '#333333'
            }
            the_plot = ggplot2::ggplot(data = to_plot,
                                       aes(
                                           x = .data[[p_close]],
                                           y = .data$x.main,
                                           group = 1
                                       )) +
                geom_line(color = line_colors[1]) + geom_point(color = line_colors[1])
        } else {
            if (substr(bar_colors[1], 1, 1) == 'v')  {
                bar_colors = '#333333'
            }
            the_plot = ggplot2::ggplot(data = to_plot,
                                       aes(
                                           x = .data[[p_close]],
                                           y = .data$x.main,
                                           group = 1
                                       )) +
                geom_bar(stat = "identity",
                         color = "black",
                         fill = bar_colors[1])
        }
    } else {
        colornum = length(unique(to_plot[[p_close]]))
        p_mid = fact_names[2]
        if (type == 'line') {
            if (substr(line_colors[1], 1, 1) == 'v')  {
                if (colornum == 2) {
                    palcolors = viridis::viridis(colornum, end = 0.5)
                } else if (colornum == 3) {
                    palcolors = viridis::viridis(colornum, end = 0.7)
                } else {
                    palcolors = viridis::viridis(colornum, end = 0.85)
                }
            } else {
                color_gen = grDevices::colorRampPalette(line_colors)
                palcolors = color_gen(colornum)
            }
            the_plot = ggplot2::ggplot(data = to_plot,
                                       aes(
                                           x = .data[[p_mid]],
                                           y = .data$x.main,
                                           group = .data[[p_close]]
                                       )) +
                geom_line(aes(linetype = .data[[p_close]], color = .data[[p_close]]),
                          position = position_dodge(dodge)) +
                geom_point(aes(shape = .data[[p_close]], color = .data[[p_close]]),
                           position = position_dodge(dodge))  +
                scale_shape_discrete(name = re_n(p_close, factor_names)) +
                scale_linetype_discrete(name = re_n(p_close, factor_names)) +
                scale_color_manual(values = palcolors,
                                   name = re_n(p_close, factor_names))
        } else {
            if (substr(bar_colors[1], 1, 1) == 'v')  {
                if (colornum == 2) {
                    palcolors = viridis::viridis(colornum, end = 0.5)
                } else if (colornum == 3) {
                    palcolors = viridis::viridis(colornum, end = 0.7)
                } else {
                    palcolors = viridis::viridis(colornum, end = 0.85)
                }
            } else {
                color_gen = grDevices::colorRampPalette(bar_colors)
                palcolors = color_gen(colornum)
            }
            the_plot = ggplot2::ggplot(data = to_plot,
                                       aes(
                                           x = .data[[p_mid]],
                                           y = .data$x.main,
                                           fill = .data[[p_close]]
                                       )) +
                geom_bar(
                    stat = "identity",
                    color = "black",
                    position = position_dodge(dodge)
                ) +
                scale_fill_manual(values = palcolors,
                                  name = re_n(p_close, factor_names))
        }
    }
    if (!is.null(eb_method)) {
        if (type == 'line') {
            if (onefact == TRUE) {
                the_plot = the_plot + geom_errorbar(
                    aes(
                        ymin = .data$x.main - .data$x.eb,
                        ymax = .data$x.main + .data$x.eb,
                        width = 0.2
                    ),
                    color = line_colors[1],
                    position = position_dodge(dodge)
                )
            } else {
                the_plot = the_plot + geom_errorbar(
                    aes(
                        ymin = .data$x.main - .data$x.eb,
                        ymax = .data$x.main + .data$x.eb,
                        width = 0.2,
                        color = .data[[p_close]]
                    ),
                    position = position_dodge(dodge)
                )
            }
        } else {
            the_plot = the_plot + geom_errorbar(
                aes(
                    ymin = .data$x.main - .data$x.eb,
                    ymax = .data$x.main + .data$x.eb,
                    width = 0.2
                ),
                position = position_dodge(dodge)
            )
        }
    }
    the_plot = the_plot + theme_bw() +
        labs(x = re_n(p_mid, factor_names), y = y_title) +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#d5d5d5"),
            panel.grid.minor.y = element_line(color = "#d5d5d5")
        )
    if (length(fact_names) == 3) {
        the_plot = the_plot + facet_wrap( ~ .data[[fact_names[3]]], nrow = row_number) +
            theme(
                strip.background = element_blank(),
                strip.text = element_text(face = 'bold', size = 12)
            )
    }
    if (numerics != FALSE) {
        graphics::plot(the_plot)
        invisible(tots)
    } else {
        return(the_plot)
    }
}

neat_plot2 = function(values,
                      binwidth = NULL,
                      parts = c('h', 'b', 'n'),
                      part_colors = NULL) {
    # c('hc', 'ha', 'dc', 'da', 'nc', 'na', 'bc', 'ba')
    validate_args(match.call(),
                  list(
                      val_arg(values, c('num'), 0),
                      val_arg(parts, c('char')),
                      val_arg(part_colors, c('null', 'num', 'char')),
                      val_arg(binwidth, c('null', 'num'))
                  ))
    clrs = c(
        hc = '#aaaadc',
        ha = 0.4,
        dc = '#004400',
        da = 0.1,
        nc = '#cc0000',
        na = 1,
        bc = '#bcdcc5',
        ba = 1,
        hlc = 'black'
    )
    wrongparts = parts[!(parts %in% c('h', 'd', 'b', 'n'))]
    if (length(wrongparts) > 0) {
        message(
            'The following "parts" inputs are not correct: "',
            paste(wrongparts, collapse = '", "'),
            '". See ?plot_neat.'
        )
    }
    wrongpartclrs = names(part_colors)[!(names(part_colors) %in% names(clrs))]
    if (length(wrongpartclrs) > 0) {
        message(
            'The following "part_colors" inputs are not correct: "',
            paste(wrongpartclrs, collapse = '", "'),
            '". See ?plot_neat.'
        )
    }
    if (!is.null(part_colors)) {
        for (nm in names(part_colors)) {
            clrs[nm] = part_colors[nm]
        }
    }
    plot_data = data.frame(values = values)
    if (is.null(binwidth)) {
        max_binwidth = (max(values) - min(values)) / 10
        my_binwidth = 2 * stats::IQR(values) / (length(values) ^ (1 / 3))
        if (max_binwidth < my_binwidth) {
            my_binwidth = max_binwidth
        }
    } else {
        my_binwidth = binwidth
    }
    the_plot = ggplot(plot_data, aes(x = values))
    if ('h' %in% parts) {
        the_plot = the_plot + geom_histogram(
            aes(y = .data$..count..),
            alpha = as.numeric(clrs['ha']),
            binwidth = my_binwidth,
            color = clrs['hlc'],
            fill = clrs['hc']
        )
        if ('d' %in% parts) {
            the_plot = the_plot +
                geom_density(
                    aes(y = .data$..count.. * my_binwidth),
                    color = clrs['dc'],
                    alpha = as.numeric(clrs['da']),
                    fill = clrs['dc']
                )
        }
        if ('n' %in% parts) {
            the_plot = the_plot +
                stat_function(
                    fun = function(x)
                        stats::dnorm(
                            x,
                            mean = mean(values),
                            sd = stats::sd(values)
                        ) * length(values) * my_binwidth,
                    color = clrs['nc'],
                    alpha = as.numeric(clrs['na']),
                    linetype = "dashed"
                )
        }
    } else {
        if ('d' %in% parts) {
            the_plot = the_plot +
                geom_density(
                    color = clrs['dc'],
                    alpha = as.numeric(clrs['da']),
                    fill = clrs['dc']
                )
        }

        if ('n' %in% parts) {
            the_plot = the_plot +
                stat_function(
                    fun = function(x)
                        stats::dnorm(
                            x,
                            mean = mean(values),
                            sd = stats::sd(values)
                        ),
                    color = clrs['nc'],
                    alpha = as.numeric(clrs['na']),
                    linetype = "dashed"
                )
        }
    }
    if ('b' %in% parts) {
        xrange = ggplot_build(the_plot)$layout$panel_params[[1]]$y.range
        hght = xrange[2] - xrange[1]
        box_y = -hght / 20
        box_w = -box_y / 2.2
        p_box <-
            ggplot(plot_data, aes(y = values)) + geom_boxplot()
        p_box_dat = layer_data(p_box)
        the_plot = the_plot +
            # manually plot flipped boxplot
            geom_segment(data = p_box_dat,
                         aes(
                             x = .data$ymin,
                             xend = .data$ymax,
                             y = box_y,
                             yend = box_y
                         )) +
            geom_rect(
                data = p_box_dat,
                aes(
                    x = NULL,
                    xmin = .data$lower,
                    xmax = .data$upper,
                    ymin = box_y - box_w,
                    ymax = box_y + box_w
                ),
                color = "black",
                fill = clrs['bc'],
                alpha = as.numeric(clrs['ba'])
            )  +
            # vertical lines at Q1 / Q2 / Q3
            geom_segment(data = p_box_dat,
                         aes(
                             x = .data$ymin ,
                             y = box_y + box_w,
                             yend = box_y - box_w,
                             xend = .data$ymin
                         )) +
            geom_segment(
                data = p_box_dat,
                aes(
                    x = .data$middle ,
                    y = box_y + box_w,
                    yend = box_y - box_w,
                    xend = .data$middle
                ),
                size = 0.8
            ) +
            geom_segment(data = p_box_dat,
                         aes(
                             x = .data$ymax,
                             y = box_y + box_w,
                             yend = box_y - box_w,
                             xend = .data$ymax
                         ))
        if (length(p_box_dat$outliers[[1]]) > 0) {
            the_plot = the_plot + geom_point(
                data = data.frame(x1 = p_box_dat$outliers[[1]]),
                aes(x = .data$x1, y = box_y),
                shape = 4
            )

        }
    }
    return(the_plot + theme_bw())
}
