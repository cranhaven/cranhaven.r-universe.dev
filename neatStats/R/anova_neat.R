#'@title Comparison of Multiple Means: ANOVA
#'
#'@description \code{\link[ez:ezANOVA]{Analysis of variance}} (ANOVA) F-test
#'  results with appropriate \code{\link[stats:oneway.test]{Welch}}'s and
#'  epsilon corrections where applicable (unless specified otherwise), including
#'  partial eta squared effect sizes with confidence intervals (CIs),
#'  generalized eta squared, and
#'  \code{\link[bayestestR:bayesfactor_inclusion]{inclusion Bayes factor based
#'  on matched models}} (BFs).
#'@param data_per_subject Data frame. Should contain all values
#'  (measurements/observations) in a single row per each subject.
#'@param values Vector of strings; column name(s) in the \code{data_per_subject}
#'  data frame. Each column should contain a single dependent variable: thus, to
#'  test repeated (within-subject) measurements, each specified column should
#'  contain one measurement.
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
#'@param ci Numeric; confidence level for returned CIs. (Default: \code{.9};
#'  Lakens, 2014; Steiger, 2004.)
#'@param norm_tests Normality tests for the pooled ANOVA residuals
#'  (\code{"none"} by default, giving no tests). Any or all of the following
#'  character input is accepted (as a single string or a character vector;
#'  case-insensitive): \code{"W"} (Shapiro-Wilk), \code{"K2"} (D'Agostino),
#'  \code{"A2"} (Anderson-Darling), \code{"JB"} (Jarque-Bera); see
#'  \code{\link{norm_tests}}. The option \code{"all"} (or \code{TRUE}) selects
#'  all four previous tests at the same time.
#'@param norm_plots If \code{TRUE}, displays density, histogram, and Q-Q plots
#'  (and scatter plots for paired tests) for the pooled residuals.
#'@param var_tests Logical, \code{FALSE} by default. If \code{TRUE} (and there
#'  are any between-subject factors), runs variance equality tests via
#'  \code{\link{var_tests}} for all combinations of the between-subject factors
#'  within each level of within-subject factor combinations; see Details.
#'@param bf_added Logical. If \code{TRUE} (default), inclusion Bayes factor is
#'  calculated and displayed. (Note: with multiple factors and/or larger
#'  dataset, the calculation can take considerable time.)
#'@param bf_sample Number of samples used to estimate Bayes factor (\code{10000}
#'  by default).
#'@param test_title String, \code{"--- neat ANOVA ---"} by default. Simply displayed
#'  in printing preceding the statistics.
#'@param welch If \code{TRUE} (default), calculates Welch's ANOVA via
#'  \code{\link[stats:oneway.test]{stats::oneway.test}} in case of a single
#'  factor (one-way) between-subject design. If \code{FALSE}, calculates via
#'  \code{\link[ez:ezANOVA]{ez::ezANOVA}} in such cases too (i.e., same as in
#'  case of every other design).
#'@param e_correction \code{NULL} (default) or one of the following strings:
#'  \code{'gg'}, \code{'hf'}, or \code{'none'}. If set to \code{'gg'},
#'  Greenhouse-Geisser correction is applied in case of repeated measures
#'  (regardless of violation of sphericity). If set to \code{'hf'}, Huynh-Feldt
#'  correction is applied. If set to \code{'none'}, no correction is applied. If
#'  \code{NULL}, Greenhouse-Geisser correction is applied when Mauchly's
#'  sphericity test is significant and the Greenhouse-Geisser epsilon is not
#'  larger than \code{.75}, while Huynh-Feldt correction is applied when
#'  Mauchly's sphericity test is significant and the Greenhouse-Geisser epsilon
#'  is larger than \code{.75} (see Girden, 1992).
#'@param type Sum of squares type specified as a number: \code{1}, \code{2}, or
#'  \code{3}. Set to \code{2} by default (which is generally recommended, see
#'  e.g. Navarro, 2019, Chapter 16).
#'@param white.adjust If not \code{FALSE} (default) uses a
#'  heteroscedasticity-corrected coefficient covariance matrix; the various
#'  values of the argument specify different corrections (\code{"hc0"},
#'  \code{"hc1"}, \code{"hc2"}, \code{"hc3"}, or \code{"hc4"}). See the
#'  documentation for \code{\link[car:hccm]{car::hccm}} for details. If set to
#'  \code{TRUE} then the \code{"hc3"} correction is selected.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@param plot_means Logical (\code{FALSE} by default). If \code{TRUE}, creates
#'  plots of means by factor, by passing data and factor information to
#'  \code{\link{plot_neat}}.
#'@param ... Any additional arguments will be passed on to
#'  \code{\link{plot_neat}}.
#'
#'@details
#'
#'The Bayes factor (BF) is always calculated with the default \code{rscaleFixed}
#'of \code{0.5} (\code{"medium"}) and \code{rscaleRandom} of \code{1}
#'(\code{"nuisance"}). BF supporting null hypothesis is denoted as BF01, while
#'that supporting alternative hypothesis is denoted as BF10. When the BF is
#'smaller than 1 (i.e., supports null hypothesis), the reciprocal is calculated
#'(hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to
#'10000, scientific (exponential) form is reported for readability. (The
#'original full BF number is available in the returned named vector as
#'\code{bf}.)
#'
#'Mauchly's sphericity test is returned for repeated measures with more than two
#'levels. If Mauchly's test is significant, epsilon correction may be applied
#'(see the \code{e_correction} parameter).
#'
#'Variance equality tests (if \code{var_tests} is \code{TRUE}): Brown-Forsythe
#'and Fligner-Killeen tests are performed for each within-subject factor level
#'(or, in case of several factors, each combination) via
#'\code{\link{var_tests}}. Note that variance testing is generally not
#'recommended (see e.g., Zimmerman, 2004). In case of a one-way between-subject
#'ANOVA, Welch-corrected results are reported by default, which corrects for
#'unequal variances. In case of multiple between-subject factors, the
#'\code{white.adjust} parameter can be set to \code{TRUE} (or \code{"hc3"}) to
#'apply \code{"hc3"} correction for unequal variances. In case of a mixed ANOVA
#'(both between-subject and within-subject factors), if the tests are
#'significant and the data is unbalanced (unequal group sample sizes), you
#'should either consider the results respectively or choose a different test.
#'
#'In case of multiple values (column names) that match identical levels for all
#'factors, the given columns will be merged into a single column taking the mean
#'value of all these columns. (This is to simplify "dropping" a within-subject
#'factor and retesting the remaining factors.) Explicit warning messages are
#'shown in each such case.
#'
#'@return Prints ANOVA statistics (including, for each model, F-test with
#'  partial eta squared and its CI, generalized eta squared, and BF, as
#'  specified via the corresponding parameters) in APA style. Furthermore, when
#'  assigned, returns all calculated information details for each effect (as
#'  \code{stat_list}), normality and variance tests (if any), etc.
#'
#'@note The F-tests are calculated via \code{\link[ez:ezANOVA]{ez::ezANOVA}},
#'  including Mauchly's sphericity test. (But Welch's ANOVA is
#'  calculated in case of one-way between-subject designs via
#'  \code{\link[stats:oneway.test]{stats::oneway.test}}, unless the \code{welch}
#'  parameter is set to \code{FALSE}.)
#'
#'  Confidence intervals are calculated, using the F value, via
#'  \code{\link[MBESS:conf.limits.ncf]{MBESS::conf.limits.ncf}}, converting
#'  noncentrality parameters to partial eta squared as \code{ncp/(ncp+
#'  df_nom+df_denom+1)} (Smithson, 2003).
#'
#'  Generalized eta squared is to facilitate potential subsequent
#'  meta-analytical comparisons (see Lakens, 2013).
#'
#'  The inclusion Bayes factor based on matched models is calculated via
#'  \code{\link[bayestestR:bayesfactor_inclusion]{bayestestR::bayesfactor_inclusion}},
#'   (with \code{match_models = TRUE}, and using an
#'  \code{\link[BayesFactor:anovaBF]{BayesFactor::anovaBF}} object for
#'  \code{models} input).
#'
#'
#'@references
#'
#'Girden, E. (1992). ANOVA: Repeated measures. Newbury Park, CA: Sage.
#'
#'Kelley, K. (2007). Methods for the behavioral, educational, and social
#'sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'\doi{https://doi.org/10.3758/BF03192993}
#'
#'Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#'cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in
#'Psychology, 4. https://doi.org/10.3389/fpsyg.2013.00863
#'
#'Lakens, D. (2014). Calculating confidence intervals for Cohen's d and
#'eta-squared using SPSS, R, and Stata [Blog post]. Retrieved from
#'\url{https://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html}
#'
#'Mathot. S. (2017). Bayes like a Baws: Interpreting Bayesian Repeated Measures
#'in JASP [Blog post]. Retrieved from
#'\url{https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp}
#'
#'McDonald, J. H. 2015. Handbook of Biological Statistics (3rd ed.). Sparky
#'House Publishing, Baltimore, Maryland. Retrieved from
#'\url{https://www.biostathandbook.com}
#'
#'Moder, K. (2010). Alternatives to F-test in one way ANOVA in case of
#'heterogeneity of variances (a simulation study). Psychological Test and
#'Assessment Modeling, 52(4), 343-353.
#'
#'Navarro, D. (2019). Learning Statistics with R: A Tutorial for Psychology
#'Students and Other Beginners (Version 0.6.1). Retrieved from
#'\url{https://learningstatisticswithr.com/}
#'
#'Smithson, M. (2003). Confidence intervals. Thousand Oaks, Calif: Sage
#'Publications.
#'
#'Steiger, J. H. (2004). Beyond the F test: effect size confidence intervals and
#'tests of close fit in the analysis of variance and contrast analysis.
#'Psychological Methods, 9(2), 164-182.
#'\doi{https://doi.org/10.1037/1082-989X.9.2.164}
#'
#'
#'Zimmerman, D. W. (2004). A note on preliminary tests of equality of variances.
#'British Journal of Mathematical and Statistical Psychology, 57(1), 173–181.
#'https://doi.org/10.1348/000711004849222
#'
#' @seealso \code{\link{plot_neat}}, \code{\link{t_neat}}
#' @examples
#' # assign random data in a data frame for illustration
#' # (note that the 'subject' is only for illustration; since each row contains the
#' # data of a single subject, no additional subject id is needed)
#' dat_1 = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     grouping1 = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'     grouping2 = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
#'     value_1_a = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
#'     value_2_a = c(-14.1, 58.5, -25.5, 42.2, -13, 4.4, 55.5, -28.5, 25.6, -37.1),
#'     value_1_b = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     value_2_b = c(8.024, -14.162, 3.1, -2.1, -1.5, 0.91, 11.53, 18.37, 0.3, -0.59),
#'     value_1_c = c(27.4,-17.6,-32.7, 0.4, 37.2, 1.7, 18.2, 8.9, 1.9, 0.4),
#'     value_2_c = c(7.7, -0.8, 2.2, 14.1, 22.1, -47.7, -4.8, 8.6, 6.2, 18.2)
#' )
#' head(dat_1) # see what we have
#'
#' # For example, numbers '1' and '2' in the variable names of the values can
#' # denote sessions in an experiment, such as '_1' for first session, and '_2 for
#' # second session'. The letters '_a', '_b', '_c' could denote three different
#' # types of techniques used within each session, to be compared to each other.
#' # See further below for a more verbose but more meaningful example data.
#'\donttest{
#' # get the between-subject effect of 'grouping1'
#' anova_neat(dat_1, values = 'value_1_a', between_vars = 'grouping1')
#'
#' # main effects of 'grouping1', 'grouping2', and their interactions
#' anova_neat(dat_1,
#'            values = 'value_1_a',
#'            between_vars = c('grouping1', 'grouping2'))
#'
#' # repeated measures:
#' # get the within-subject effect for 'value_1_a' vs. 'value_1_b'
#' anova_neat(dat_1, values = c('value_1_a', 'value_1_b'))
#'
#' # same, but give the factor a custom variable name, and omit BF for speed
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b'),
#'     within_ids = 'a_vs_b',
#'     bf_added = FALSE
#' )
#' # or
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b'),
#'     within_ids = 'letters',
#'     bf_added = FALSE
#' )
#'
#' # within-subject effect for 'value_1_a' vs. 'value_1_b' vs. 'value_1_c'
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     bf_added = FALSE
#' )
#'}
#' # within-subject main effect for 'value_1_a' vs. 'value_1_b' vs. 'value_1_c',
#' # between-subject main effect 'grouping1', and the interaction of these two main
#' # effects
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = 'grouping1',
#'     bf_added = FALSE
#' )
#'
#' # within-subject 'number' main effect for variables with number '1' vs. number
#' # '2' ('value_1_a' and 'value_1_b' vs. 'value_2_a' and 'value_2_b'), 'letter'
#' # main effect for variables with final letterr 'a' vs. final letter 'b'
#' # ('value_1_a' and 'value_2_a' vs. 'value_1_b' and 'value_2_b'), and the
#' # 'letter' x 'number' interaction
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     bf_added = FALSE
#' )
#'\donttest{
#' # same as above, but now including between-subject main effect 'grouping2' and
#' # its interactions
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     between_vars = 'grouping2',
#'     bf_added = FALSE
#' )
#'
#' # same as above, but now creating a plot of means
#' # y_title passed add an example title (label) for the Y axis
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     between_vars = 'grouping2',
#'     bf_added = FALSE,
#'     plot_means = TRUE,
#'     y_title = 'Example Y Title'
#' )
#'
#' # same as above, but collapsing means over the removed "numbers" factor
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b')
#'     ),
#'     between_vars = 'grouping2',
#'     bf_added = FALSE,
#'     plot_means = TRUE,
#'     y_title = 'Example Y Title'
#' )
#'
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
#'     rating_fright_low_disgust_low = c(36.2,45.2,41,24.6,30.5,28.2,40.9,45.1,31,16.9),
#'     rating_fright_high_disgust_low = c(-14.1,58.5,-25.5,42.2,-13,4.4,55.5,-28.5,25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83,71,111,70,92,75,110,111,110,85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162,3.1,-2.1,-1.5,0.91,11.53,18.37,0.3,-0.59)
#' )
#' head(pic_ratings) # see what we have
#'
#' # the same logic applies as for the examples above, but now the
#' # within-subject differences can be more meaningfully specified, e.g.
#' # 'disgust_low' vs. 'disgust_high' for levels of disgustingness, while
#' # 'fright_low' vs. 'fright_high' for levels of frighteningness
#' anova_neat(
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
#'     bf_added = FALSE
#' )
#'
#' # the results are the same as for the analogous test for the 'dat_1' data, only
#' # with different names
#'
#' # now let's say the ratings were done in two separate groups
#' pic_ratings = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     group_id = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
#'     rating_fright_low_disgust_low = c(36.2,45.2,41,24.6,30.5,28.2,40.9,45.1,31,16.9),
#'     rating_fright_high_disgust_low = c(-14.1,58.5,-25.5,42.2,-13,4.4,55.5,-28.5,25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83,71,111,70,92,75,110,111,110,85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162,3.1,-2.1,-1.5,0.91,11.53,18.37,0.3,-0.59)
#' )
#'
#' # now test the effect and interactions of 'group_id'
#' anova_neat(
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
#'     between_vars = 'group_id',
#'     bf_added = FALSE
#' )
#'
#' # again, same results as with 'dat_1' (using 'grouping2' as group_id)
#'}
#' @export
anova_neat = function(data_per_subject,
                      values,
                      within_ids = NULL,
                      between_vars = NULL,
                      ci = 0.90,
                      norm_tests = 'none',
                      norm_plots = FALSE,
                      var_tests = FALSE,
                      bf_added = FALSE,
                      bf_sample = 10000,
                      test_title = "--- neat ANOVA ---",
                      welch = TRUE,
                      e_correction = NULL,
                      type = 2,
                      white.adjust = FALSE,
                      hush = FALSE,
                      plot_means  = FALSE,
                      ...) {
    validate_args(
        match.call(),
        list(
            val_arg(data_per_subject, c('df')),
            val_arg(values, c('char')),
            val_arg(within_ids, c('null', 'char', 'list'), 1),
            val_arg(between_vars, c('null', 'char')),
            val_arg(ci, c('num'), 1),
            val_arg(norm_tests, c('char')),
            val_arg(norm_plots, c('bool'), 1),
            val_arg(var_tests, c('bool')),
            val_arg(bf_added, c('bool'), 1),
            val_arg(bf_sample, c('num'), 1),
            val_arg(test_title, c('char'), 1),
            val_arg(welch, c('bool'), 1),
            val_arg(e_correction, c('null', 'char'), 1, c('gg', 'hf', 'none')),
            val_arg(type, c('num'), 1),
            val_arg(white.adjust, c('bool', 'char'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    cols_notfound = c()
    bv_copy = between_vars
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
                '" were collapsed (averaged) into one column.'
            )
            data_per_subject[[dup]] = rowMeans(data_per_subject[, to_collapse], na.rm =
                                                   TRUE)
            values = values[!(values %in% to_collapse)]
            values = c(values, dup)
        }
    }
    # end collapsing
    if (plot_means == TRUE) {
        means_plot =
            plot_neat(
                data_per_subject,
                values,
                within_ids,
                bv_copy,
                numerics = FALSE,
                hush = TRUE,
                ...
            )
        if (!is.null(means_plot[1])) {
            graphics::plot(means_plot)
        }
    }
    dat_tot = plot_neat(
        data_per_subject,
        values,
        within_ids,
        bv_copy,
        eb_method = stats::sd,
        numerics = TRUE
    )
    if (is.null(e_correction)) {
        e_correction = ''
    }
    data_wide = data_per_subject
    name_taken('within_factor', data_wide)
    name_taken('..neat_values', data_wide)
    name_taken('..neat_id', data_wide)
    id_col = '..neat_id'
    data_wide[[id_col]] = as.character(seq.int(nrow(data_wide)))
    w_anova = NULL
    if (length(values) > 1) {
        withname = "within_factor"
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
                    data_reshaped[[fact_name]][grepl(fact_x, data_reshaped$within_factor)] = fact_x
                }
                data_reshaped[[fact_name]] = as.factor(data_reshaped[[fact_name]])
            }
            within_vars = paste(names(within_ids), collapse = ', ')
        } else if (is.list(within_ids)) {
            within_vars = names(within_ids)
            names(data_reshaped)[names(data_reshaped) == 'within_factor'] = names(within_ids)
            withname = names(within_ids)
        }  else if (is.character(within_ids)) {
            within_vars = within_ids
            names(data_reshaped)[names(data_reshaped) == 'within_factor'] = within_ids
            withname = within_ids
        } else {
            within_vars = 'within_factor'
        }
        value_col = "..neat_values"
        this_data = data_reshaped
    } else {
        value_col = values
        this_data = data_wide
        within_vars = NULL
        if (length(to_c(between_vars)) == 1 && welch != FALSE) {
            w_anova = eval(parse(
                text = paste0(
                    'stats::oneway.test(',
                    value_col,
                    ' ~ ',
                    between_vars,
                    ', data = this_data, var.equal = FALSE  )'
                )
            ))
        }
    }

    this_data[, id_col] = to_fact(this_data[[id_col]])


    if (is.null(between_vars)) {
        between_vars_ez = 'NULL'
        between_vars_bf = ''
    } else {
        between_vars_ez = paste0('c(', between_vars , ')')
        between_vars_bf = between_vars
        for (this_col in to_c(between_vars)) {
            this_data[, this_col] = to_fact(this_data[[this_col]])
        }
    }
    if (is.null(within_vars)) {
        within_vars_ez = 'NULL'
        within_vars_bf = ''
        id_part = ''
    } else {
        within_vars_ez = paste0('c(', within_vars , ')')
        if (is.null(between_vars)) {
            within_vars_bf = within_vars
        } else {
            within_vars_bf = paste0(' * ', within_vars)
        }
        id_part = paste0(' + ', id_col)
        for (this_col in to_c(within_vars)) {
            this_data[, this_col] = to_fact(this_data[[this_col]])
        }
    }
    ezANOVA = ez::ezANOVA
    a_text = paste0(
        'ezANOVA(data= this_data, dv=',
        value_col,
        ', wid=',
        id_col,
        ', between =',
        between_vars_ez,
        ', within =',
        within_vars_ez,
        ', type = ',
        type,
        ', white.adjust = ',
        white.adjust,
        ', detailed = TRUE, return_aov = TRUE)'
    )
    ez_anova_out = eval(parse(text = a_text))
    # suppressWarnings
    # suppressMessages

    if (bf_added == TRUE) {
        indep_vars = gsub(',', '*', paste0(between_vars_bf, within_vars_bf))
        bf = eval(parse(
            text =
                paste0(
                    'BayesFactor::anovaBF(',
                    value_col,
                    ' ~ ',
                    indep_vars,
                    id_part,
                    ', data = this_data, whichRandom = "',
                    id_col,
                    '", iterations = ',
                    bf_sample,
                    ', whichModels = "withmain")'
                )
        ))
        if (is.null(within_vars) &&
            length(to_c(between_vars)) == 1) {
            bf_inc = as.vector(bf)
        } else {
            bf_inc = bayestestR::bayesfactor_inclusion(bf, match_models = TRUE)
            bf_inc$BF = exp(bf_inc$log_BF)
            bf_inc = stats::setNames(object = bf_inc$BF, nm = rownames(bf_inc))
        }
        bf_models = bf
        names(bf_inc) = bf_names(names(bf_inc))
    } else {
        bf_inc = NULL
        bf_models = NULL
    }
    resids = ez_anova_out$aov$residuals
    if (is.null(resids)) {
        aov_proj = stats::proj(ez_anova_out$aov)
        resids = aov_proj[[length(aov_proj)]][, "Residuals"]
        fitt = stats::fitted(ez_anova_out$aov[[length(ez_anova_out$aov)]])
    } else {
        fitt = ez_anova_out$aov$fitted
    }
    if (norm_plots == TRUE &
        (norm_tests == 'none' | norm_tests == FALSE)) {
        norm_tests = 'all'
    }
    if (norm_tests != 'none' & norm_tests != FALSE &
        hush == FALSE) {
        if (norm_tests == TRUE) {
            norm_tests = 'all'
        }
        prnt('--- Normality of the Residuals ---')
        norm_tests_in(
            var1 = resids,
            var2 = NULL,
            pair = FALSE,
            norm_tests = norm_tests,
            alpha = 0.05,
            hush = FALSE,
            plots = norm_plots,
            tneet = FALSE,
            nonparametric = FALSE,
            aspect_ratio = 1,
            anov = TRUE
        )
    }
    norm_info = all_norm_tests(resids)
    var_info = NULL
    if (!is.null(bv_copy)) {
        if (var_tests == TRUE && hush == FALSE) {
            var_hush = FALSE
            prnt('--- Equality of Variances ---')
        } else {
            var_hush = TRUE
        }
        if (is.null(within_vars)) {
            var_info = neatStats::var_tests(value_col, bv_copy, this_data, hush = var_hush)
        } else {
            for (fact in unique(this_data[[withname]])) {
                if (isFALSE(var_hush)) {
                    cat(fact,
                        ': ',
                        sep = '',
                        fill = TRUE)
                }
                var_info[[fact]] = neatStats::var_tests(value_col, bv_copy,
                                                        this_data[this_data[[withname]] == fact,],
                                                        hush = var_hush)
            }
        }
    }
    to_return = anova_apa(
        ezANOVA_out = ez_anova_out,
        ci = ci,
        bf_added = bf_inc,
        bf_models = bf_models,
        test_title = test_title,
        welch = w_anova,
        e_correction = e_correction,
        hush = hush,
        dat_tot = dat_tot,
        resids = resids,
        fitt = fitt
    )
    to_return$normality_tests = norm_info
    to_return$variance_tests = var_info
    class(to_return) = "anova_neat"
    invisible(to_return)
}

anova_apa = function(ezANOVA_out,
                     ci = 0.90,
                     bf_added = NULL,
                     bf_models = NULL,
                     test_title = "--- neat ANOVA ---",
                     welch = NULL,
                     e_correction = '',
                     hush = FALSE,
                     dat_tot = NULL,
                     fitt = NULL,
                     resids = NULL) {
    ezANOVA_out$ANOVA$pes = ezANOVA_out$ANOVA$SSn / (ezANOVA_out$ANOVA$SSn + ezANOVA_out$ANOVA$SSd)
    ezANOVA_out$ANOVA$Effect = as.character(ezANOVA_out$ANOVA$Effect)
    if (hush == FALSE) {
        prnt(test_title)
    }
    mauchly = ezANOVA_out$"Mauchly's Test for Sphericity"
    if (!is.null(mauchly)) {
        if (hush == FALSE) {
            prnt("--- Mauchly's Sphericity Test ---")
        }
        mauchly_info = get_e_corrs(mauchly,
                                   ezANOVA_out$"Sphericity Corrections",
                                   e_correction,
                                   hush)
        eps_p_corrs = mauchly_info$eps_p_corrs
        mauchly_info$eps_p_corrs = NULL
        mauchly_info = tryCatch({
            as.data.frame(do.call(cbind, mauchly_info))
        }, error = function(e) {
            warning(
                "Mauchly effects' lists not identical, returning as lists instead of a data frame."
            )
            mauchly_info
        })
        if (hush == FALSE) {
            prnt("--- ANOVA ---")
        }
    } else {
        mauchly_info = NULL
        eps_p_corrs = NULL
    }
    stat_list = list()
    for (indx in 1:length(ezANOVA_out$ANOVA$Effect)) {
        f_name = ezANOVA_out$ANOVA$Effect[indx]
        f_name = sort(strsplit(f_name, ":")[[1]])
        f_name = paste(f_name, collapse = " CHAR_X ")
        if (is.null(bf_added) |
            !(f_name %in% names(bf_added))) {
            bf_out = "."
            bf_val = NULL
        } else {
            bf_val = bf_added[f_name]
            bf_out = bf_neat(bf_val)
        }
        F_val = ezANOVA_out$ANOVA$F[indx]
        df_n = ezANOVA_out$ANOVA$DFn[indx]
        df_d = ezANOVA_out$ANOVA$DFd[indx]
        df_d_val = df_d
        pvalue = ezANOVA_out$ANOVA$p[indx]
        limits = MBESS::conf.limits.ncf(
            F.value = F_val,
            conf.level = ci,
            df.1 = df_n,
            df.2 = df_d
        )
        lower = limits$Lower.Limit / (limits$Lower.Limit + df_n + df_d + 1)
        upper = limits$Upper.Limit / (limits$Upper.Limit + df_n + df_d + 1)
        if (!is.null(welch)) {
            F_val = as.numeric(welch$statistic)
            df_n = as.numeric(welch$parameter['num df'])
            df_d_val = as.numeric(welch$parameter['denom df'])
            df_d = ro(as.numeric(welch$parameter['denom df']), 1)
            pvalue = as.numeric(welch$p.value)
        }
        if (!is.null(eps_p_corrs[[f_name]])) {
            pvalue = as.numeric(eps_p_corrs[[f_name]]['pval'])
            eps_num = as.numeric(eps_p_corrs[[f_name]]['eps'])
            eps_val = eps_num
            eps_added = paste0(', CHAR_EPS = ', ro(eps_num, 3))
        } else {
            if (is.null(mauchly_info)) {
                eps_val = NULL
            } else {
                eps_val = NA
            }
            eps_num = NULL
            eps_added = NULL
        }
        petas = ezANOVA_out$ANOVA$pes[indx]
        if (is.na(lower)) {
            lower_val = 0
            lower = "0"
        } else {
            lower_val = lower
            lower = sub('.', '', ro(lower, 3))
        }
        if (is.na(upper)) {
            upper_val = "< .001"
            upper = "< .001"
        } else {
            upper_val = upper
            upper = sub('.', '', ro(upper, 3))
        }
        np2 = sub('.', '', ro(petas, 3))
        the_ci = paste0(", ", ro(ci * 100, 0), "% CI [")
        getas = ezANOVA_out$ANOVA$ges[indx]
        nG2 = sub('.', '', ro(getas, 3))
        out = paste0(
            "F(",
            df_n,
            ", ",
            df_d,
            ")",
            " = ",
            ro(F_val, 2),
            ", p = ",
            ro(pvalue, 3),
            eps_added,
            ", CHAR_ETAp2 = ",
            np2,
            the_ci,
            lower,
            ", ",
            upper,
            "], CHAR_ETAG2 = ",
            nG2,
            bf_out,
            " (",
            f_name,
            ")"
        )
        if (hush == FALSE) {
            prnt(out)
        }
        s_name = gsub(" CHAR_X ", "_x_", f_name)
        stat_list[[s_name]] = c(
            Fval = as.numeric(F_val),
            df1 = df_n,
            df2 = df_d_val,
            pval = pvalue,
            epsilon = eps_val,
            petas = as.numeric(petas),
            ci_lower = lower_val,
            ci_upper = upper_val,
            getas = as.numeric(getas),
            bf = as.numeric(bf_val)
        )
    }
    stat_list = list(
        effects =
            tryCatch({
                as.data.frame(do.call(rbind, stat_list))
            }, error = function(e) {
                warning("Effects' vectors not identical, returning as list instead of a data frame.")
                stat_list
            }),
        mauchly = mauchly_info,
        ci_level = ci,
        ez_anova = ezANOVA_out,
        bf_models = bf_models,
        totals = dat_tot,
        residuals = resids,
        fitted = fitt
    )
    invisible(stat_list)
}

all_norm_tests = function(var_to_norm) {
    norm_tests = list()
    norm_tests[['W']] = fBasics::shapiroTest(var_to_norm)@test
    if (length(var_to_norm) >= 8) {
        if (length(var_to_norm) >= 21) {
            norm_tests[['K2']] = fBasics::dagoTest(var_to_norm)@test
        }
        norm_tests[['A2']] = fBasics::adTest(var_to_norm)@test
    }
    norm_tests[['JB']] = fBasics::jarqueberaTest(var_to_norm)@test
    norms_out = list()
    for (norm_stat in names(norm_tests)) {
        norms_out[[norm_stat]] = list(
            title = as.character(
                c(
                    'W' = 'Shapiro-Wilk test',
                    'K2' = "D'Agostino test",
                    'A2' = "Anderson-Darling test",
                    'JB' = "Jarque-Bera test"
                )[norm_stat]
            ),
            statistic = norm_tests[[norm_stat]]$statistic[[1]],
            pval = norm_tests[[norm_stat]]$p.value[[1]]
        )
    }
    return(as.data.frame(do.call(cbind, norms_out)))
}
