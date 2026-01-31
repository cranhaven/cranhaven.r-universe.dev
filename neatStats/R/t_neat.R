#'@title Difference of Two Means and Area Under the Curve
#'
#'@description Welch's \code{\link[stats:t.test]{t-test}} results including
#'  Cohen's d with confidence interval (CI),
#'  \code{\link[BayesFactor:ttestBF]{Bayes factor}} (BF), and
#'  \code{\link[pROC:auc]{area under the receiver operating characteristic
#'  curve}} (AUC). For non-parametric version,
#'  \code{\link[stats:wilcox.test]{Wilcoxon test}} results (Mann–Whitney U test,
#'  aka "Wilcoxon rank-sum test", for independent samples; Wilcoxon signed-rank
#'  test for paired samples; including nonparametric "location difference
#'  estimate" (see \code{\link[stats:wilcox.test]{stats::wilcox.test}}); along
#'  with corresponding rank-based BFs as per van Doorn et al., 2020).
#'@param var1 Numeric vector; numbers of the first variable.
#'@param var2 Numeric vector; numbers of the second variable.
#'@param pair Logical. If \code{TRUE}, all tests (t, BF, AUC) are conducted for
#'  paired samples. If \code{FALSE} (default) for independent samples.
#'@param nonparametric Logical (\code{FALSE} by default). If \code{TRUE}, uses
#'  nonparametric (rank-based, "Wilcoxon") t-tests (including BFs; see Notes).
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided tests (t and BF): either "1" (\code{var1} mean expected to be
#'  greater than \code{var2} mean) or "2" (\code{var2} mean expected to be
#'  greater than \code{var1} mean). If \code{NULL} (default), the test is
#'  two-sided.
#'@param norm_tests Normality tests. Any or all of the following character input
#'  is accepted (as a single string or a character vector; case-insensitive):
#'  \code{"W"} (Shapiro-Wilk), \code{"K2"} (D'Agostino), \code{"A2"}
#'  (Anderson-Darling), \code{"JB"} (Jarque-Bera); see Notes. Two other options
#'  are \code{"all"} (same as \code{TRUE}; to choose all four previous tests at
#'  the same time) or \code{"latent"} (default value; prints all tests only if
#'  \code{nonparametric} is set to \code{FALSE} and any of the four tests gives
#'  a p value below .05). Each normality test is performed for the difference
#'  values between the two variables in case of paired samples, or for each of
#'  the two variables for unpaired samples. Set to \code{"none"} to disable
#'  (i.e., not to perform any normality tests).
#'@param norm_plots If \code{TRUE}, displays density, histogram, and Q-Q plots
#'  (and scatter plots for paired tests) for each of the two variable (and
#'  differences for pairwise observations, in case of paired samples).
#'@param ci Numeric; confidence level for returned CIs for Cohen's d and AUC.
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed.
#'@param bf_rscale The scale of the prior distribution (\code{0.707} by
#'  default).
#'@param bf_sample Number of samples used to estimate Bayes factor (\code{1000}
#'  by default). More samples (e.g. \code{10000}) take longer time but give more
#'  stable BF.
#'@param auc_added Logical (\code{FALSE} by default). If \code{TRUE}, AUC is
#'  calculated and displayed. Includes TPR and TNR, i.e., true positive and true
#'  negative rates, i.e. sensitivity and specificity, using an optimal
#'  value, i.e. threshold, that provides maximal TPR and TNR. These values may
#'  be cross-validated: see \code{cv_rep}. (Note that what is designated as
#'  "positive" or "negative" depends on the scenario: this function always
#'  assumes \code{var1} as positive and \code{var2} as negative. If your
#'  scenario or preference differs, you can simply switch the names or values
#'  when reporting the results.)
#'@param cutoff Numeric. Custom cutoff value for AUC TPR and TNR, also to be
#'  depicted in the plot. In case of multiple given, the first is used for
#'  calculations, but all will be depicted in the plot.
#'@param r_added Logical. If \code{TRUE} (default), Pearson correlation is
#'  calculated and displayed in case of paired comparison.
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@param test_title \code{NULL} or string. If not \code{NULL}, simply displayed
#'  in printing preceding the statistics. (Useful e.g. to distinguish several
#'  different comparisons inside a \code{function} or a \code{for} loop.)
#'@param round_descr Number \code{\link[=ro]{to round}} to the descriptive
#'  statistics (means and SDs).
#'@param round_auc Number \code{\link[=ro]{to round}} to the AUC and its CI.
#'@param auc_greater String (or number); specifies which variable is expected to
#'  have greater values for 'cases' as opposed to 'controls': "1" (default;
#'  \code{var1} expected to be greater for 'cases' than \code{var2} mean) or "2"
#'  (\code{var2} expected to be greater for 'cases' than \code{var1}). Not to be
#'  confused with one-sided tests; see Details.
#'@param cv_rep \code{FALSE} (default), \code{TRUE}, or numeric. If \code{TRUE}
#'  or numeric, a cross-validation is performed for the calculation of TPRs and
#'  TNRs. Numeric value specifies the number of repetitions, while, if
#'  \code{TRUE}, it defaults to \code{100} repetitions. In each repetition, the
#'  data is divided into \code{k} random parts ("folds"; see \code{cv_fold}),
#'  and the optimal accuracy is obtained k times from a k-1 training set
#'  (\code{var1} and \code{var2} truncated to equal length, if needed, in each
#'  case within each repetition), and the TPR and TNR are calculated from the
#'  remaining test set (different each time).
#'@param cv_fold Numeric. The number of folds into which the data is divided for
#'  cross-validation (default: 10).
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@param plots Logical (or \code{NULL}). If \code{TRUE}, creates a combined
#'  density plot (i.e., \code{\link[stats:density]{Gaussian kernel density
#'  estimates}}) from the two variables. Includes dashed vertical lines to
#'  indicate means of each of the two variables. If \code{nonparametric} is set
#'  to \code{TRUE}, medians are calculated for these dashed lines instead of
#'  means. When \code{auc_added} is \code{TRUE} (and the AUC is at least .5),
#'  the best threshold value for classification (maximal differentiation
#'  accuracy using Youden's index) is added to the plot as solid vertical line.
#'  (In case of multiple best thresholds with identical overall accuracy, all
#'  are added.) If \code{NULL}, same as if \code{TRUE} except that histogram is
#'  added to the background.
#'@param rug_size Numeric (\code{4} by default): size of the rug ticks below the
#'  density plot. Set to \code{0} (zero) to omit rug plotting.
#'@param aspect_ratio Aspect ratio of the plots: \code{1} (\code{1}/\code{1}) by
#'  default. (Set to \code{NULL} for dynamic aspect ratio.)
#'@param y_label String or \code{NULL}; the label for the \code{y} axis.
#'  (Default: \code{"density estimate"}.)
#'@param x_label String or \code{NULL}; the label for the \code{x} axis.
#'  (Default: \code{"values"}.)
#'@param factor_name String or \code{NULL}; factor legend title. (Default:
#'  \code{NULL}.)
#'@param var_names A vector of two strings; the variable names to be displayed
#'  in the legend. (Default: \code{c("1", "2")}.)
#'@param reverse Logical. If \code{TRUE}, reverses the order of variable names
#'  displayed in the legend.
#'
#'@details
#' The Bayes factor (BF) supporting null hypothesis is denoted as BF01, while
#' that supporting alternative hypothesis is denoted as BF10. When the BF is
#' smaller than 1 (i.e., supports null hypothesis), the reciprocal is calculated
#' (hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to
#' 10000, scientific (exponential) form is reported for readability. (The
#' original full BF number is available in the returned named vector as
#' \code{bf}.)
#'
# For details about the nonparametric (rank-based, Wilcoxon) Bayes factors, see
# van Doorn et al. (2020). The source code for the calculation is a contribution
# by J. van Doorn; the original version is available via https://osf.io/gny35/.
#'
#'For simplicity, Cohen's d is reported for nonparametric tests too: you may
#'however want to consider reporting alternative effect sizes in this case.
#'
#'The original \code{\link[pROC:auc]{pROC::auc}} function, by default, always
#'returns an AUC greater than (or equal to) .5, assuming that the prediction
#'based on values in the expected direction work correctly at least at chance
#'level. This however may be confusing. Consider an example where we measure the
#'heights of persons in a specific small sample and expect that greater height
#'predicts masculine gender. The results are, say, 169, 175, 167, 164 (cm) for
#'one gender, and 176, 182, 179, 165 for the other. If the expectation is
#'correct (the second, greater values are for males), the AUC is .812. However,
#'if in this particular population females are actually taller than males, the
#'AUC is in fact .188. To keep things clear, the \code{t_neat} function always
#'makes an assumption about which variable is expected to be greater for correct
#'classification ("1" by default; i.e., \code{var1}; to be specified as
#'\code{auc_greater = "2"} for \code{var2} to be expected as greater). For this
#'example, if the first (smaller) variables are given as \code{var1} for
#'females, and second (larger), variables are given as \code{var2} for males, we
#'have to specify \code{auc_greater = "2"} to indicate the expectation of larger
#'values for males. (Or, easier, just add the expected larger values as
#'\code{var1}.)
#'
#'@return Prints t-test statistics (including Cohen's d with CI, BF, and AUC, as
#'  specified via the corresponding parameters) in APA style. Furthermore, when
#'  assigned, returns a list, that contains a named vector '\code{stats}' with
#'  the following elements: \code{t} (t value), \code{p} (p value), \code{d}
#'  (Cohen's d), \code{bf} (Bayes factor), \code{auc} (AUC), \code{accuracy}
#'  (overall accuracy using the optimal classification threshold), and
#'  \code{youden} (Youden's index: \code{specificity + sensitivity - 1}). The
#'  latter three are \code{NULL} when \code{auc_added} is \code{FALSE}. When
#'  \code{auc_added} is \code{TRUE}, there are also two or three additional
#'  elements of the list. One is '\code{roc_obj}', which is a
#'  \code{\link[pROC]{roc}} object, to be used e.g. with the
#'  \code{\link{roc_neat}} function. Another is '\code{best_thresholds}', which
#'  contains the best threshold value(s) for classification, along with
#'  corresponding specificity and sensitivity. The third '\code{cv_results}'
#'  contains the results, if any, of the cross-validation of TPRs and TNRs
#'  (means per repetition). Finally, if \code{plots} is \code{TRUE} (or
#'  \code{NULL}), the plot is displayed as well as returned as a
#'  \code{\link[ggplot2]{ggplot}} object, named \code{t_plot}.
#'
#'@note
#'
#'The Welch's t-test is calculated via
#'\code{\link[stats:t.test]{stats::t.test}}.
#'
#'#'Normality tests are all calculated via
#'\code{\link[fBasics:NormalityTests]{fBasics::NormalityTests}}, selected based
#'on the recommendation of Lakens (2015), quoting Yap and Sim (2011, p. 2153):
#'"If the distribution is symmetric with low kurtosis values (i.e. symmetric
#'short-tailed distribution), then the D'Agostino and Shapiro-Wilkes tests have
#'good power. For symmetric distribution with high sample kurtosis (symmetric
#'long-tailed), the researcher can use the JB, Shapiro-Wilkes, or
#'Anderson-Darling test." See \url{https://github.com/Lakens/perfect-t-test} for
#'more details.
#'
#'Cohen's d and its confidence interval are calculated, using the t value, via
#'\code{\link[MBESS:ci.smd]{MBESS::ci.smd}} for independent samples (as
#'standardized mean difference) and via \code{\link[MBESS:ci.sm]{MBESS::ci.sm}}
#'for paired samples (as standardized mean).
#'
#'The parametric Bayes factor is calculated via
#'\code{\link[BayesFactor:ttestBF]{BayesFactor::ttestBF}}. The nonparametric
#'(rank-based) Bayes factor is a contribution by Johnny van Doorn; the original
#'source code is available via \url{https://osf.io/gny35/}.
#'
#'The correlation and its CI are calculated via
#'\code{\link[stats:cor.test]{stats::cor.test}}, and is always two-sided, always
#'with 95 percent CI. For more, use \code{\link{corr_neat}}.
#'
#'The AUC and its CI are calculated via \code{\link[pROC:auc]{pROC::auc}}, and
#'the accuracy at optimal threshold via \code{\link[pROC:coords]{pROC::coords}}
#'(\code{x = "best"}); both using the object \code{\link[pROC:roc]{pROC::roc}}.
#'
#'@references
#'
#'Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by
#'default use Welch's t-test instead of Student's t-test. International Review
#'of Social Psychology, 30(1). \doi{https://doi.org/10.5334/irsp.82}
#'
#'Kelley, K. (2007). Methods for the behavioral, educational, and social
#'sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'\doi{https://doi.org/10.3758/BF03192993}
#'
#'Lakens, D. (2015). The perfect t-test (version 1.0.0). Retrieved from
#'https://github.com/Lakens/perfect-t-test.
#'\doi{https://doi.org/10.5281/zenodo.17603}
#'
#'Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J. C., &
#'Muller, M. (2011). pROC: an open-source package for R and S+ to analyze and
#'compare ROC curves. BMC bioinformatics, 12(1), 77.
#'\doi{https://doi.org/10.1186/1471-2105-12-77}
#'
#'van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2020). Bayesian
#'rank-based hypothesis testing for the rank sum test, the signed rank test, and
#'Spearman’s rho. Journal of Applied Statistics, 1–23.
#'\doi{https://doi.org/10.1080/02664763.2019.1709053}
#'
#'Yap, B. W., & Sim, C. H. (2011). Comparisons of various types of normality
#'tests. Journal of Statistical Computation and Simulation, 81(12), 2141–2155.
#'\doi{https://doi.org/10.1080/00949655.2010.520163}
#'
#' @seealso \code{\link{corr_neat}}, \code{\link{roc_neat}}
#' @examples
#' # assign two variables (numeric vectors)
#' v1 = c(191, 115, 129, 43, 523,-4, 34, 28, 33,-1, 54)
#' v2 = c(4,-2, 23, 13, 32, 16, 3, 29, 37,-4, 65)
#'
#' t_neat(v1, v2) # prints results as independent samples
#' t_neat(v1, v2, pair = TRUE) # as paired samples (r added by default)
#' t_neat(v1, v2, pair = TRUE, greater = "1") # one-sided
#' t_neat(v1, v2, pair = TRUE, auc_added = TRUE ) # AUC included
#'
#' # print results and assign returned list
#' results = t_neat(v1, v2, pair = TRUE)
#'
#' results$stats['bf'] # get precise BF value
#'
#' @export
t_neat = function(var1,
                  var2,
                  pair = FALSE,
                  nonparametric = FALSE,
                  greater = NULL,
                  norm_tests = 'latent',
                  norm_plots = FALSE,
                  ci = NULL,
                  bf_added = FALSE,
                  bf_rscale = sqrt(0.5),
                  bf_sample = 1000,
                  auc_added = FALSE,
                  cutoff = NULL,
                  r_added = TRUE,
                  for_table = FALSE,
                  test_title = NULL,
                  round_descr = 2,
                  round_auc = 3,
                  auc_greater = '1',
                  cv_rep = FALSE,
                  cv_fold = 10,
                  hush = FALSE,
                  plots = FALSE,
                  rug_size = 4,
                  aspect_ratio = 1,
                  y_label = "density estimate",
                  x_label = "\nvalues",
                  factor_name = NULL,
                  var_names = c("1", "2"),
                  reverse = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('num'), 0),
            val_arg(pair, c('bool'), 1),
            val_arg(nonparametric, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(norm_tests, c('bool', 'char')),
            val_arg(norm_plots, c('bool'), 1),
            val_arg(ci, c('null', 'num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(bf_rscale, c('num'), 1),
            val_arg(bf_sample, c('num'), 1),
            val_arg(auc_added, c('bool'), 1),
            val_arg(cutoff, c('null', 'num')),
            val_arg(r_added, c('bool'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(test_title, c('char', 'null'), 1),
            val_arg(round_descr, c('num'), 1),
            val_arg(round_auc, c('num'), 1),
            val_arg(cv_rep, c('num', 'bool'), 1),
            val_arg(cv_fold, c('num'), 1),
            val_arg(auc_greater, c('char'), 1, c('1', '2')),
            val_arg(hush, c('bool'), 1),
            val_arg(plots, c('bool', 'null'), 1),
            val_arg(rug_size, c('num'), 1),
            val_arg(aspect_ratio, c('num', 'null'), 1),
            val_arg(y_label, c('null', 'char'), 1),
            val_arg(x_label, c('null', 'char'), 1),
            val_arg(factor_name, c('null', 'char'), 1),
            val_arg(var_names, c('char'), 0),
            val_arg(reverse, c('bool'), 1)
        )
    )
    if (is.null(ci)) {
        if (is.null(greater)) {
            ci = 0.95
        }
        else {
            ci = 0.90
        }
    }
    greater = toString(greater)
    if (anyNA(var1) || anyNA(var2)) {
        if (pair == TRUE) {
            message("NA values omitted pairwise.")
            if (anyNA(var1)) {
                var2 = var2[!is.na(var1)]
                var1 = var1[!is.na(var1)]
            }
            if (anyNA(var2)) {
                var1 = var1[!is.na(var2)]
                var2 = var2[!is.na(var2)]
            }
        } else {
            var1 = var1[!is.na(var1)]
            var2 = var2[!is.na(var2)]
            message("NA values omitted.")
        }
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
        norm_tests_in(
            var1 = var1,
            var2 = var2,
            pair = pair,
            norm_tests = norm_tests,
            alpha = 0.05,
            hush = FALSE,
            plots = norm_plots,
            tneet = TRUE,
            nonparametric = nonparametric,
            aspect_ratio = 1
        )
    }
    descr_1 = paste0(ro(mean(var1), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var1), round_descr))
    descr_2 = paste0(ro(mean(var2), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var2), round_descr))
    if (bf_added == TRUE &&
        nonparametric == TRUE) {
        if (pair == TRUE) {
            delta_samps = signRankGibbsSampler(
                var1,
                var2,
                progBar = (!hush),
                nSamples = bf_sample,
                cauchyPriorParameter = bf_rscale
            )$deltaSamples
        } else {
            delta_samps = rankSumGibbsSampler(
                var1,
                var2,
                progBar = (!hush),
                nSamples = bf_sample,
                cauchyPriorParameter = bf_rscale
            )$deltaSamples
        }
        if (hush == FALSE) {
            cat('', fill = TRUE)
        }
    }
    if (pair == TRUE & r_added == TRUE & hush == FALSE) {
        if (nonparametric == TRUE) {
            cat("Spearman's rank correlation: ")
            corr_neat(
                var1,
                var2,
                nonparametric = TRUE,
                ci = 0.95,
                bf_added = FALSE,
                hush = hush
            )
        } else {
            cat("Pearson correlation: ")
            corr_neat(
                var1,
                var2,
                ci = 0.95,
                bf_added = FALSE,
                hush = hush
            )
        }
    }
    if (greater == "1") {
        if (hush == FALSE) {
            message(
                "One-sided t-test and BF (with 90% CI default)! H1: first is greater than second."
            )
        }
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                alternative = "greater",
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(
                var1,
                var2,
                paired = pair,
                alternative = "greater",
                conf.level = ci
            )
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               oneSided = "right",
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(
                    BayesFactor::ttestBF(
                        var1,
                        var2,
                        paired = pair,
                        iterations = bf_sample,
                        rscale = bf_rscale,
                        nullInterval = c(0, Inf)
                    )[1]
                )
            }
        }
    } else if (greater == "2") {
        if (hush == FALSE) {
            message(
                "One-sided t-test and BF (with 90% CI default)! H1: second is greater than first."
            )
        }
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                alternative = "less",
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(
                var1,
                var2,
                paired = pair,
                alternative = "less",
                conf.level = ci
            )
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               oneSided = "left",
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(
                    BayesFactor::ttestBF(
                        var1,
                        var2,
                        paired = pair,
                        iterations = bf_sample,
                        rscale = bf_rscale,
                        nullInterval = c(0, -Inf)
                    )[1]
                )
            }
        }
    } else {
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(var1,
                                  var2,
                                  paired = pair,
                                  conf.level = ci)
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(
                    BayesFactor::ttestBF(
                        var1,
                        var2,
                        paired = pair,
                        iterations = bf_sample,
                        rscale = bf_rscale
                    )
                )
            }
        }
    }
    if (bf_added == TRUE) {
        bf_out = bf_neat(bf)
    } else {
        bf_out = "."
        bf = NA
    }
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    n1 = length(var1)
    n2 = length(var2)
    t_eq = stats::t.test(var1, var2, paired = pair, var.equal = TRUE)$statistic
    if (pair == TRUE) {
        sm = quiet(MBESS::ci.sm(
            ncp = t_eq,
            N = n1,
            conf.level = ci
        ))
        d_orig = sm$Standardized.Mean
        d = paste0("d = ", ro(d_orig, 2))
        df = ro(df, 0)
        lower = ro(sm$Lower.Conf.Limit.Standardized.Mean, 2)
        upper = ro(sm$Upper.Conf.Limit.Standardized.Mean, 2)
    } else {
        the_smd = MBESS::ci.smd(
            ncp = t_eq,
            n.1 = n1,
            n.2 = n2,
            conf.level = ci
        )
        d_orig = the_smd$smd
        d = paste0("d = ", ro(d_orig, 2))
        df = ro(df, 1)
        lower = ro(the_smd$Lower.Conf.Limit.smd, 2)
        upper = ro(the_smd$Upper.Conf.Limit.smd, 2)
    }
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    if (nonparametric == TRUE) {
        titl = "Nonpar. difference estimate: "
        m_dif = ro(ttest$estimate, round_descr)
    } else {
        titl = "Mean difference (var1CHAR_MINUSvar2): "
        m_dif = ro(mean(var1) - mean(var2), round_descr)
    }
    ci_r_low = ro(ttest$conf.int[1], round_descr)
    ci_r_upp = ro(ttest$conf.int[2], round_descr)
    if (greater == "1") {
        ci_r_upp = 'CHAR_INF'
        upper = 'CHAR_INF'
    } else if (greater == "2") {
        ci_r_low = 'CHAR_MINUSCHAR_INF'
        lower = 'CHAR_MINUSCHAR_INF'
    }
    if (hush == FALSE) {
        if (nonparametric == TRUE) {
            if (pair == TRUE) {
                outbegin = "W = "
            } else {
                outbegin = "U = "
            }
        } else {
            outbegin = paste0("t(",
                              df,
                              ") = ")
        }
        if (!is.null(test_title) && hush == FALSE) {
            cat(test_title, fill = TRUE)
        }
        prnt(
            titl,
            m_dif,
            ci_disp,
            " [",
            ci_r_low,
            ", ",
            ci_r_upp,
            "] ",
            "(MeanCHAR_PLUSMINSD = ",
            descr_1,
            " vs. ",
            descr_2,
            "; MedianCHAR_PLUSMINMAD = ",
            paste0(
                ro(stats::median(var1), round_descr),
                "CHAR_PLUSMIN",
                ro(stats::mad(var1), round_descr)
            ),
            " vs. ",
            paste0(
                ro(stats::median(var2), round_descr),
                "CHAR_PLUSMIN",
                ro(stats::mad(var2), round_descr)
            ),
            "), ",
            paste0(
                outbegin,
                ro(t, 2),
                ", p = ",
                ro(pvalue, 3),
                ", ",
                d,
                ci_disp,
                " [",
                lower,
                ", ",
                upper,
                "]",
                bf_out
            )
        )
    }
    cv_cdrs = NULL
    if (auc_added == TRUE) {
        if (auc_greater == "2") {
            auc_dir = ">" # v2 expected larger
            v_large = var2
            v_small = var1
        } else {
            auc_dir = "<" # v1 expected larger
            v_large = var1
            v_small = var2
        }
        the_roc = pROC::roc(
            response = c(rep(0, length(var2)), rep(1, length(var1))),
            predictor = c(var2, var1),
            levels = c(0, 1),
            direction =  auc_dir
        ) # v1 larger
        youdn = pROC::coords(the_roc, x = "best", ret = "youden")
        if (inherits(youdn, "data.frame")) {
            maxyouden = as.numeric(youdn$youden[1]) - 1
        } else {
            maxyouden = as.numeric(youdn[1]) - 1
        }
        bestacc = pROC::coords(the_roc, x = "best", ret = "accuracy")
        if (inherits(bestacc, "data.frame")) {
            max_acc = as.numeric(bestacc$accuracy[1])
        } else {
            max_acc = as.numeric(bestacc[1])
        }
        best_coords = pROC::coords(the_roc, x = "best")
        if (inherits(best_coords, "data.frame")) {
            plot_thres = as.numeric(best_coords$threshold)[1]
            best_tp = as.numeric(best_coords$sensitivity)[1]
            best_tn = as.numeric(best_coords$specificity)[1]
        } else {
            plot_thres = as.numeric(best_coords["threshold"])[1]
            best_tp = as.numeric(best_coords["sensitivity"])[1]
            best_tn = as.numeric(best_coords["specificity"])[1]
        }
        if (cv_rep != FALSE) {
            if (cv_rep == TRUE) {
                cv_rep = 100
            }
            cv_cdrs = docv_auc(v_large,
                               v_small,
                               cv_rep,
                               cv_fold)
        }
        the_auc = pROC::auc(the_roc)
        if (hush == FALSE) {
            if (!is.null(cutoff)) {
                plot_thres = cutoff
                cutoff = cutoff[1]
                showthres = cutoff
            } else {
                showthres = plot_thres
            }
            if (!is.null(cv_cdrs)) {
                sd_tp = paste0('CHAR_PLUSMIN', edges(stats::sd(cv_cdrs$TPRs), round_auc))
                sd_tn = paste0('CHAR_PLUSMIN', edges(stats::sd(cv_cdrs$TNRs), round_auc))
                sd_th = paste0('CHAR_PLUSMIN', edges(stats::sd(cv_cdrs$thresholds), round_auc))
                best_tp = mean(cv_cdrs$TPRs)
                best_tn = mean(cv_cdrs$TNRs)
                plot_thres = mean(cv_cdrs$thresholds)
            } else {
                sd_tp = NULL
                sd_tn = NULL
                sd_th = NULL
                if (!is.null(cutoff)) {
                    if (auc_greater == '1') {
                        best_tp = mean(var1 > cutoff)
                        best_tn = mean(var2 <= cutoff)
                    } else {
                        best_tp = mean(var1 < cutoff)
                        best_tn = mean(var2 >= cutoff)
                    }
                }
            }
            show_auc(
                theroc = the_roc,
                ci = ci,
                round_to = round_auc,
                for_table = for_table,
                thres = showthres,
                best_tp = best_tp,
                best_tn = best_tn,
                sd_tp = sd_tp,
                sd_tn = sd_tn,
                sd_th = sd_th
            )
        }
        plot_thres = plot_thres[!plot_thres %in% c(-Inf, Inf)]
    } else {
        the_auc = NULL
        max_acc = NULL
        maxyouden = NULL
        the_roc = NULL
        best_coords = NULL
        plot_thres = NULL
    }
    if (is.null(plots) || plots == TRUE) {
        the_plot = plot_dens(
            v1 = var1,
            v2 = var2,
            y_label = y_label,
            x_label = x_label,
            thres = plot_thres,
            factor_name = factor_name,
            var_names = var_names,
            reverse = reverse,
            hist = plots,
            rug_size = rug_size,
            aspect_ratio = aspect_ratio,
            nonparam = nonparametric
        )
        graphics::plot(the_plot)
    } else {
        the_plot = NULL
    }
    invisible(
        list(
            stats = c(
                t = as.numeric(t),
                p = pvalue,
                d = as.numeric(d_orig),
                bf = as.numeric(bf),
                auc = the_auc,
                accuracy = max_acc,
                youden = maxyouden
            ),
            roc_obj = the_roc,
            best_thresholds = best_coords,
            cv_results = cv_cdrs,
            t_plot = the_plot
        )
    )
}

## density plot

plot_dens = function(v1,
                     v2,
                     y_label,
                     x_label,
                     thres,
                     factor_name,
                     var_names,
                     reverse,
                     hist,
                     rug_size,
                     aspect_ratio,
                     nonparam) {
    dens_dat = data.frame(vals = c(v1, v2),
                          facts = c(rep(var_names[1], length(v1)),
                                    rep(var_names[2], length(v2))))
    if (reverse == TRUE) {
        dens_dat$facts =  factor(dens_dat$facts, levels = c(var_names[2], var_names[1]))
        colrs = c('#006600', '#b3b3ff')
    } else {
        colrs = c('#b3b3ff', '#006600')
    }
    if (is.null(hist)) {
        max_1 = (max(v1) - min(v1)) / 10
        max_2 = (max(v2) - min(v2)) / 10
        freed1 = 2 * stats::IQR(v1) / (length(v1) ^ (1 / 3))
        freed2 = 2 * stats::IQR(v2) / (length(v2) ^ (1 / 3))
        my_binwidth = min(max_1, max_2, freed1, freed2)
        the_plot = ggplot(dens_dat,
                          aes(
                              x = .data$vals,
                              fill = .data$facts,
                              color = .data$facts
                          )) +
            geom_histogram(
                aes(y = .data$..count..),
                alpha = 0.1,
                binwidth = my_binwidth,
                color = 'darkgray',
                position = "identity"
            ) +
            geom_density(aes(y = .data$..count.. * my_binwidth), alpha = 0.3)
    } else {
        the_plot = ggplot(dens_dat,
                          aes(
                              x = .data$vals,
                              fill = .data$facts,
                              color = .data$facts
                          )) +
            geom_density(aes(y = .data$..count.. / sum(.data$..count..)),
                         alpha = 0.3)
    }
    if (rug_size > 0) {
        yrange = ggplot_build(the_plot)$layout$panel_params[[1]]$y.range
        hght = yrange[2] - yrange[1]
        dens_dat$ypos <- -hght / 20
        dens_dat$ypos[dens_dat$facts == var_names[2]] <-
            -(2 * hght / 20)
        the_plot = the_plot + geom_point(
            aes(
                x = .data$vals,
                y = dens_dat$ypos,
                colour = .data$facts
            ),
            alpha = 1,
            shape = '|',
            size = rug_size
        )
    }
    if (!is.null(thres)) {
        the_plot = the_plot +
            geom_vline(
                xintercept = c(thres),
                color = "#8f8f8f",
                linetype = "solid",
                size = 0.5
            )
    }
    if (nonparam == TRUE) {
        xfunc = stats::median
    } else {
        xfunc = mean
    }
    return(
        the_plot + scale_fill_manual(values = c('#004d00', '#8080ff'),
                                     name = factor_name) +
            scale_color_manual(
                values = c('#004d00', '#8080ff'),
                guide = FALSE
            ) +
            geom_vline(
                xintercept = c(xfunc(dens_dat$v1), xfunc(dens_dat$v2)),
                color = "#777777",
                linetype = "dashed",
                size = 0.5
            ) + theme_bw() +
            ylab(y_label) + xlab(x_label) + theme(
                aspect.ratio = aspect_ratio,
                text = element_text(family = "serif", size = 17)
            )
    )
}
