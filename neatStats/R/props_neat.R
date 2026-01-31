#'@title Difference of Two Proportions
#'
#'@description
#'
#'Comparison of paired and unpaired proportions. For unpaired: Pearson's
#'\code{\link[stats:prop.test]{chi-squared test}} or
#'\code{\link[Exact:exact.test]{ unconditional exact test}}, including
#'confidence interval (CI) for the proportion difference, and corresponding
#'\code{\link[BayesFactor:contingencyTableBF]{independent multinomial
#'contingency table Bayes factor}} (BF). (Cohen's h and its CI are also
#'calculated.) For paired tests, \code{\link[stats:prop.test]{classical
#'(asymptotic) McNemar test}} (optionally with mid-P as well), including
#'confidence interval (CI) for the proportion difference.
#'
#'@param var1 First variable containing classifications, in 'group 1', for the
#'  first proportion (see Examples). If given (strictly necessary for paired
#'  proportions), proportions will be defined using \code{var1} and \code{var2}
#'  (see Details). To distinguish classification ('cases' and 'controls'; e.g.
#'  positive outcomes vs. negative outcomes), any two specific characters (or
#'  numbers) can be used. However, more than two different elements (apart from
#'  \code{NA}s) will cause error.
#'@param var2 Second variable containing classifications in, 'group 2', for the
#'  second proportion, analogously to \code{var1}.
#'@param case1 Number of 'cases' (as opposed to 'controls'; e.g. positive
#'  outcomes vs. negative outcomes) in 'group 1'. As counterpart, either control
#'  numbers or sample sizes needs to be given (see Details).
#'@param case2 Number of 'cases' in 'group 2'.
#'@param control1 Number of 'controls' in 'group 1'. As counterpart, case
#'  numbers need to be given (see Details).
#'@param control2 Number of 'controls' in 'group 2'.
#'@param prop1 Proportion in 'group 1'. As counterpart, sample sizes need to be
#'  given (see Details).
#'@param prop2 Proportion in 'group 2'.
#'@param n1 Number; sample size of 'group 1'.
#'@param n2 Number; sample size of 'group 2'.
#'@param pair Logical. Set \code{TRUE} for paired proportions (McNemar, mid-P),
#'  or \code{FALSE} (default) for unpaired (chi squared, or unconditional exact
#'  test). Note: paired data must be given in \code{var1} and \code{var2}.
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided exact test: either "1" (\code{case1/n1} proportion expected to be
#'  greater than \code{case2/n2} proportion) or "2" (\code{case2/n2} proportion
#'  expected to be greater than \code{case1/n1} proportion). If \code{NULL}
#'  (default), the test is two-sided.
#'@param ci Numeric; confidence level for the returned CIs (proportion
#'  difference and Cohen's h).
#'@param bf_added Logical. If \code{TRUE}, Bayes factor is calculated and
#'  displayed. (Always two-sided!)
#'@param round_to Number \code{\link[=ro]{to round}} to the proportion
#'  statistics (difference and CIs).
#'@param exact Logical, \code{FALSE} by default. If \code{TRUE},
#'  \code{\link[Exact:exact.test]{ unconditional exact test}} is calculated and
#'  displayed, otherwise the default Pearson's
#'  \code{\link[stats:prop.test]{chi-squared test}}.
#'@param inverse Logical, \code{FALSE} by default. When \code{var1} and
#'  \code{var2} are given to calculate proportion from, by default the factors'
#'  frequency determines which are 'cases' and which are 'controls' (so that the
#'  latter are more frequent). If the \code{inverse} argument is \code{TRUE}, it
#'  reverses the default proportion direction.
#'@param yates Logical, \code{FALSE} by default. If \code{TRUE}, Yates'
#'  continuity correction is applied to the chi-squared (unpaired) or the
#'  McNemar (paired) test. Some authors advise this correction for certain
#'  specific cases (e.g., small sample), but evidence does not seem to support
#'  this (Pembury Smith & Ruxton, 2020).
#'@param midp Logical, \code{FALSE} by default. If \code{TRUE}, displays an
#'  additional 'mid-P' p value (using the formula by Pembury Smith & Ruxton,
#'  2020) for McNemar's test (Fagerland et al., 2013). This provides better
#'  control for Type I error (less false positive findings) than the classical
#'  McNemar test, while it is also probably not much less robust (Pembury Smith
#'  & Ruxton, 2020).
#'@param h_added Logical. If \code{TRUE}, Cohen's h and its CI are calculated
#'  and displayed. (\code{FALSE} by default.)
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@details
#'
#'The proportion for the two groups can be given using any of the following
#'combinations (a) two vectors (\code{var1} and \code{var2}), (b) cases and
#'controls, (c) cases and sample sizes, or (d) proportions and sample sizes.
#'Whenever multiple combinations are specified, only the first parameters (as
#'given in the function and in the previous sentence) will be taken into
#'account.
#'
#'The Bayes factor (BF), in case of unpaired samples, is always calculated with
#'the default r-scale of \code{0.707}. BF supporting null hypothesis is denoted
#'as BF01, while that supporting alternative hypothesis is denoted as BF10. When
#'the BF is smaller than 1 (i.e., supports null hypothesis), the reciprocal is
#'calculated (hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than or
#'equal to 10000, scientific (exponential) form is reported for readability.
#'(The original full BF number is available in the returned named vector as
#'\code{bf}.)
#'
#'@return Prints exact test statistics (including proportion difference with CI,
#'  and BF) in APA style. Furthermore, when assigned, returns a named vector
#'  with the following elements: \code{z} (Z), \code{p} (p value),
#'  \code{prop_diff} (raw proportion difference), \code{h} (Cohen's h),
#'  \code{bf} (Bayes factor).
#'
#'@note Barnard's unconditional exact test is calculated via
#'  \code{\link[Exact:exact.test]{Exact::exact.test}} ("z-pooled").
#'
#'  The CI for the proportion difference in case of the exact test is calculated
#'  based on the p value, as described by Altman and Bland (2011). In case of
#'  extremely large or extremely small p values, this can be biased and
#'  misleading.
#'
#'  The Bayes factor is calculated via
#'  \code{\link[BayesFactor:contingencyTableBF]{BayesFactor::contingencyTableBF}},
#'   with \code{sampleType = "indepMulti"}, as appropriate when both sample
#'  sizes (\code{n1} and \code{n2}) are known in advance (as it normally
#'  happens). (For details, see \code{\link[BayesFactor]{contingencyTableBF}},
#'  or e.g. 'Chapter 17 Bayesian statistics' in Navarro, 2019.)
#'
#'@references
#'
#'Altman, D. G., & Bland, J. M. (2011). How to obtain the confidence interval
#'from a P value. Bmj, 343(d2090). \doi{https://doi.org/10.1136/bmj.d2090}
#'
#'Barnard, G. A. (1947). Significance tests for 2x2 tables. Biometrika, 34(1/2),
#'123-138. \doi{https://doi.org/10.1093/biomet/34.1-2.123}
#'
#'Fagerland, M. W., Lydersen, S., & Laake, P. (2013). The McNemar test for
#'binary matched-pairs data: Mid-p and asymptotic are better than exact
#'conditional. BMC Medical Research Methodology, 13(1), 91.
#'\doi{https://doi.org/10.1186/1471-2288-13-91}
#'
#'Lydersen, S., Fagerland, M. W., & Laake, P. (2009). Recommended tests for
#'association in 2x2 tables. Statistics in medicine, 28(7), 1159-1175.
#'\doi{https://doi.org/10.1002/sim.3531}
#'
#'Navarro, D. (2019). Learning statistics with R.
#'\url{https://learningstatisticswithr.com/}
#'
#'Pembury Smith, M. Q. R., & Ruxton, G. D. (2020). Effective use of the McNemar
#'test. Behavioral Ecology and Sociobiology, 74(11), 133.
#'\doi{https://doi.org/10.1007/s00265-020-02916-y}
#'
#'Suissa, S., & Shuster, J. J. (1985). Exact unconditional sample sizes for the
#'2 times 2 binomial trial. Journal of the Royal Statistical Society: Series A
#'(General), 148(4), 317-327. \doi{https://doi.org/10.2307/2981892}
#'
#'@examples
#' # example data
#' set.seed(1)
#' outcomes_A = sample(c(rep('x', 490), rep('y', 10)))
#' outcomes_B = sample(c(rep('x', 400), rep('y', 100)))
#'
#' # paired proportion test (McNemar)
#' props_neat(var1 = outcomes_A,
#'            var2 = outcomes_B,
#'            pair = TRUE)
#'
#' # unpaired chi test for the same data (two independent samples assumed)
#' # Yates correction applied
#' # cf. https://www.sthda.com/english/wiki/two-proportions-z-test-in-r
#' props_neat(
#'     var1 = outcomes_A,
#'     var2 = outcomes_B,
#'     pair = FALSE,
#'     yates = TRUE
#' )
#'
#' # above data given differently for unpaired test
#' # (no Yates corrrection)
#' props_neat(
#'     case1 = 490,
#'     case2 = 400,
#'     control1 = 10,
#'     control2 = 100
#' )
#'
#' # again differently
#' props_neat(
#'     case1 = 490,
#'     case2 = 400,
#'     n1 = 500,
#'     n2 = 500
#' )
#'
#' # other example data
#' outcomes_A2 = c(rep(1, 707), rep(0, 212),  rep(1, 256), rep(0, 144))
#' outcomes_B2 = c(rep(1, 707), rep(0, 212),  rep(0, 256), rep(1, 144))
#'
#' # paired test
#' # cf. https://www.medcalc.org/manual/mcnemartest2.php
#' props_neat(var1 = outcomes_A2,
#'            var2 = outcomes_B2,
#'            pair = TRUE)
#'
#' # show reverse proportions (otherwise the same)
#' props_neat(
#'     var1 = outcomes_A2,
#'     var2 = outcomes_B2,
#'     pair = TRUE,
#'     inverse = TRUE
#' )
#'
#'
#' # two different sample sizes
#' out_chi = props_neat(
#'     case1 = 40,
#'     case2 = 70,
#'     n1 = 150,
#'     n2 = 170
#' )
#'
#' # exact test
#' out_exact = props_neat(
#'     case1 = 40,
#'     case2 = 70,
#'     n1 = 150,
#'     n2 = 170,
#'     exact = TRUE
#' )
#'
#' # the two p values are just tiny bit different
#' print(out_chi) # p 0.00638942
#' print(out_exact) # p 0.006481884
#'
#' # one-sided test
#' props_neat(
#'     case1 = 40,
#'     case2 = 70,
#'     n1 = 150,
#'     n2 = 170,
#'     greater = '2'
#' )
#'
#' @export
props_neat = function(var1 = NULL,
                      var2 = NULL,
                      case1 = NULL,
                      case2 = NULL,
                      control1 = NULL,
                      control2 = NULL,
                      prop1 = NULL,
                      prop2 = NULL,
                      n1 = NULL,
                      n2 = NULL,
                      pair = FALSE,
                      greater = NULL,
                      ci = NULL,
                      bf_added = FALSE,
                      round_to = 3,
                      exact = FALSE,
                      inverse = FALSE,
                      yates = FALSE,
                      midp = FALSE,
                      h_added = FALSE,
                      for_table = FALSE,
                      hush = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(case1, c('num', 'null'), 1),
            val_arg(case2, c('num', 'null'), 1),
            val_arg(control1, c('num', 'null'), 1),
            val_arg(control2, c('num', 'null'), 1),
            val_arg(prop1, c('num', 'null'), 1),
            val_arg(prop2, c('num', 'null'), 1),
            val_arg(n1, c('num', 'null'), 1),
            val_arg(n2, c('num', 'null'), 1),
            val_arg(pair, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('null', 'num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(round_to, c('num'), 1),
            val_arg(exact, c('bool'), 1),
            val_arg(inverse, c('bool'), 1),
            val_arg(yates, c('bool'), 1),
            val_arg(midp, c('bool'), 1),
            val_arg(h_added, c('bool'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    greater = toString(greater)
    if (pair == TRUE) {
        if (length(var1) > 1 && length(var2) > 1) {
            if (length(var1) != length(var2)) {
                stop('For paired sample, var1 and var2 must have equal lengths.')
            }
        } else {
            stop('For paired sample, var1 and var2 require vector arguments.')
        }
    }
    if (length(var1) > 1 & length(var2) > 1) {
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
        var1 = as.factor(as.character(var1))
        var2 = as.factor(as.character(var2))
        thefactors = as.character(unique(var1), unique(var2))
        if (length(thefactors) != 2) {
            stop(
                'The vectors var1 and var2 must contain exactly',
                ' two unique values (for the binary classification).'
            )
        } else {
            if (sum(c(var1 == thefactors[1],
                      var2 == thefactors[1])) > length(c(var1, var2)) / 2) {
                thefactors = c(thefactors[2], thefactors[1])
            }
            if (inverse == TRUE) {
                thefactors = c(thefactors[2], thefactors[1])
            }
        }
        n1 = length(var1)
        n2 = length(var2)
        case1 = length(var1[var1 == thefactors[2]])
        case2 = length(var2[var2 == thefactors[2]])
        prop1 = case1 / n1
        prop2 = case2 / n2
    } else if (!is.null(case1) & !is.null(case1)) {
        if (!is.null(control1)) {
            n1 = case1 + control1
            n2 = case2 + control2
        }
        prop1 = case1 / n1
        prop2 = case2 / n2
    } else if (!is.null(prop1) & !is.null(prop2) &
               !is.null(n1) & !is.null(n2)) {
        case1 = round(n1 * prop1)
        case2 = round(n2 * prop2)
    } else {
        stop('No valid data combination specified. See ?prop_neat for help.')
    }
    if ((greater == "1" | greater == "2") & is.null(ci)) {
        ci = 0.90
    } else if (is.null(ci)) {
        ci = 0.95
    }
    if (pair == TRUE) {
        p_diff = prop1 - prop2
        propdat = stats::xtabs(~ var2 + var1)
        res = stats::prop.test(propdat[2, 1],
                               propdat[2, 1] + propdat[1, 2],
                               correct = yates,
                               conf.level = ci)
        discr = propdat[2, 1] + propdat[1, 2]
        p_low = (res$conf.int[1] * 2 - 1) * discr / sum(propdat)
        p_upp = (res$conf.int[2] * 2 - 1) * discr / sum(propdat)
        if (p_diff < p_low |
            p_diff > p_upp) {
            pcitemp = c(p_low, p_upp)
            p_low = -pcitemp[2]
            p_upp = -pcitemp[1]
        }
        ci1diff = abs(p_diff - p_low)
        ci2diff = abs(p_diff - p_upp)
        if (res$conf.int[2] == 1 & ci1diff > ci2diff) {
            p_upp = p_diff + ci1diff
            if (p_low < -1) {
                p_low = -1
            }
        } else if (res$conf.int[1] == -1 & ci1diff < ci2diff) {
            p_low = p_diff - ci2diff
            if (p_upp > 1) {
                p_upp = 1
            }
        }
        if (greater == "1") {
            if (hush == FALSE) {
                message(
                    "One-sided McNemar test (with 90% CI default)! H1: first is greater than second."
                )
            }
            res = stats::prop.test(
                propdat[2, 1],
                propdat[2, 1] + propdat[1, 2],
                correct = yates,
                conf.level = ci,
                alternative = "greater"
            )
        } else if (greater == "2") {
            if (hush == FALSE) {
                message(
                    "One-sided McNemar (with 90% CI default)! H1: second is greater than first."
                )
            }
            res = stats::prop.test(
                propdat[2, 1],
                propdat[2, 1] + propdat[1, 2],
                correct = yates,
                conf.level = ci,
                alternative = "less"
            )
        }
        if (midp == TRUE) {
            if (propdat[1, 2] > propdat[2, 1]) {
                prop_b = propdat[1, 2]
                prop_c = propdat[2, 1]
            } else {
                prop_b = propdat[2, 1]
                prop_c = propdat[1, 2]
            }
            midp = (2 * stats::pbinom(prop_c,
                                      prop_c + prop_b, 0.5, lower.tail = TRUE)) -
                stats::dbinom(prop_c, prop_c + prop_b, 0.5)
            midp_inf = paste(', mid-P p =', ro(midp, 3))
        } else {
            midp = NULL
            midp_inf = ''
        }
        p_diff_out = edges(p_diff, round_to, no_null = TRUE)
        p_low_out = edges(p_low, round_to, no_null = TRUE)
        p_upp_out = edges(p_upp, round_to, no_null = TRUE)
        pvalue = res$p.value

        if (for_table == TRUE) {
            ci_disp = ""
        } else {
            ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
        }
        out = paste0(
            'X2 = ',
            ro(res$statistic, 2),
            ", p = ",
            ro(pvalue, 3),
            ", Pdiff = ",
            p_diff_out,
            ci_disp,
            " [",
            p_low_out,
            ", ",
            p_upp_out,
            "] (",
            edges(prop1, round_to, no_null = TRUE),
            " vs. ",
            edges(prop2, round_to, no_null = TRUE),
            ")",
            midp_inf
        )
        if (hush == FALSE) {
            prnt(out)
        }
        invisible(c(
            x = as.numeric(res$statistic),
            p = pvalue,
            prop_diff = p_diff,
            midp = midp
        ))
    } else {
        p_diff = prop1 - prop2
        z_c = stats::qnorm(1 - (1 - ci) / 2)
        x1 = asin(sign(prop1) * sqrt(abs(prop1)))
        x2 = asin(sign(prop2) * sqrt(abs(prop2)))
        es = x1 - x2
        se_h = sqrt(0.25 * (1 / n1 + 1 / n2))
        h_low = (es - (z_c * se_h)) * 2
        h_upp = (es + (z_c * se_h)) * 2
        h = es * 2
        if (exact == TRUE) {
            testtitle = 'Z = '
            matr = matrix(c(case1, case2, n1 - case1, n2 - case2), 2, 2)
            exact_res = Exact::exact.test(matr, to.plot = FALSE)
            z_norm = -0.862 + sqrt(0.743 - 2.404 * log(exact_res$p.value))
            if (greater == "1") {
                if (hush == FALSE) {
                    message(
                        "One-sided exact-test (with 90% CI default)! H1: first is greater than second."
                    )
                }
                exact_res = Exact::exact.test(matr,
                                              to.plot = FALSE,
                                              alternative = "greater")
            } else if (greater == "2") {
                if (hush == FALSE) {
                    message(
                        "One-sided exact-test (with 90% CI default)! H1: second is greater than first."
                    )
                }
                exact_res = Exact::exact.test(matr,
                                              to.plot = FALSE,
                                              alternative = "less")
            }
            p_se = abs(p_diff / z_norm)
            z_c = stats::qnorm(1 - (1 - ci) / 2)
            p_low = p_diff - p_se * z_c
            p_upp = p_diff + p_se * z_c
            z = abs(exact_res$statistic)
            pvalue = exact_res$p.value
        } else {
            testtitle = 'X = '
            if (greater == "1") {
                if (hush == FALSE) {
                    message(
                        "One-sided chi-squared test test (with 90% CI default)! H1: first is greater than second."
                    )
                }
                res = stats::prop.test(
                    x = c(case1, case2),
                    n = c(n1, n2),
                    correct = yates,
                    conf.level = ci,
                    alternative = "greater"
                )
            } else if (greater == "2") {
                if (hush == FALSE) {
                    message(
                        "One-sided chi-squared test (with 90% CI default)! H1: second is greater than first."
                    )
                }
                res = stats::prop.test(
                    x = c(case1, case2),
                    n = c(n1, n2),
                    correct = yates,
                    conf.level = ci,
                    alternative = "less"
                )
            } else {
                res = stats::prop.test(
                    x = c(case1, case2),
                    n = c(n1, n2),
                    correct = yates,
                    conf.level = ci
                )
            }
            p_low = res$conf.int[1]
            p_upp = res$conf.int[2]
            z = res$statistic
            pvalue = res$p.value
        }
        if (bf_added == TRUE) {
            bf = BayesFactor::contingencyTableBF(matr,
                                                 sampleType = "indepMulti",
                                                 fixedMargin = "rows")
            bf = as.vector(bf)
            bf_out = bf_neat(bf)
        } else {
            bf_out = "."
            bf = NA
        }
        if (for_table == TRUE) {
            ci_disp = ""
        } else {
            ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
        }
        if (h_added == TRUE) {
            h_out = paste0(", h = ",
                           ro(h, 2),
                           ci_disp,
                           " [",
                           ro(h_low, 2),
                           ", ",
                           ro(h_upp, 2),
                           "]")
        } else {
            h_out = ''
        }
        p_diff_out = edges(p_diff, round_to, no_null = TRUE)
        p_low_out = edges(p_low, round_to, no_null = TRUE)
        p_upp_out = edges(p_upp, round_to, no_null = TRUE)

        out = paste0(
            testtitle,
            ro(z, 2),
            ", p = ",
            ro(pvalue, 3),
            ", Pdiff = ",
            p_diff_out,
            ci_disp,
            " [",
            p_low_out,
            ", ",
            p_upp_out,
            "]",
            "] (",
            edges(prop1, round_to, no_null = TRUE),
            " vs. ",
            edges(prop2, round_to, no_null = TRUE),
            ")",
            h_out,
            bf_out
        )
        if (hush == FALSE) {
            prnt(out)
        }
        invisible(c(
            z = as.numeric(z),
            p = pvalue,
            prop_diff = p_diff,
            h = h,
            bf = as.numeric(bf)
        ))
    }
}
