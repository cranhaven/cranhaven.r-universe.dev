#'@title Correlation Statistics
#'
#'@description \code{\link[stats:cor.test]{Pearson correlation}} results
#'  including confidence interval (CI) and correlation
#'  \code{\link[BayesFactor:correlationBF]{Bayes factor}} (BF). For
#'  non-parametric version, Spearman's \code{\link[stats:cor.test]{rank
#'  correlation}} results along with corresponding rank-based BFs (as per van
#'  Doorn et al., 2020).
#'@param var1 Numeric vector; numbers of the first variable.
#'@param var2 Numeric vector; numbers of the second variable.
#'@param nonparametric Logical (\code{FALSE} by default). If \code{TRUE}, uses
#'  nonparametric tests (Spearman's rank correlation, including BFs; see
#'  Details).
#'@param ci Numeric; confidence level for the returned CI, as implemented in
#'  \code{\link[stats]{cor.test}}.
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed.
#'@param direction \code{NULL} or string; optionally specifies one-sided test:
#'  either "negative" (negative correlation expected) or "positive" (positive
#'  correlation expected). (Short forms also work, e.g. "p", "pos", "neg", etc.)
#'  If \code{NULL} (default), the test is two-sided.
#'@param round_r Number \code{\link[=ro]{to round}} to the correlation and its
#'  CI.
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@param sb_correction Logical. If \code{TRUE}, applies Spearman-Brown
#'  correction (\code{2 * r / (1+r)}) to the correlation (including CI).
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@details The Bayes factor (BF) is calculated with the default r-scale of
#'  \code{1/3} for parametric test, and with the default r-scale of \code{1} for
#'  nonparametric test. BF supporting null hypothesis is denoted as BF01, while
#'  that supporting alternative hypothesis is denoted as BF10. When the BF is
#'  smaller than 1 (i.e., supports null hypothesis), the reciprocal is
#'  calculated (hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than
#'  or equal to 10000, scientific (exponential) form is reported for
#'  readability. (The original full BF number is available in the returned named
#'  vector as \code{bf}.)#'
#'
# For details about the nonparametric (Spearman's rank correlation) Bayes
# factors, see van Doorn et al. (2020). The source code for the calculation is a
# contribution by J. van Doorn; the original version is available via
# https://osf.io/gny35/.
#'
#'@return Prints correlation statistics (including CI and BF) in APA style.
#'  Furthermore, when assigned, returns a named vector with the following
#'  elements: \code{r} (Pearson correlation), \code{p} (p value), \code{bf}
#'  (Bayes factor).
#'@note The correlation and CI is calculated via
#'\code{\link[stats:cor.test]{stats::cor.test}}.
#'
#'The parametric Bayes factor is calculated via
#'\code{\link[BayesFactor:correlationBF]{BayesFactor::correlationBF}}. The
#'nonparametric (rank-based) Bayes factor is a contribution by Johnny van Doorn;
#'the original source code is available via \url{https://osf.io/gny35/}.
#'
#'@references
#'
#'Brown, W. (1910). Some experimental results in the correlation of mental
#'abilities. British Journal of Psychology, 1904-1920, 3(3), 296-322.
#'\doi{https://doi.org/10.1111/j.2044-8295.1910.tb00207.x}
#'
#'Eisinga, R., Grotenhuis, M. te, & Pelzer, B. (2013). The reliability of a
#'two-item scale: Pearson, Cronbach, or Spearman-Brown? International Journal of
#'Public Health, 58(4), 637-642. \doi{https://doi.org/10.1007/s00038-012-0416-3}
#'
#'Spearman, C. (1910). Correlation calculated from faulty data. British Journal
#'of Psychology, 1904-1920, 3(3), 271-295.
#'\doi{https://doi.org/10.1111/j.2044-8295.1910.tb00206.x}
#'
#'van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2020). Bayesian
#'rank-based hypothesis testing for the rank sum test, the signed rank test, and
#'Spearman’s rho. Journal of Applied Statistics, 1–23.
#'\doi{https://doi.org/10.1080/02664763.2019.1709053}
#'
#'@seealso \code{\link{t_neat}}
#' @examples
#' # assign two variables
#' v1 = c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 )
#' v2 = c(4, -2, 23, 13, 32, 16, 3, 29, 37, -4, 65 )
#'
#' corr_neat(v1, v2) # prints statistics
#'
#' # one-sided, and omitting the "95% CI" part
#' corr_neat(v1, v2, direction = 'pos', for_table = TRUE)
#'
#' # print statistics and assign main results
#' results = corr_neat(v1, v2, direction = 'pos')
#'
#' results['p'] # get precise p value
#' @export
corr_neat = function(var1,
                     var2,
                     nonparametric = FALSE,
                     ci = .95,
                     bf_added = FALSE,
                     direction = NULL,
                     round_r = 3,
                     for_table = FALSE,
                     sb_correction = FALSE,
                     hush = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('num'), 0),
            val_arg(nonparametric, c('bool'), 1),
            val_arg(ci, c('num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(direction, c('null', 'char'), 1),
            val_arg(round_r, c('num'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    if (nonparametric == TRUE) {
        corr_method = 'spearman'
        if (bf_added == TRUE) {
            rho_samps = spearmanGibbsSampler(var1, var2, progBar = (!hush))$rhoSamples
            if (hush == FALSE) {
                cat('', fill = TRUE)
            }
        }
    } else {
        corr_method = 'pearson'
    }
    direction = toString(direction)
    if (direction != "" &&
        substr("negative", 1, nchar(direction)) == direction) {
        if (hush == FALSE) {
            message("One-sided test! Negative correlation expected.")
        }
        direction = 'neg'
        the_cor = stats::cor.test(var1,
                           var2,
                           alternative = "l",
                           conf.level = ci,
                           method = corr_method)
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(
                    rho_samps,
                    oneSided = "left",
                    whichTest = "Spearman",
                    priorParameter = 1
                )
            } else {
                bf = as.vector(BayesFactor::correlationBF(var1, var2, nullInterval = c(-1, 0))[1])
            }
        }
    } else if (direction != "" &
               substr("positive", 1, nchar(direction)) == direction) {
        if (hush == FALSE) {
            message("One-sided test! Positive correlation expected.")
        }
        direction = 'pos'
        the_cor = stats::cor.test(var1,
                           var2,
                           alternative = "g",
                           conf.level = ci,
                           method = corr_method)
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(
                    rho_samps,
                    oneSided = "right",
                    whichTest = "Spearman",
                    priorParameter = 1
                )
            } else {
                bf = as.vector(BayesFactor::correlationBF(var1, var2, nullInterval = c(0, 1))[1])
            }
        }
    } else {
        the_cor = stats::cor.test(var1, var2, conf.level = ci, method = corr_method)
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(rho_samps,
                                               priorParameter = 1,
                                               whichTest = "Spearman")
            } else {
                bf = as.vector(BayesFactor::correlationBF(var1, var2))
            }
        }
    }
    if (bf_added == TRUE) {
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
    r_raw = the_cor$estimate
    if (nonparametric == TRUE) {
        the_n = length(stats::na.omit(pmin(var1, var2)))
        se <- 1 / ((the_n - 3) ** 0.5)
        z_c <- stats::qnorm(1 - (1 - ci) / 2) * se
        low_raw <- tanh(atanh(r_raw) - z_c)
        upp_raw <- tanh(atanh(r_raw) + z_c)
    } else {
        low_raw = the_cor$conf.int[1]
        upp_raw = the_cor$conf.int[2]
    }
    if (sb_correction == TRUE) {
        r_raw = (2 * r_raw) / (1 + abs(r_raw))
        low_raw = (2 * low_raw) / (1 + abs(low_raw))
        upp_raw = (2 * upp_raw) / (1 + abs(upp_raw))
    }
    r = edges(r_raw, round_r, no_null = TRUE)
    lower = tryCatch({
        edges(low_raw, round_r, no_null = TRUE)
    },
    error = function(e) {
        return("NA")
    })
    upper = tryCatch({
        edges(upp_raw, round_r, no_null = TRUE)
    },
    error = function(e) {
        return("NA")
    })
    p_value = the_cor$p.value
    if (nonparametric == TRUE) {
        outbegin = "rs = "
    } else {
        df = the_cor$parameter
        outbegin = paste0("r(",
                          df,
                          ") = ")
    }
    out = paste0(outbegin,
                 r,
                 ci_disp,
                 " [",
                 lower,
                 ", ",
                 upper,
                 "]",
                 ", p = ",
                 ro(p_value, 3),
                 bf_out)
    if (hush == FALSE) {
        prnt(out)
    }
    invisible(c(
        r = as.numeric(the_cor$estimate),
        p = p_value,
        bf = as.numeric(bf)
    ))
}
