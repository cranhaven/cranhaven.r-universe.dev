#'@title Normality Tests and Plots
#'
#'@description Performs normality tests and creates related plots (histogram,
#'  density, Q-Q). This is primarily a subfunction of \code{\link{t_neat}} and
#'  \code{\link{anova_neat}}, but here it is available separately for other
#'  potential purposes.
#'@param var1 Numeric vector; numbers of any given variable.
#'@param var2 Optional numeric vector (or \code{NULL}); numbers of a second
#'  variable.
#'@param pair Logical; only matters if \code{var2} is not null. In that case, if
#'  \code{TRUE} each normality test is performed for the difference values
#'  between the two variables in case of paired samples, or, if \code{FALSE},
#'  separately for each of the two variables for unpaired samples.
#'@param norm_tests Normality tests. Any or all of the following character input
#'  is accepted (as a single string or a character vector; case-insensitive):
#'  \code{"W"} (Shapiro-Wilk), \code{"K2"} (D'Agostino), \code{"A2"}
#'  (Anderson-Darling), \code{"JB"} (Jarque-Bera); see Notes. The option
#'  \code{"all"} (default value) selects all four previous tests at the same
#'  time.
#'@param alpha Numeric (\code{.05} by default), alpha level: if any p value if
#'  below this alpha level, the function returns \code{TRUE}, otherwise
#'  \code{FALSE}.
#'@param plots Logical: if \code{TRUE} adds histogram, density, and Q-Q plots.
#'  (Note: in case of paired samples, Q-Q plots are plotted on a separate
#'  figure. In RStudio, press on "Previous plot" under "Plots" to see these Q-Q
#'  plots.)
#'@param aspect_ratio Aspect ratio of the plots: \code{1} (\code{1}/\code{1}) by
#'  default. (Set to \code{NULL} for dynamic aspect ratio.)
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints normality tests, and displays plots if so specified. Returns
#'  \code{TRUE} if any of the specified tests has p value below the specified
#'  \code{alpha}, otherwise returns \code{FALSE}.
#'
#'@note
#'
#'Normality tests are all calculated via
#'\code{\link[fBasics:NormalityTests]{fBasics::NormalityTests}}, selected based
#'on the recommendation of Lakens (2015), quoting Yap and Sim (2011, p. 2153):
#'"If the distribution is symmetric with low kurtosis values (i.e. symmetric
#'short-tailed distribution), then the D'Agostino and Shapiro-Wilkes tests have
#'good power. For symmetric distribution with high sample kurtosis (symmetric
#'long-tailed), the researcher can use the JB, Shapiro-Wilkes, or
#'Anderson-Darling test." See \url{https://github.com/Lakens/perfect-t-test} for
#'more details.
#'
#'@references
#'
#'Lakens, D. (2015). The perfect t-test (version 1.0.0). Retrieved from
#'https://github.com/Lakens/perfect-t-test.
#'\doi{https://doi.org/10.5281/zenodo.17603}
#'
#'Yap, B. W., & Sim, C. H. (2011). Comparisons of various types of normality
#'tests. Journal of Statistical Computation and Simulation, 81(12), 2141–2155.
#'\doi{https://doi.org/10.1080/00949655.2010.520163}
#'
#'@seealso \code{\link{t_neat}}
#' @examples
#'
#' norm_tests(stats::rnorm(100))
#' # should be normal...
#'
#' @export
norm_tests = function(var1,
                      var2 = NULL,
                      pair = FALSE,
                      norm_tests = 'all',
                      alpha = 0.05,
                      plots = FALSE,
                      aspect_ratio = 1,
                      hush = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('null', 'num')),
            val_arg(pair, c('bool'), 1),
            val_arg(norm_tests, c('char')),
            val_arg(alpha, c('num'), 1),
            val_arg(plots, c('bool'), 1),
            val_arg(aspect_ratio, c('num', 'null'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    norm_tests_in(
        var1 = var1,
        var2 = var2,
        pair = pair,
        norm_tests = norm_tests,
        alpha = alpha,
        hush = hush,
        plots = plots,
        tneet = FALSE,
        nonparametric = FALSE,
        aspect_ratio = aspect_ratio
    )
}
norm_tests_in = function(var1,
                         var2,
                         pair,
                         norm_tests,
                         alpha,
                         hush,
                         plots,
                         tneet,
                         nonparametric,
                         aspect_ratio,
                         anov = FALSE) {
    if (pair == TRUE) {
        diff = var2 - var1
    }
    if (plots == TRUE) {
        if (anov == TRUE) {
            v1lab = 'Residuals'
        } else {
            v1lab = 'Var 1'
        }
        qqclrs = c('#000099', '#e60000', '#ff5555')
        pv1q = ggpubr::ggqqplot(var1, shape = 1) +
            xlab('Theoretical quantiles') +
            ylab(paste(v1lab, 'quantiles')) +
            theme(aspect.ratio = aspect_ratio)
        pv1q$layers[[1]]$aes_params$colour <- qqclrs[1]
        pv1q$layers[[2]]$aes_params$colour <- qqclrs[2]
        pv1q$layers[[3]]$aes_params$fill <- qqclrs[3]

        parts = c('b', 'd', 'n', 'h')
        part_colors = c(
            ha = 0.15,
            da = 0.2,
            dc = '#0000b3',
            hlc = '#737373'
        )
        pv1 = plot_neat(values = var1,
                        parts = parts,
                        part_colors = part_colors) + xlab(v1lab) +
            theme(aspect.ratio = aspect_ratio)
        if (!is.null(var2)) {
            pv2q = ggpubr::ggqqplot(var2, shape = 1) +
                xlab('Theoretical quantiles') +
                ylab('Var 2 quantiles') +
                theme(aspect.ratio = aspect_ratio)
            pv2q$layers[[1]]$aes_params$colour <- qqclrs[1]
            pv2q$layers[[2]]$aes_params$colour <- qqclrs[2]
            pv2q$layers[[3]]$aes_params$fill <- qqclrs[3]
            pv2 = plot_neat(
                values = var2,
                parts = parts,
                part_colors = part_colors
            ) + xlab('Var 2') +
                theme(aspect.ratio = aspect_ratio)
            if (pair == TRUE) {
                pv21q = ggpubr::ggqqplot(diff, shape = 1) +
                    xlab('Theoretical quantiles') +
                    ylab('Difference quantiles') +
                    theme(aspect.ratio = aspect_ratio)
                pv21q$layers[[1]]$aes_params$colour <- qqclrs[1]
                pv21q$layers[[2]]$aes_params$colour <- qqclrs[2]
                pv21q$layers[[3]]$aes_params$fill <- qqclrs[3]
                pv21 = plot_neat(
                    values = diff,
                    parts = parts,
                    part_colors = part_colors
                ) +
                    xlab('Difference (Var 2 - Var 1)') +
                    theme(aspect.ratio = aspect_ratio)
                var12 = data.frame(v1 = var1, v2 = var2)
                pscat = ggplot(var12, aes(x = .data$v1,
                                          y = .data$v2)) +
                    stat_smooth(
                        method = "lm",
                        formula = y ~ x,
                        col = "72b7cd",
                        se = FALSE,
                        size = 0.8
                    ) +
                    geom_point(shape = 23)  +
                    xlab('Var 1')  + ylab('Var 2') + theme_bw() +
                    theme(aspect.ratio = aspect_ratio)
                graphics::plot(ggpubr::ggarrange(pv1q, pv2q, pv21q))
                graphics::plot(ggpubr::ggarrange(pv1, pv2, pv21, pscat))
            } else {
                graphics::plot(ggpubr::ggarrange(pv1, pv2, pv1q, pv2q))
            }
        } else {
            graphics::plot(ggpubr::ggarrange(pv1, pv1q))
        }
    }
    norm_tests = tolower(norm_tests)
    norm_outs = c()
    norm_ps = c()
    norm_latent = FALSE
    if (norm_tests  == 'all') {
        norm_tests = c("w", "k2", "a2", "jb")
    } else if (norm_tests  == 'latent')  {
        norm_tests = c("w", "k2", "a2", "jb")
        norm_latent = TRUE
    } else {
        wrongnorm = norm_tests[!(norm_tests %in% c("w", "k2", "a2", "jb"))]
        if (length(wrongnorm) > 0) {
            message(
                'The following "norm_tests" inputs are not correct: "',
                paste(wrongnorm, collapse = '", "'),
                '". Switched to "all".'
            )
            norm_tests = c("w", "k2", "a2", "jb")
        }
    }
    if (length(var1) < 8 &
        ('k2' %in% norm_tests | 'a2' %in% norm_tests)) {
        norm_tests = norm_tests[!norm_tests %in% c('k2', 'a2')]
        warning(
            "A sample size below 8 is not reasonable for normality testing;",
            " D'Agostino and Anderson-Darling tests cannot be computed."
        )
    } else if (length(var1) < 21 & 'k2' %in% norm_tests) {
        norm_tests = norm_tests[norm_tests != 'k2']
        warning(
            "A sample size no greater than 20 is not reasonable for",
            " normality testing; D'Agostino test cannot be computed."
        )
    }
    for (norm_abbr in norm_tests) {
        ntest_fun = list(
            'w' = fBasics::shapiroTest,
            'k2' = fBasics::dagoTest,
            'a2' = fBasics::adTest,
            'jb' = fBasics::jarqueberaTest
        )[[norm_abbr]]
        stat_title = as.character(
            c(
                'w' = 'Shapiro-Wilk test: ',
                'k2' = "D'Agostino test: ",
                'a2' = "Anderson-Darling test: ",
                'jb' = "Jarque-Bera test: "
            )[norm_abbr]
        )
        if (is.null(var2)) {
            normres = ntest_fun(var1)@test
            norm_ps = c(norm_ps, normres$p.value[1])
            norm_outs = c(norm_outs,
                          paste0(
                              stat_title,
                              toupper(norm_abbr),
                              " = ",
                              ro(normres$statistic[1], 2),
                              ", p = ",
                              ro(normres$p.value[1], 3)
                          ))
        } else if (pair == TRUE) {
            normres = ntest_fun(diff)@test
            norm_ps = c(norm_ps, normres$p.value[1])
            norm_outs = c(norm_outs,
                          paste0(
                              stat_title,
                              toupper(norm_abbr),
                              " = ",
                              ro(normres$statistic[1], 2),
                              ", p = ",
                              ro(normres$p.value[1], 3)
                          ))
        } else {
            normres1 = ntest_fun(var1)@test
            normres2 = ntest_fun(var2)@test
            norm_ps = c(norm_ps, normres1$p.value[1], normres2$p.value[1])
            norm_outs = c(
                norm_outs,
                paste0(
                    stat_title,
                    toupper(norm_abbr),
                    " = ",
                    ro(normres1$statistic[1], 2),
                    ", p = ",
                    ro(normres1$p.value[1], 3),
                    ' (1st var.); ',
                    toupper(norm_abbr),
                    " = ",
                    ro(normres2$statistic[1], 2),
                    ", p = ",
                    ro(normres2$p.value[1], 3),
                    ' (2nd var.)'
                )
            )
        }
    }
    if (tneet == TRUE) {
        if (norm_latent == FALSE |
            (any(norm_ps[!is.na(norm_ps)] < alpha) &
             nonparametric == FALSE)) {
            prnt("--- Normality ---")
            prnt(paste(norm_outs, collapse = '\n'))
            prnt("--- t-test ---")
        }
    } else {
        if (hush == FALSE) {
            prnt(paste(norm_outs, collapse = '\n'))
        }
        if (any(norm_ps[!is.na(norm_ps)] < alpha)) {
            invisible(TRUE)
        } else {
            invisible(FALSE)
        }
    }
}
