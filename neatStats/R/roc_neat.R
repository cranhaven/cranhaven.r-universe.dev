#'@title Difference of Two Areas Under the Curves
#'
#'@description Comparison of two \code{\link[neatStats:t_neat]{areas under the
#'  receiver operating characteristic curves}} (AUCs) and plotting any number of
#'  ROC curves.
#'@param roc1 Receiver operating characteristic (ROC) \code{\link[pROC:roc]{
#'  object}}, or, for plotting only, a \code{\link{list}} including any number
#'  of such ROC objects.
#'@param roc2 Receiver operating characteristic (ROC) \code{\link[pROC:roc]{
#'  object}}, or, for plotting only, leave it as \code{NULL} (default) and
#'  provide list for the first parameter (\code{roc1}).
#'@param pair Logical. If \code{TRUE}, the test is conducted for paired samples.
#'  Otherwise (default) for independent samples.
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided test: either "1" (\code{roc1} AUC expected to be greater than
#'  \code{roc2} AUC) or "2" (\code{roc2} AUC expected to be greater than
#'  \code{roc2} AUC). If \code{NULL} (default), the test is two-sided.
#'@param ci Numeric; confidence level for the returned CIs (raw difference).
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console
#'  (and plotting).
#'@param plot_rocs Logical. If \code{TRUE}, plots and returns ROC curves.
#'@param roc_labels Optional character vector to provide legend label texts (in
#'  the order of the provided ROC objects) for the ROC plot.
#'@param cutoff_auto Logical. If \code{TRUE} (default), optimal cutoffs
#'  on the ROC plots are displayed.
#'@param cutoff_custom Custom cutoff to be indicated on the plot can be given
#'  here in a \code{list}. The list index must exactly correspond to the index
#'  of the list index of the AUC (given in \code{roc1}) for which the given
#'  cutoff is intended.
#'@return Prints DeLong's test results for the comparison of the two given AUCs
#'  in APA style, as well as corresponding CI for the AUC difference.
#'  Furthermore, when assigned, returns a list  with \code{stat} (D value),
#'  \code{p} (p value), and, when plot is added, ROC plot.
#'@note The main test statistics are calculated via
#'  \code{\link[pROC:roc.test]{pROC::roc.test}} as DeLong's test (for both
#'  paired and unpaired). The \code{roc_neat} function merely prints it in APA
#'  style. The CI is calculated based on the p value, as described by Altman and
#'  Bland (2011).
#'
#'  The ROC object may be calculated via \code{\link{t_neat}}, or directly with
#'  \code{\link[pROC:roc]{pROC::roc}}.
#'
#'@references
#'
#'Altman, D. G., & Bland, J. M. (2011). How to obtain the confidence interval
#'from a P value. Bmj, 343(d2090). \doi{https://doi.org/10.1136/bmj.d2090}
#'
#'DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the
#'areas under two or more correlated receiver operating characteristic curves: a
#'nonparametric approach. Biometrics, 44(3), 837-845.
#'\doi{https://doi.org/10.2307/2531595}
#'
#'Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J. C., &
#'Muller, M. (2011). pROC: an open-source package for R and S+ to analyze and
#'compare ROC curves. BMC bioinformatics, 12(1), 77.
#'\doi{https://doi.org/10.1186/1471-2105-12-77}
#'
#' @seealso \code{\link{t_neat}}
#' @examples
#'
#' # calculate first AUC (from v1 and v2)
#' v1 = c(191, 115, 129, 43, 523,-4, 34, 28, 33,-1, 54)
#' v2 = c(4,-2, 23, 13, 32, 16, 3, 29, 37,-4, 65)
#' results1 = t_neat(v1, v2, auc_added = TRUE)
#'
#' # calculate second AUC (from v3 and v4)
#' v3 = c(14.1, 58.5, 25.5, 42.2, 13, 4.4, 55.5, 28.5, 25.6, 37.1)
#' v4 = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9)
#' results2 = t_neat(v3, v4, auc_added = TRUE)
#'
#' # one-sided comparison of the two AUCs
#' roc_neat(results1$roc_obj, results2$roc_obj, greater = "1")
#'
#'
#' # create a list of randomlz generated AUCs
#' set.seed(1)
#' aucs_list = list()
#' for (i in 1:4) {
#'     aucs_list[[i]] = t_neat(rnorm(50, (i-1)),
#'                             rnorm(50),
#'                             auc_added = TRUE,
#'                             hush = TRUE)$roc_obj
#' }
#' # depict AUCs (recognized as list)
#' roc_neat(aucs_list)
#'
#'\donttest{
#' # with custom cutoffs depicted
#' roc_neat(aucs_list,
#'          cutoff_custom = list(0.2),
#'          cutoff_auto = FALSE)
#' roc_neat(aucs_list,
#'          cutoff_custom = list(.1, c(-.5, 0), NULL, c(.7, 1.6)),
#'          cutoff_auto = FALSE)
#' roc_neat(aucs_list,
#'          cutoff_custom = list(.6, NULL, NULL, 1.1))
#'}
#'
#' @export

roc_neat = function(roc1,
                    roc2 = NULL,
                    pair = FALSE,
                    greater = NULL,
                    ci = NULL,
                    hush = FALSE,
                    plot_rocs = FALSE,
                    roc_labels = "",
                    cutoff_auto = TRUE,
                    cutoff_custom = NULL) {
    validate_args(
        match.call(),
        list(
            val_arg(pair, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('null', 'num'), 1),
            val_arg(hush, c('bool'), 1),
            val_arg(roc_labels, c('char')),
            val_arg(cutoff_auto, c('bool'), 1),
            val_arg(cutoff_custom, c('null', 'list'))
        )
    )
    if (roc_labels == "") {
        roc_labels = NA
    }
    if (is.null(roc2)) {
        return(plot_roc(roc1, roc_labels,
                        cutoff_auto,
                        cutoff_custom))
    }
    greater = toString(greater)
    if (greater == "1") {
        if (hush == FALSE) {
            message("One-sided test (with 90% CI default)! H1: first is greater than second.")
        }
        alt = "greater"
    } else if (greater == "2") {
        if (hush == FALSE) {
            message("One-sided test (with 90% CI default)! H1: second is greater than first.")
        }
        alt = "less"
    } else {
        alt = "two.sided"
        if (is.null(ci)) {
            ci = 0.95
        }
    }
    if (is.null(ci)) {
        ci = 0.90
    }
    roc_test = pROC::roc.test(roc1, roc2, paired = pair, alternative = alt)
    roc_stat = roc_test$statistic
    df = roc_test$parameter
    p_value = roc_test$p.value

    roc_test_ts = pROC::roc.test(roc1, roc2, paired = pair)
    auc_diff = as.numeric(roc1$auc) - as.numeric(roc2$auc)
    z_norm = -0.862 + sqrt(0.743 - 2.404 * log(roc_test_ts$p.value))
    auc_se = abs(auc_diff / z_norm)
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    auc_low = auc_diff - auc_se * z_c
    auc_upp = auc_diff + auc_se * z_c
    ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")

    if (pair == FALSE) {
        out = paste0(
            "D(",
            ro(df, 1),
            ") = ",
            ro(roc_stat, 2),
            ", p = ",
            ro(p_value, 3),
            " (AUC difference: ",
            ro(auc_diff, 3, leading_zero = FALSE),
            ci_disp,
            " [",
            ro(auc_low, 3, leading_zero = FALSE),
            ", ",
            ro(auc_upp, 3, leading_zero = FALSE),
            "])"
        )
    } else {
        out = paste0(
            "D = ",
            ro(roc_stat, 2),
            ", p = ",
            ro(p_value, 3),
            " (AUC difference: ",
            ro(auc_diff, 3, leading_zero = FALSE),
            ci_disp,
            " [",
            ro(auc_low, 3, leading_zero = FALSE),
            ", ",
            ro(auc_upp, 3, leading_zero = FALSE),
            "])"
        )
    }
    if (hush == FALSE) {
        prnt(out)
    }
    if (plot_rocs == TRUE) {
        plotted = plot_roc(list(roc1, roc2),
                           roc_labels,
                           cutoff_auto,
                           cutoff_custom)
        if (hush == FALSE) {
            graphics::plot(plotted)
        }
    } else {
        plotted = NA
    }
    invisible(list(
        stat = as.numeric(roc_stat),
        p = p_value,
        roc_plot = plotted
    ))
}

plot_roc = function(roc_list,
                    roc_labels,
                    cutoff_auto,
                    cutoff_custom) {
    cutoff_custom = cutoff_custom[1:min(length(roc_list),
                                        length(cutoff_custom))]
    tps = c()
    tns = c()
    cases = c()
    casenames = c()
    count = 0
    ths = list()
    for (rocx in roc_list) {
        count = count + 1
        tps = c(tps, rocx$sensitivities)
        tns = c(tns, rocx$specificities)
        if (is.na(roc_labels[count])) {
            cases = c(cases, c(rep(
                paste("ROC", count), length(rocx$specificities)
            )))
            casenames = c(casenames, paste("ROC", count))
        } else {
            cases = c(cases, c(rep(
                roc_labels[count], length(rocx$specificities)
            )))
            casenames = c(casenames, roc_labels[count])
        }
        ths[[count]] =
            pROC::coords(rocx, pROC::coords(rocx, x = "best")$threshold[1])
    }
    if (!is.null(cutoff_custom)) {
        ths_cust = list()
        count2 = 0
        for (rocx in roc_list[1:length(cutoff_custom)]) {
            count2 = count2 + 1
            if (is.numeric(cutoff_custom[[count2]])) {
                ths_cust[[count2]] =
                    pROC::coords(rocx, cutoff_custom[[count2]])
            }
        }
    }
    cases = factor(cases, levels = casenames, labels = casenames)
    roc_dat = data.frame(tp = tps,
                         tn = tns,
                         Case = cases)
    rocnum = length(roc_list)
    if (rocnum < 3) {
        lin_cols = c('#86b300', '#19004d')
    } else {
        lin_cols = viridis::viridis(rocnum, end = .85)
    }
    lin_a = 1
    rocplot = ggplot(roc_dat,
                     aes(
                         x = .data$tn,
                         y = .data$tp,
                         color = .data$Case
                     )) +
        labs(x = "True Negative Rate (Specificity)",
             y = "True Positive Rate (Sensitivity)") +
        annotate(
            'segment',
            x = 1,
            xend = 0,
            y = 0,
            yend = 1,
            color = '#dfdfdf',
            size = 0.7
        )
    count = 0
    if (cutoff_auto == TRUE) {
        for (thre in ths) {
            count = count + 1
            rocplot = rocplot +
                annotate(
                    'segment',
                    x = 1,
                    xend = thre$specificity,
                    y = thre$sensitivity,
                    yend = thre$sensitivity,
                    alpha = lin_a,
                    color = lin_cols[count],
                    linetype = 'dotted'
                ) +
                annotate(
                    'segment',
                    x = thre$specificity,
                    xend = thre$specificity,
                    y = 0,
                    yend = thre$sensitivity,
                    alpha = lin_a,
                    color = lin_cols[count],
                    linetype = 'dotted'
                )
        }
    }
    count2 = 0
    if (!is.null(cutoff_custom)) {
        for (thre in ths_cust) {
            if (!is.null(thre)) {
                count2 = count2 + 1
                rocplot = rocplot +
                    annotate(
                        'segment',
                        x = 1,
                        xend = thre$specificity,
                        y = thre$sensitivity,
                        yend = thre$sensitivity,
                        alpha = lin_a,
                        color = lin_cols[count2],
                        linetype = 'dotted'
                    ) +
                    annotate(
                        'segment',
                        x = thre$specificity,
                        xend = thre$specificity,
                        y = 0,
                        yend = thre$sensitivity,
                        alpha = lin_a,
                        color = lin_cols[count2],
                        linetype = 'dotted'
                    )
            }
        }
    }
    rocplot = rocplot +
        scale_color_manual(values = lin_cols) + geom_path(size = 0.7) +
        scale_x_reverse() + theme_bw() +
        theme(
            aspect.ratio = 1,
            legend.title = element_blank(),
            text = element_text(family = "serif", size = 17)
        )
    if (rocnum == 1) {
        rocplot = rocplot + theme(legend.position = "none")
    }
    if (cutoff_auto == TRUE) {
        for (thre in ths) {
            rocplot = rocplot +
                annotate(
                    'point',
                    x = thre$specificity,
                    y = thre$sensitivity,
                    color = '#1a1a1a',
                    size = 1
                )
        }
    }
    if (!is.null(cutoff_custom)) {
        for (thre in ths_cust) {
            if (!is.null(thre)) {
                rocplot = rocplot +
                    annotate(
                        'point',
                        x = thre$specificity,
                        y = thre$sensitivity,
                        color = '#1a1a1a',
                        size = 1.5,
                        shape = 1
                    )
            }
        }
    }
    return(rocplot)
}
