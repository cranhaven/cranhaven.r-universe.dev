#'@title Variance Equality Tests
#'
#'@description Displayed sample sizes and SDs and performs Brown-Forsythe and
#'  Fligner-Killeen variance equality tests (tests of homogeneity of variances)
#'  per group combinations. This is primarily a subfunction of
#'  \code{\link{anova_neat}}, but here it is available separately for other
#'  potential purposes.
#'@param xvar Either a numeric vector (numbers of any given variable), or, if
#'  \code{dat} is given, a column name specifying the variable in the given data
#'  frame.
#'@param group_by Either a vector of factors with which to group the \code{xvar}
#'  values, or, if \code{dat} is given, one or more column names specifying the
#'  columns in the given data frame.
#'@param dat Either \code{NULL} or a data frame from which the respective column
#'  names should be selected for \code{xvar} and \code{group}.
#'@param sep String (underscore \code{"_"} by default) for separating group
#'  names.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints test results.
#'
#'@note
#'
#'Brown-Forsythe test (i.e., Levene's test using medians) is calculated via
#'\code{\link[car:leveneTest]{car::leveneTest}}. Fligner-Killeen test, which may
#'be more robust (i.e., less affected by non-normal distribution), is calculated
#'via \code{\link[stats:fligner.test]{stats::fligner.test}}. (See also Conover
#'et al., 1981, p. 360.)
#'
#'@references
#'
#'Brown, M. B. & Forsythe, A. B. (1974). Robust tests for the equality of
#'variances. Journal of the American Statistical Association, 69, pp. 364-367.
#'
#'Conover W. J., Johnson M. E., & Johnson M. M. (1981). A comparative study of
#'tests for homogeneity of variances, with applications to the outer continental
#'shelf bidding data. Technometrics, 23, 351–361.
#'
#'Fligner, M. A. & Killeen, T. J. (1976). Distribution-free two-sample tests for
#'scale. ‘Journal of the American Statistical Association. 71(353), 210-213.
#'
#'Fox, J. & Weisberg, S. (2019) An R Companion to Applied Regression, Third
#'Edition, Sage.
#'
#'Levene, H. (1960). Robust tests for equality of variances. In I. Olkin, H.
#'Hotelling, et al. (eds.). Contributions to Probability and Statistics: Essays
#'in Honor of Harold Hotelling. Stanford University Press. pp. 278–292.
#'
#' @seealso \code{\link{anova_neat}}
#' @examples
#'
#' data("ToothGrowth") # load base R example dataset
#'
#' # the statistics of the four functions below should match
#' var_tests(ToothGrowth$len, ToothGrowth$supp)
#' var_tests('len', 'supp', ToothGrowth)
#' car::leveneTest(len ~ supp, data = ToothGrowth)
#' stats::fligner.test(len ~ supp, ToothGrowth)
#'
#' # again the results below should match each other
#' var_tests(ToothGrowth$len,
#'           interaction(ToothGrowth$supp, ToothGrowth$dose))
#' var_tests('len', c('supp', 'dose'), ToothGrowth)
#' car::leveneTest(len ~ supp * as.factor(dose), data = ToothGrowth)
#' stats::fligner.test(len ~ interaction(supp, dose), ToothGrowth)
#'
#' @export
var_tests = function(xvar,
                     group_by,
                     dat = NULL,
                     hush = FALSE,
                     sep = ', ') {
    if (typeof(dat) == "character") {
        dat = eval(parse(text = dat))
    }
    validate_args(match.call(),
                  list(val_arg(xvar, c('num', 'char')),
                       val_arg(dat, c('null', 'df'), 1)))
    if (!is.null(dat)) {
        if (typeof(xvar) == 'character') {
            checkcol(names(dat), xvar)
            xvar = dat[[xvar]]
        }
        if (typeof(group_by) == 'character') {
            group_by = eval(parse(
                text = paste0(
                    'with(data = dat, paste(',
                    paste(group_by, collapse = ','),
                    ", sep = '",
                    sep,
                    "'))"
                )
            ))
        }
    }
    group_by = as.factor(as.character(group_by))
    df_mes = data.frame(xvar = xvar, group_by = group_by)
    df_sds = aggr_neat(
        df_mes,
        'xvar',
        group_by = 'group_by',
        new_name = 'sd',
        method = function(x) {
            stats::sd(x, na.rm = TRUE)
        }
    )
    df_sds$n = aggr_neat(
        df_mes,
        'xvar',
        group_by = 'group_by',
        method = function(x) {
            length(stats::na.omit(x))
        }
    )$aggr_value
    sds_zip = paste(paste0(
        df_sds$aggr_group,
        ': n = ',
        ro(df_sds$n, 2, signi = TRUE),
        ', SD = ',
        ro(df_sds$sd, 2)
    ),
    collapse = '; ')
    lev_med = car::leveneTest(y = xvar, group = group_by)
    fk_med = stats::fligner.test(x = xvar, g = group_by)
    if (hush == FALSE) {
        prnt(
            "  ",
            sds_zip,
            ".\n  Brown-Forsythe: F(",
            lev_med$Df[1],
            ",",
            lev_med$Df[2],
            ")",
            " = ",
            ro(lev_med$`F value`[1], 2),
            ", p = ",
            ro(lev_med$`Pr(>F)`[1], 3),
            "; Fligner-Killeen: X2(",
            fk_med$parameter,
            ")",
            " = ",
            ro(fk_med$statistic, 3),
            ", p = ",
            ro(fk_med$p.value, 3),
            '.'
        )
    }

    invisible(list(
        df_sds = df_sds,
        Brown = list(
            title = "Brown-Forsythe",
            Fval = lev_med$`F value`[1],
            df1 = lev_med$Df[1],
            df2 = lev_med$Df[2],
            pval = lev_med$`Pr(>F)`[1]
        ),
        Fligner = list(
            title = "Fligner-Killeen",
            X2 = fk_med$statistic[[1]],
            df = fk_med$parameter[[1]],
            pval = fk_med$p.value
        )
    ))
}

