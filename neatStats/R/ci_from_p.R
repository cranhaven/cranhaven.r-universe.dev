#' @title CI from p value
#'
#'@description Calculates approximate confidence interval (CI) for any given
#'  difference, based on the difference value and p value, according to Altman &
#'  Bland (2011).
#'@param diff Difference (number) around which the CI is to be calculated.
#'@param p_value The p value for CI calculation.
#'@param ci Numeric; confidence level for the returned CIs (\code{.95} per
#'  default).
#'@return CI limits  as named numeric vector with two elements.
#'@note
#'
#'Check the Altman & Bland (2011) paper for details! (The calculation for
#'proportions is not implemented here.)
#'
#'@references
#'
#'Altman, D. G., & Bland, J. M. (2011). How to obtain the confidence interval
#'from a P value. Bmj, 343(d2090). \doi{https://doi.org/10.1136/bmj.d2090}
#'
#' @examples
#'
#' # Example 1
#' # calculate proportion difference test
#' proptest_stat = prop.test(x = c(49, 40), n = c(50, 50))
#'
#' # calculate proportion difference
#' my_diff = 49/50-40/50
#'
#' # calculate approximate CI
#' ci_from_p(my_diff, proptest_stat$p.value)
#'
#' # returned CI should be very similar to the actual CI
#' proptest_stat$conf.int
#'
#'
#' # Example 2
#' # generate random data
#' v1 = stats::rnorm(190, 40, 60)
#' v2 = stats::rnorm(170, 50, 45)
#'
#' # calculate t-test
#' ttest_stat = stats::t.test(v1, v2)
#'
#' # calculate mean difference
#' my_diff = mean(v1) - mean(v2)
#'
#' # calculate approximate CI
#' ci_from_p(my_diff, ttest_stat$p.value)
#'
#' # returned CI should be similar to the actual CI
#' ttest_stat$conf.int
#'
#' @export
ci_from_p = function(diff,
                     p_value,
                     ci = 0.95) {
    validate_args(match.call(),
                  list(val_arg(diff, c('num')),
                       val_arg(p_value, c('num'))))
    diff = as.numeric(as.character(diff))
    p_value = as.numeric(as.character(p_value))
    z_norm = -0.862 + sqrt(0.743 - 2.404 * log(p_value))
    diff_se = abs(diff / z_norm)
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    low = diff - diff_se * z_c
    upp = diff + diff_se * z_c
    return(c(ci_lower = low, ci_upper = upp))
}
