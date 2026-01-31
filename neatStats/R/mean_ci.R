#'@title Confidence Interval of Mean
#'@description Calculates confidence interval of a vector of numbers.
#'@param x Numeric vector.
#'@param distance_only Logical. If \code{TRUE} (default), the function returns
#'  only the distance between the mean and either confidence interval limit.
#'  Otherwise returns the confidence interval (i.e., both limits).
#'@param ci Numeric; confidence level for returned CI.
#'@return Distance of limit or confidence interval (as named vector).
#' @seealso \code{\link{se}}, \code{\link{plot_neat}}, \code{\link{sd_ci}}
#' @examples
#' myvec = c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 )
#' mean_ci( myvec, FALSE )
#' mean_ci( myvec, FALSE, ci = .80 )
#' mean_ci( myvec, ci = .80 )
#'
#' @export
mean_ci = function(x,
                   distance_only = TRUE,
                   ci = 0.95) {
    m_c = mean(x, na.rm = TRUE)
    se_c = se(x)
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    dist = z_c * se_c
    low = m_c - dist
    upp = m_c + dist
    if (distance_only == TRUE) {
        return(dist)
    } else {
        return(c(
            lower = low,
            upper = upp,
            mean = m_c,
            se = se_c
        ))
    }
}
