#'@title Confidence Interval of Standard Deviation
#'@description Calculates the SD confidence interval of a vector of numbers.
#'@param x Numeric vector.
#'@param ci Numeric; confidence level for returned CI.
#'@return SD confidence interval (as named vector).
#' @seealso \code{\link{mean_ci}}, \code{\link{plot_neat}}
#' @examples
#' myvec = c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 )
#' sd_ci( myvec )
#' sd_ci( myvec, ci = .80 )
#'
#' @export
sd_ci = function(x,
                 ci = 0.95) {
    x = x[!is.na(x)]
    n_x = length(x)
    sd_c = stats::sd(x)
    sd_c_l = sqrt((n_x - 1) / stats::qchisq((1 - ci) / 2,
                                            n_x - 1, lower.tail = FALSE))
    sd_c_u  = sqrt((n_x - 1) / stats::qchisq((1 - ci) / 2,
                                             n_x - 1, lower.tail = TRUE))
    return(c(
        lower = sd_c * sd_c_l,
        upper = sd_c * sd_c_u,
        sd = sd_c
    ))
}
