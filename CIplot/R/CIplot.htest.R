#' @rdname CIplot
#' @include CIplot.default.R
#'
#' @method CIplot htest
#' @export
#'
#' @examples
#' ##### 'htest' objects
#' require(graphics)
#'
#' ## t test
#' set.seed(1234)
#' a <- rnorm(10, 10, 2); b <- rnorm(10, 8, 2)
#' x <- t.test(a, b)
#' CIplot(x)
#'
#' ## binomial test
#' x <- binom.test(5, 20)
#' CIplot(x, xlim = c(0, 1))
#'
#' ## Fisher's exact test
#' x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
#' res <- fisher.test(x)
#' CIplot(res, xlog = TRUE)
#'
#' @keywords plot
#' @keywords htest
#'
CIplot.htest <-
    function(x, xlog = FALSE, xlim = NULL, xlab = NULL, yname = FALSE, v = NULL, ...)
{
    est <- x$estimate
    ci <- x$conf.int

    if (is.null(ci)) {
        warning("No information about confidential interval\n")
        return(invisible(NULL))
    }

    if (length(est) == 2L) est <- est[1] - est[2]

    if (is.null(v)) {
        if (is.null(x$null.value)) {
            v <- ifelse(log, 1, 0)
        } else {
            v <- x$null.value
        }
    }

    if (is.null(xlim)) {
        xlim <- c(min(v, ci[1]), max(v, ci[2]))
    }

    if (is.null(xlab)) {
        xlab <- attr(x$null.value, "names")
        if (is.null(xlab)) {
            xlab <- ifelse(log, "ratio", "difference")
        }
    }

    tmp <- data.frame(estimate = est, lwr = ci[1], upr = ci[2])
    CIplot.default(tmp, xlog = xlog, xlim = xlim, xlab = xlab,
                   yname = yname,
                   conf.level = attr(x$conf.int, "conf.level"),
                   v = v,
                   ...)
}

