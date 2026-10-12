#' @rdname CIplot
#' @include CIplot.glm.R
#'
#' @method CIplot ORci
#' @export
#'
#' @keywords plot
#' @keywords ORci
#'
CIplot.ORci <-
    function(x, xlog = TRUE, xlab = "Odds Ratio", v = 1, ...)
{
    xi <- x
    conf.level <- attr(x, "conf.level")
    xi <- unclass(x)
    CIplot.default(xi,
                   xlog = xlog, xlab = xlab,
                   conf.level = conf.level,
                   v = v,
                   ...)
}

