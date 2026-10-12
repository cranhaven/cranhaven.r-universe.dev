#' @rdname CIplot
#' @include CIplot.htest.R
#'
#' @aliases CIplot.TukeyHSD
#'
#' @method CIplot TukeyHSD
#' @export
#'
#' @examples
#'
#' ##### 'TukeyHSD' objects
#' require(graphics)
#'
#' ## Tukey test
#' aov1 <- aov(breaks ~ tension + wool, data = warpbreaks)
#' x <- TukeyHSD(aov1)
#'
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1, 2))
#' CIplot(x, las = 1)
#' par(oldpar)
#'
#' ## example of line type and color
#' aov1 <- aov(breaks ~ tension, data = warpbreaks)
#' x <- TukeyHSD(aov1)
#' CIplot(x, las = 1,
#'        pcol = 2:4, pcolbg = 2:4, cicol = 2:4,
#'        vlty = 1, vcol = "gray")
#'
#' @keywords plot
#' @keywords TukeyHSD
#'
CIplot.TukeyHSD <-
    function(x, xlab = "Differences in mean", v = 0, ...)
{
    conf.level <- attr(x, "conf.level")
    for (i in seq_along(x)) {
        xi <- x[[i]]
        CIplot.default(xi, xlab = xlab, v = v, ...)
    }
}

