#' @rdname CIplot
#' @include CIplot.TukeyHSD.R
#'
#' @aliases CIplot.glht
#'
#' @import multcomp
#'
#' @method CIplot glht
#' @export
#'
#' @examples
#' ##### 'glht' objects
#' require(graphics)
#'
#' ## Tukey test
#' require(multcomp)
#' aov1 <- aov(breaks ~ tension, data = warpbreaks)
#' x <- glht(aov1, linfct = mcp(tension = "Tukey"))
#' CIplot(x, las = 1)
#'
#' ## Dunnett test
#' x <- glht(aov1, linfct = mcp(tension = "Dunnett"))
#' CIplot(x, las = 1)
#'
#' @keywords plot
#' @keywords glht
#'
CIplot.glht <-
    function(x, xlab = "Differences in mean", v = 0, ...)
{
    xi <- confint(x)$confint
    conf.level <- attr(xi, "conf.level")

    CIplot.default(xi, xlab = xlab, v = v, ...)
}

