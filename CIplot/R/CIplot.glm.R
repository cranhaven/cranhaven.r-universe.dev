#' @rdname CIplot
#' @include CIplot.glht.R
#'
#' @method CIplot glm
#' @export
#'
# #' @param conf.level \code{glm} object only.
# #'                   the confidence interval. Default is 0.95.
# #'                   see also \code{\link{ORci}}.
#'
#' @examples
#'
#' ##### 'glm' object: logistic regression only!
#' ## odds ratio
#' require(graphics)
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' CIplot(x, las = 1)
#'
#' @keywords plot
#' @keywords glm
#'
CIplot.glm <-
    function(x, conf.level = 0.95, xlog = TRUE, xlab = "Odds Ratio", v = 1, ...)
{
    ci <- ORci(x, conf.level = conf.level)
    CIplot.ORci(ci,
                xlog = xlog, xlab = xlab,
                v = v,
                ...)
}

