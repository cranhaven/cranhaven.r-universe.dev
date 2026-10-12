#' @rdname CIplot
#' @include CIplot.R
#'
#' @aliases CIplot.posthocTGH
#'
#' @method CIplot posthocTGH
#' @export
#'
#' @examples
#'
#' ##### 'posthocTGH' object
#' ## Tukey or Games-Howell methos
#' require(graphics)
#' if (require(userfriendlyscience)) {
#'     x <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
#'     CIplot(x, las = 1)
#' }
#'
#' @keywords plot
#' @keywords posthocTGH
#'
CIplot.posthocTGH <-
    function(x, xlab = "Differences in mean", v = 0, ...)
{

    if (x$input$method == 'tukey') {
        xi <- x$output$tukey
    }
    else if (x$input$method == 'games-howell') {
        xi <- x$output$games.howell
    }

    colnames(xi)[2:3] <- c("lwr", "upr")
    conf.level <- x$input$conf.level

    CIplot.default(xi,
                   xlab = xlab,
                   conf.level = conf.level,
                   v = v,
                   ...)
}

