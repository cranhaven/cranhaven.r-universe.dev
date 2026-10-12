#' @rdname CIplot
#' @include CIplot.R
#'
#' @method CIplot default
#' @export
#'
#' @param x \code{default}: \code{matrix} or \code{data.frame} class
#'          with 3 columns ('any name', \code{lwr}, \code{upr}),
#'          or an object: \code{htest}, \code{TukeyHSD},
#'          \code{glht} (\pkg{multcomp}),
#'          \code{glm} (logistic regression only!)
#'          or \code{posthocTGH} (\pkg{userfriendlyscience}).
#' @param xlog (logical) if \code{log} is \code{TRUE},
#'             the x axis is drawn logarithmically.
#'             Default is \code{FALSE}.
#' @param xlab a title for the plot.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param yname If \code{yname} is \code{TRUE},
#'              the name of comparison between groups are shown.
#' @param las  numeric in {0,1,2,3}; the style of axis labels.
#'             Default is 0. see also \code{par}.
#' @param pch plotting 'character', i.e., symbol to use.
#' @param pcol color code or name of the points.
#' @param pcolbg background (fill) color for the open plot symbols
#'               given by 'pch = 21:25'.
#' @param pcex character (or symbol) expansion of points.
#' @param conf.level \code{default} and \code{glm} object only.
#'                   the confidence interval. Default is 0.95.
#'                   see also \code{\link{ORci}}.
#' @param cilty line types of conficence intervals.
#' @param cilwd line width of conficence intervals.
#' @param cicol color code or name of conficence intervals.
#' @param v the x-value(s) for vertical line.
#' @param vlty line types of vertical line.
#' @param vlwd line width of vertical line.
#' @param vcol color code or name of vertical line.
#' @param main  a main title for the plot.
#' @param \dots other options for x-axis.
#'
#' @examples
#' ##### default (matrix or data.frame)
#' require(graphics)
#' x <- matrix(c(3, 1, 5,
#'               4, 2, 6), 2, 3, byrow = TRUE)
#' colnames(x) <- c("esti", "lwr", "upr")
#' rownames(x) <- c("A", "B")
#' CIplot(x, xlab = "difference", v = 2, las = 1)
#'
#' @keywords plot
#'
## x: matrix or data.frame(estimation, lwr, upr)
## v: required
CIplot.default <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = NULL,             ## x-axis
             yname = TRUE, las = 0,                              ## y-axis
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,     ## points
             conf.level = 0.95, cilty = 1, cilwd = 1, cicol = 1, ## conf.int
             v, vlty = 2, vlwd = 1,  vcol = 1,                   ## vertical
             main = NULL,                                        ## Title
             ...)
{
    if (ncol(x) < 3L) {
        stop("x is required at least 3 columns.\n")
    }

    if (is.matrix(x)) x <- as.data.frame(x)
    if (is.null(main)) {
        main <- paste0(format(100 * conf.level, digits = 2L),
                       "% confidence interval")
    }

    xi <- x[,1:3]         ## estimation, lwr, upr
    yvals <- nrow(xi):1L

    if (is.null(xlim)) xlim <- c(min(xi), max(xi))

    plot(c(xi[,"lwr"], xi[,"upr"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE,
         xlab = xlab, ylab = "",
         xlim = xlim, log = ifelse(xlog, "x", ""),
         ylim = c(0.5, nrow(xi) + 0.5),
         main = main)

    axis(1, ...)
    if (yname) {
        axis(2, at = nrow(xi):1,
             labels = dimnames(xi)[[1L]], las = las)
    }

    abline(v = v, lty = vlty, lwd = vlwd, col = vcol)
    arrows(xi[, "lwr"], yvals, xi[, "upr"], yvals,
           code = 3, angle = 90, length = 0.15,
           lty = cilty, lwd = cilwd, col = cicol)
    points(xi[, 1], yvals, pch = pch, cex = pcex,
           col = pcol, bg = pcolbg)
    box()
}

