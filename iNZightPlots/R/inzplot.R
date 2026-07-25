#' iNZight Plot Method
#'
#' A generic function used to plot objects within the iNZight ecosystem.
#' @param x An object
#' @param ... additional arguments for methods
#' @param ... an environment to evaluate things
#' @return The output depends on the type of input, but is usually called for
#'         the side-effect of producing a plot.
#' @export
#' @md
inzplot <- function(x, ..., env = parent.frame()) {
    UseMethod("inzplot")
}

#' @export
inzplot.default <- function(x, ..., env = parent.frame()) {
    stop("That type of object is not supported.")
}

#' iNZight Plot
#'
#' @param x A formula in the form of \code{y ~ x | g}. See Details.
#' @param data Dataset to plotq
#' @param design A survey design to use
#' @param ... Any arguments to pass to \code{\link{iNZightPlot}}
#' @param env the parent environment to pass to the plot function
#'
#' @rdname inzplot
#' @details
#' \code{inzplot} is a simple wrapper around the \code{\link{iNZightPlot}} function.
#'
#' There are four options for the formula passed in:
#'
#' \code{y} will produce a plot of the single variable \code{y}.
#'
#' \code{y ~ x} will produce a plot of \code{y} against \code{x}.
#'
#' \code{y ~ x | g1} will produce a plot of \code{y} against \code{x} subset by \code{g1}.
#'
#' \code{y ~ x | g1 + g2} will produce a plot of \code{y} against \code{x} subset by \code{g1} and \code{g2}.
#'
#' @return An \code{inzightplotoutput} object, which contains the information
#'         displayed in the plot
#' @export
#'
#' @seealso iNZightPlot
#'
#' @examples
#' data("CO2")
#' inzplot(~uptake, data = CO2)
#' inzplot(uptake ~ Treatment, data = CO2)
#' inzplot(uptake ~ Treatment | Type, data = CO2)
#' inzplot(uptake ~ Treatment | Type,
#'     data = CO2, g1.level = "Quebec"
#' )
inzplot.formula <- function(x, data = NULL, design = NULL, ..., env = parent.frame()) {
    dots <- rlang::enexprs(...)
    fmla <- parse_formula(x)

    if (!is.null(fmla$y)) {
        if (!is.null(design)) {
            varx <- design$variables[[fmla$x]]
            vary <- design$variables[[fmla$y]]
        } else {
            varx <- data[[fmla$x]]
            vary <- data[[fmla$y]]
        }
        if (is_cat(varx)) {
            # need to do a switch-a-roo
            xx <- fmla$x
            fmla$x <- fmla$y
            fmla$y <- xx
            rm("xx")
        }
    }

    exp <- rlang::expr(
        iNZightPlots::iNZightPlot(
            x = !!fmla$x,
            y = !!fmla$y,
            g1 = !!fmla$g1,
            g2 = !!fmla$g2,
            data = !!match.call()[["data"]],
            design = !!match.call()[["design"]],
            !!!dots,
            env = !!env
        )
    )

    if (is.null(fmla$y)) exp$y <- NULL
    if (is.null(fmla$g1)) exp$g1 <- NULL
    if (is.null(fmla$g2)) exp$g2 <- NULL

    eval(exp, envir = env)
}
