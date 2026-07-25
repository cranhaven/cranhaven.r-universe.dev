#' iNZight Summary Method
#'
#' A generic function used to summarize objects within the iNZight ecosystem.
#' @param x An object
#' @param ... additional arguments for methods
#' @param env an environment to evaluate things
#' @return The output depends on the type of input, and consists of a summary object with a `print` method.
#' @export
#' @md
inzsummary <- function(x, ..., env = parent.frame()) {
    UseMethod("inzsummary")
}

#' @export
inzsummary.default <- function(x, ..., env = parent.frame()) {
    stop("That type of object is not supported.")
}

#' @describeIn inzsummary Wrapper for getPlotSummary to obtain summary information about a plot
#' @param data Dataset to plotq
#' @param design A survey design to use
#' @export
inzsummary.formula <- function(x, data = NULL, design = NULL, ..., env = parent.frame()) {
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
        getPlotSummary(
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
