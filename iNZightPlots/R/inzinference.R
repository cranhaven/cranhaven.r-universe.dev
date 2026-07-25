#' iNZight Inference Method
#'
#' A generic function used to generate inferential information for objects within the iNZight ecosystem.
#' @param x An object
#' @param ... additional arguments for methods
#' @param env an environment to evaluate things
#' @return The output depends on the type of input, and consists of a inference object with a `print` method.
#' @export
#' @md
inzinference <- function(x, ..., env = parent.frame()) {
    UseMethod("inzinference")
}

#' @export
inzinference.default <- function(x, ..., env = parent.frame()) {
    stop("That type of object is not supported.")
}

#' @describeIn inzinference Wrapper for getPlotSummary to obtain inference information about a plot
#' @export
#' @param data Dataset to plotq
#' @param design A survey design to use
#' @param type Type type of inference to obtain, one of 'conf' or 'comp'
#'             for confidence intervals and comparison intervals, respectively
#'             (currently ignored).
inzinference.formula <- function(x, data = NULL, design = NULL, type = c("conf", "comp"), ...,
                                 env = parent.frame()) {
    dots <- rlang::enexprs(...)
    fmla <- parse_formula(x)
    type <- match.arg(type)

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
            summary.type = "inference",
            inference.type = !!type,
            !!!dots,
            env = !!env
        )
    )

    if (is.null(fmla$y)) exp$y <- NULL
    if (is.null(fmla$g1)) exp$g1 <- NULL
    if (is.null(fmla$g2)) exp$g2 <- NULL

    eval(exp, envir = env)
}
