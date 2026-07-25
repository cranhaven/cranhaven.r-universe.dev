#' This function draws partial residual plots for a continuous explanatory variables in a given model.
#'
#' @title Partial residual plot of continuous variable
#'
#' @param fit an \code{lm}, \code{glm} or \code{svyglm} object.
#' @param varname character, the name of an explanatory variable in the model
#' @param showBootstraps logical, if \code{TRUE}, bootstrap smoothers will overlay the graph. By default this is \code{TRUE} if there are between 30 and 4000 observations in the model, otherwise it is \code{FALSE}.
#' @param use.inzightplots logical, if \code{TRUE}, the iNZightPlots package will be used for plotting.
#' @param env environment where the data is stored for bootstrapping
#' @return No return value, called for side-effect of producing a plot.
#'
#' @author David Banks, Tom Elliott.
#'
#' @export
#'
#' @examples
#' m <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
#' partialResPlot(m, "Sepal.Width")
#'
#' \donttest{
#' allPartialResPlots(lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris))
#' }
partialResPlot <-
    function(fit, varname,
             showBootstraps = nrow(fit$model) >= 30 & nrow(fit$model) < 4000,
             use.inzightplots = FALSE, env = parent.frame()) {

  # if iNZightPlots is available, use it for plotting:
    inzplots <- use.inzightplots && requireNamespace("iNZightPlots", TRUE)


    xVarterms = attr(fit$terms, "term.labels")
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]
    if (! varname %in% xVarnames)
        stop(paste(varname, "is not an explanatory variable in the given model"))
    xVartypes = attr(fit$terms, "dataClasses")
    varType = xVartypes[varname]
    if (varType %in% c("factor", "ordered"))
        stop(paste(varname, "is a factor variable"))

    yname = dimnames(attr(fit$terms, "factors"))[[1]][1]
    r = fit$residuals
    Bi = fit$coefficients[varname]
    Xi = fit$model[, varname]
    Yi = r + Bi*Xi

    ylab <- "Y - estimated effects of other variables"
    main <- paste("Partial residual plot for", varname)

    ## Plot the data points
    if (inzplots) {
        iNZightPlots::iNZightPlot(Xi, Yi, xlab = varname, ylab = ylab,
                    main = main, cex.pt = 0.6)
        ovp <- current.viewport()
        pushViewport(viewport(xscale = ovp$xscale, yscale = ovp$yscale,
                              clip = "on"))
    } else {
        plot(Xi, Yi, xlab = varname, ylab = ylab, main = main)
    }


    if (showBootstraps) {
        ## Plot bootstrap smooths
        bsm = try(bootstrapModels(fit, env = env), TRUE)
        if (inherits(bsm, "try-error")) {
            showBootstraps <- FALSE
            warning("Could not generate bootstraps.")
        } else {
            for (j in seq_along(bsm)) {
                bsm_r = bsm[[j]]$residuals
                bsm_Bi = bsm[[j]]$coefficients[varname]
                bsm_Xi = bsm[[j]]$model[, varname]

                if (inzplots) {
                    iNZightPlots:::addQuantileSmoother(
                        bsm_Xi, bsm_r + bsm_Bi * bsm_Xi, quantile = 0.5,
                        col = "lightgreen", lty = 1, lwd = 1)
                } else {
                    bsm_sm = loess(bsm_r + bsm_Bi * bsm_Xi ~ bsm_Xi)
                    bsm_smOrd = order(bsm_sm$x)
                    bsm_smx = bsm_sm$x[bsm_smOrd]
                    bsm_smy = bsm_sm$fitted[bsm_smOrd]
                    lines(bsm_smx, bsm_smy, col = "lightgreen")
                }
            }
        }
    }

    ## Plot linear trend we are modelling
    xlims = range(Xi)
    if (inzplots) {
        grid.lines(xlims, Bi * xlims, default.units = "native",
                   gp = gpar(lty = 2, col = "blue", lwd = 2))
    } else {
        lines(xlims, Bi * xlims, lty = "dashed", col = "blue", lwd = 2)
    }

    ## Plot original data smooth


    if (inzplots) {
        iNZightPlots:::addQuantileSmoother(Xi, Yi, quantile = 0.5,
                                           col = "orangered", lty = 1,
                                           lwd = 2)
    } else {
        sm = loess(Yi ~ Xi)
        smOrd = order(sm$x)
        smx = sm$x[smOrd]
        smy = sm$fitted[smOrd]
        lines(smx, smy, col= "orangered", lwd = 2)
    }
}

#' @describeIn partialResPlot Cycle through all partial residual plots
#' @param ... additional arguments passed to `partialResPlot`
#' @export
allPartialResPlots <-
    function(fit, ...) {
        promptSetting = grDevices::devAskNewPage(TRUE)
        on.exit(grDevices::devAskNewPage(promptSetting))
        xVarterms = attr(fit$terms, "term.labels")
        xVarnames = xVarterms[ ! grepl(":", xVarterms)]
        xVartypes = attr(fit$terms, "dataClasses")
        for (v in xVarnames)
            if (! xVartypes[v] %in% c("factor", "ordered"))
                partialResPlot(fit, v, ...)
}
