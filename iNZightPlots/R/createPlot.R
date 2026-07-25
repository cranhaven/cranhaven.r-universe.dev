createPlot <- function(df, opts, xattr) {
    ## This function takes a data.frame object and creates the necessary object which will have a
    ## `plot` method.

    if (is.null(df)) {
        return(nullPlot(opts, xattr))
    }

    large <- ifelse(
        is.null(lg <- opts$largesample),
        (if (is_survey(df)) nrow(df$variables) else nrow(df)) > opts$large.sample.size,
        lg
    )
    wts <- xattr$class != "inz.simple"

    v <- xattr$v
    vt <- xattr$vartypes
    xfact <- vt$x == "factor"
    ynull <- !"y" %in% v
    yfact <- if (ynull) FALSE else vt$y == "factor"
    xnum <- !xfact
    ynum <- if (ynull) FALSE else !yfact

    ## allow forcing the plot type:
    ## -- here, the switch takes the given plot type, checks the data types are correct,
    ## and if they aren't, just uses the default
    plottype <- gsub("plot", "", opts$plottype) # remove `plot` from type, if specified
    type <- switch(plottype,
        "bar" = ifelse(xfact & (ynull | yfact), plottype, "default"),
        "hist" = ,
        "dot" = ifelse((xnum & !ynum) | (!xnum & ynum), plottype, "default"),
        "scatter" = ,
        "grid" = ,
        "hex" = ifelse(xnum & ynum, plottype, "default"),
        "other"
    )

    ## throw a warning if they give an invalid type
    if (type != plottype & type != "other") {
        warning("The plot type specified does not match the supplied data.")
    }

    if (type %in% c("default", "other")) {
        if (ynull) {
            newtype <- ifelse(xfact,
                "barplot",
                ifelse(large | wts, "histplot", "dotplot")
            )
        } else {
            newtype <- ifelse(xfact,
                ifelse(yfact,
                    "barplot",
                    ifelse(large | wts, "histplot", "dotplot")
                ),
                ifelse(yfact,
                    ifelse(large, "histplot", "dotplot"),
                    ifelse(large,
                        "hexplot", ## ifelse(wts, "hexplot", "gridplot"),
                        "scatterplot"
                    )
                )
            )
        }

        type <- if (type == "other") {
            c(paste0(plottype, "plot"), newtype)
        } else {
            newtype
        }
    } else {
        type <- paste0(type, "plot")
    }

    # Here, we create a class for the object to be plotted, then we use a generic function `create`
    # which will use the correct method, and create the required plot.

    pclass <- paste("inz", type, sep = ".")
    obj <- structure(
        .Data = list(df = df, opts = opts, xattr = xattr),
        class = pclass
    )

    tryCatch(create(obj),
        error = function(e) {
            if (grepl("no applicable method for 'create'", e)) {
                stop(paste0('No method available for plottype = "', plottype, '"'))
            } else {
                stop(e)
            }
        }
    )
}


#' Create a Plot Object
#'
#' This create method is to be used by packages extending 'iNZightPlots',
#' and should not be used by users. The resulting object should have
#' an associated \code{plot} method.
#'
#' @title Create plots for iNZight
#' @param obj an object
#' @param ... additional arguments
#' @return an iNZight plot object with class determined by data type
#' @author Tom Elliott
#' @export
create <- function(obj, ...) {
    UseMethod("create")
}
