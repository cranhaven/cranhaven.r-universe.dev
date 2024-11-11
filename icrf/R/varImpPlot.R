#' @rdname varImpPlot.icrf
#' @name varImpPlot.icrf
#' @export varImpPlot
#' @useDynLib icrf
#'
#' @title 'Variable Importance Plot'
#'
#' @description 'Dotchart of variable importance as measured by' \code{icrf}.
#' (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#'
#' @param x 'an object of class' \code{icrf}
#' @param sort 'Should the variables be sorted in decreasing order of importance?'
#' @param n.var 'How many variables to show? (Ignored if sort=FALSE.)'
#' @param type 'arguments to be passed on to importance'
#' @param forest The forest for which the importance is plotted. If \code{NULL},
#' the best forest is plotted.
#' @param main 'plot title'
#' @param ... 'Other graphical parameters to be passed on to \code{dotchart}.'
#'
#' @return 'Invisibly, the importance of the variables that were plotted.'
#'
#'
#' @examples
#' # rats data example.
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' data(rat2)
#' \donttest{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=10, nfold=3)
#'  varImpPlot(rats.icrf)
#' }
#' \dontshow{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=2, nfold=2)
#'  varImpPlot(rats.icrf)
#' }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
varImpPlot <- function(x, ...) {
  UseMethod('varImpPlot')
}

#' @rdname varImpPlot.icrf
#' @export
varImpPlot.icrf <- function(x, sort=TRUE,
                            n.var=min(30, nrow(x$importance)),
                            type=NULL, forest=NULL,
                            main=deparse(substitute(x)), ...) {
    if (!inherits(x, "icrf"))
        stop("This function only works for objects of class `icrf'")
    imp <- importance(x, type=type, ...)
    ## If there are more than two columns, just use the first and the last columns.
    if (dim(imp)[3] > 2) imp <- imp[,, c(1, 3)]
    nmeas <- dim(imp)[3]
    if (is.null(forest)) {
      forest <- x$bestFold[1, "bestFold"]
    } else {
      if (!forest %in% 1:dim(imp)[2]) stop("Wrong forest number specified")
      if (length(forest) != 1) stop("Only one forest should be specified")
    }
    main <- paste0(main, "\nvariable importance of the ", forest, "th forest")

    if (nmeas > 1) {
        op <- par(mfrow=c(1, 2), mar=c(4, 5, 4, 1), mgp=c(2, .8, 0),
                  oma=c(0, 0, 2, 0), no.readonly=TRUE)
        on.exit(par(op))
    }
    for (i in 1:nmeas) {
        ord <- if (sort) rev(order(imp[, forest, i],
                                   decreasing=TRUE)[1:n.var]) else 1:n.var
        xmin <- if (dimnames(imp)[[3]][i] %in%
                    c("%IncIMSE1", "%IncIMSE2", "IncNodePurity")) 0 else min(imp[ord, forest, i])
        dotchart(imp[ord, forest, i], xlab=dimnames(imp)[[3]][i], ylab="",
                 main = if (nmeas == 1) main else NULL,
                 xlim = c(xmin, max(imp[, forest, i])), ...)
    }
    if (nmeas > 1) mtext(outer=TRUE, side=3, text=main, cex=1.2)
    invisible(imp)
}
