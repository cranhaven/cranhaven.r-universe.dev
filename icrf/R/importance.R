# scale not supported
#' @rdname importance.icrf
#' @name importance.icrf
#' @export importance
#' @useDynLib icrf
#'
#' @title 'Extract variable importance measure'
#'
#' @description 'This is the extractor function for variable importance measures as produced by' \code{icrf}.
#' (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#'
#' @param x 'an object of class' \code{icrf}
#' @param type either 1, 2, 3, or any combination of them, 'specifying the type of importance measure'
#' (1 = mean increase in IMSE1, 2 = mean increase in IMSE2, 3 = mean decrease in node impurity).
#' If not specified, all available types of importances are returned.
#' @param ... 'not used'
#'
#' @details
#' 'Here are the definitions of the variable importance measures. The first two measures are
#' computed from permuting OOB data: For each tree, the prediction error
#' on the out-of-bag portion of the data is recorded' (IMSE1 and IMSE2).
#' 'Then the same is done after permuting each predictor variable.'
#' 'The difference between the two are then averaged over all trees'
#' The normalization by the standard deviation of the differences is not supported in this version.
#' The third measure 'is the total decrease in node impurities from splitting on the variable,
#' averaged over all trees.'
#' 'For regression, it is measured by residual sum of squares.'
#' @return An array of importance measure matrices, one row for each predictor variable.' Each column
#' corresponds to the forest iteration. Each matrix corresponds to the type of the measure.
#'
#' @seealso \code{icrf}, \code{varImpPlot}
#'
#'
#' @examples
#' # rats data example.
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' data(rat2)
#' \donttest{
#' set.seed(1)
#' rats.icrf <-
#'   icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'        data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'        returnBest = TRUE, ntree=10, nfold=3)
#' importance(rats.icrf)
#' }
#' \dontshow{
#' set.seed(1)
#' rats.icrf <-
#'   icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'        data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'        returnBest = TRUE, ntree=2, nfold=2)
#' importance(rats.icrf)
#' }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
importance <- function(x, ...)  UseMethod("importance")

#' @rdname importance.icrf
#' @export
importance.default <- function(x, ...)
    stop("No method implemented for this class of object")

#' @rdname importance.icrf
#' @export
importance.icrf <- function(x, type=NULL, ...) {
    if (!inherits(x, "icrf"))
        stop("x is not of class icrf")
    hasImp <- !is.null(dim(x$importance)) || dim(x$importance)[3] == 1
    hasType <- !is.null(type)
    types <- c("%IncIMSE1", "%IncIMSE2", "IncNodePurity")
    existsType <- which(types %in% dimnames(x$importance)[[3]])
    if (hasType && any(!type %in% 1:3))
      stop("Wrong type specified")
    if (hasType && any(!type %in% existsType))
      stop("One of the measures has not been computed")
    allImp <- is.null(type) && hasImp

    imp <- x$importance
    #if (!hasType) type <- existsType
    if (hasType && hasImp) imp <- imp[,, types[type], drop=FALSE]

    imp
}
