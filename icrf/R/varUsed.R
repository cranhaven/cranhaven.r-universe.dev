#' @rdname varUsed.icrf
#' @name varUsed.icrf
#' @export varUsed
#'
#' @title 'Variables used in' an \code{icrf} ensemble
#'
#' @description 'Find out which predictor variables are actually used in' the returned forest of
#' the \code{icrf}. The returned forest depends on the \code{returnBest} argument of the
#' \code{icrf} function; It is either the last forest, when \code{returnBest = FALSE} or
#' the the best forest, when \code{returnBest = TRUE}. (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#'
#' @param x 'an object of class' \code{icrf}.
#' @param by.tree 'Should the list of variables used be broken down by trees in the forest?'
#' @param count 'Should the frequencies that variables appear in trees be returned?'
#' @param ... not used.
#' @return 'A vector containing number of nodes for the trees' in the icrf object.
#'
#' @return 'If \code{count=TRUE} and \code{by.tree=FALSE}, an integer vector containing
#' frequencies that variables are used in the forest. If \code{by.tree=TRUE}, a matrix is returned,
#' breaking down the counts by tree (each column corresponding to one tree and each row to a variable).'
#'
#' 'If \code{count=FALSE} and \code{by.tree=TRUE}, a list of integer indices is returned giving the
#' variables used in the trees, else if \code{by.tree=FALSE}, a vector of integer indices giving
#' the variables used in the entire forest.'
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
#'  varUsed(rats.icrf)
#' }
#' \dontshow{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=2, nfold=2)
#'  varUsed(rats.icrf)
#' }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
#' @useDynLib icrf
#'
varUsed <- function(x, ...) {
  UseMethod("varUsed")
}

#' @rdname varUsed.icrf
#' @export
varUsed.icrf <- function(x, by.tree=FALSE, count=TRUE, ...) {
    if (!inherits(x, "icrf"))
        stop(deparse(substitute(x)), "is not a icrf object")
    if (is.null(x$forest))
        stop(deparse(substitute(x)), "does not contain forest")

    p <- length(x$forest$ncat)  # Total number of variables.
    if (count) {
        if (by.tree) {
            v <- apply(x$forest$bestvar, 2, function(x) {
                xx <- numeric(p)
                y <- table(x[x>0])
                xx[as.numeric(names(y))] <- y
                xx
            })
        } else {
            v <- numeric(p)
            vv <- table(x$forest$bestvar[x$forest$bestvar > 0])
            v[as.numeric(names(vv))] <- vv
        }
    } else {
        v <- apply(x$forest$bestvar, 2, function(x) sort(unique(x[x>0])))
        if(!by.tree) v <- sort(unique(unlist(v)))
    }
    v
}
