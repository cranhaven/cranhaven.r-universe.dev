#' @rdname treesize.icrf
#' @name treesize.icrf
#' @export treesize
#' @useDynLib icrf
#'
#' @title Size of trees in an \code{icrf} ensemble
#'
#' @description 'Size of trees (number of nodes)' in the returned forest of \code{icrf}.
#' The returned forest depends on the \code{returnBest} argument of the \code{icrf} function;
#' It is either the last forest, when \code{returnBest = FALSE} or
#' the the best forest, when \code{returnBest = TRUE}.
#' (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#'
#' @param x an object of class \code{icrf}, 'which contains a forest component.'
#' @param terminal 'count terminal nodes only (\code{TRUE}) or all nodes (\code{FALSE})'
#' @param ... 'not used.'
#' @return 'A vector containing number of nodes for the trees' in the icrf object.
#'
#' @note The \code{icrf} 'object must contain the forest component; i.e.,
#' created with' \code{icrf(..., keep.forest=TRUE)}.
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
#'  treesize(rats.icrf)
#' }
#' \dontshow{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=2, nfold=2)
#'  treesize(rats.icrf)
#' }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
treesize <- function(x, ...){
  UseMethod("treesize")
}

#' @rdname treesize.icrf
#' @export
treesize.icrf <- function(x, terminal=TRUE, ...) {
  if(!inherits(x, "icrf"))
    stop("This function works for objects of class `icrf'")
  if(is.null(x$forest)) stop("The object must contain the forest component")
  if(terminal) return((x$forest$ndbigtree+1)/2) else return(x$forest$ndbigtree)
}
