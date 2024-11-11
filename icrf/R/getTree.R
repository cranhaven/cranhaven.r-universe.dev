#' @rdname getTree.icrf
#' @name getTree.icrf
#'
#' @title Extract a single tree from an icrf object
#'
#' @description \code{getTree} 'extracts the structure of a tree from an \code{icrf} object.'
#' Among \code{nfold} forests, the forest designated when implementing \code{icrf} will be
#' considered. i.e., the \code{k}th tree of the last forest, when \code{returnBest = FALSE} or
#' the tree of the best forest, when \code{returnBest = TRUE}, will be extracted.
#' (Quoted statements are from \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#' @param x an \code{icrf} object.
#' @param k 'which tree to extract?'
#' @param labelVar Splitting variables will be labelled with the original names
#' when \code{labelVar = TRUE}. Otherwise they will be expressed as integers.
#' @param ... not used.
#'
#' @details 'For numerical predictors, data with values of the variable less than or equal
#' to the splitting point go to the left daughter node.'
#'
#' 'For categorical predictors, the splitting point is represented by an integer,
#' whose binary expansion gives the identities of the categories that goes to left or right.
#' For example, if a predictor has four categories, and the split point is 13.
#' The binary expansion of 13 is (1, 0, 1, 1) (because 13 = 1*2^0 + 0*2^1 + 1*2^2 + 1*2^3),
#' so cases with categories 1, 3, or 4 in this predictor get sent to the left,
#' and the rest to the right.'
#'
#' @return 'A matrix (or data frame, if labelVar=TRUE) with' (5 + number of time points) 'columns and
#' number of rows equal to total number of nodes in the tree. The columns are:'
#'  \itemize{
#'   \item{left daughter:}{ 'the row where the left daughter node is; 0 if the node is terminal'}
#'   \item{right daughter:}{ 'the row where the right daughter node is; 0 if the node is terminal'}
#'   \item{split var:}{ 'which variable was used to split the node; 0 if the node is terminal'}
#'   \item{split point:}{ 'where the best split is; see Details for categorical predictor'}
#'   \item{status:}{ 'is the node terminal' (-1) or not (-3)}
#'   \item{from the 6th to the last columns:}{ the survival probability prediction for the node;
#'   each column represents the distinct time points.
#'   '0 if the node is not terminal'}
#'  }
#'
#' @examples
#' library(survival) # for Surv()
#' data(rat2)
#' L = ifelse(rat2$tumor, 0, rat2$survtime)
#' R = ifelse(rat2$tumor, rat2$survtime, Inf)
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' \donttest{
#' set.seed(1)
#' rats.icrf <-
#'   icrf(Surv(L, R, type = "interval2") ~ dose.lvl + weight + male + cage.no,
#'        data = rat2, ntree = 10, nfold = 3)
#' getTree(rats.icrf, k = 2)
#' }
#' \dontshow{
#' set.seed(2)
#' rats.icrf <-
#'   icrf(Surv(L, R, type = "interval2") ~ dose.lvl + weight + male + cage.no,
#'        data = rat2, ntree = 2, nfold = 2)
#' getTree(rats.icrf, k = 2)
#' }
#'
#'
#' @export getTree
#' @useDynLib icrf
getTree <- function(x, ...) {
  UseMethod('getTree')
}

#' @rdname getTree.icrf
#' @export
getTree.icrf <- function(x, k=1, labelVar=FALSE, ...) {
  if (!inherits(x, "icrf"))
    stop("object not of class icrf")
  if (is.null(x$forest)) {
    stop("No forest component in ", deparse(substitute(x)))
  }
  if (k > x$ntree) {
    stop("There are fewer than ", k, "trees in the forest")
  }

  tree <- cbind(x$forest$leftDaughter[,k],
                x$forest$rightDaughter[,k],
                x$forest$bestvar[,k],
                x$forest$xbestsplit[,k],
                x$forest$nodestatus[,k],
                t(x$forest$nodepred[,,k]))[1:x$forest$ndbigtree[k],]

  dimnames(tree) <- list(1:nrow(tree), c("left daughter", "right daughter",
                                         "split var", "split point",
                                         "status", x$time.points))

  if (labelVar) {
      tree <- as.data.frame(tree)
      v <- tree[[3]]
      v[v == 0] <- NA
      tree[[3]] <- factor(rownames(x$importance)[v])
  }
  tree
}

