#' @title top_cols
#' @description Find the column numbers corresponding the largest values in a
#'   particular row of a matrix
#' @details To find top-n recommendations of a ratings matrix given an item (or
#'   a user).  Although k recommendations are expected to be returned, the
#'   function might sometimes return more or less than k recommendations.
#'
#'   \itemize{ \item Less: This happens when it is not possible to recommend k
#'   elements. For example, k is larger than the number of elements. \item More:
#'   This happens when a few elements have same rating. The function returns the
#'   index corresponding to all the elements which have the same rating. If
#'   ratings were \code{3,2,2,2,3}: \itemize{ \item k = 3: returns
#'   \code{1, 5, 2, 3, 4} \item k = 2: returns \code{1, 5} \item k = 1: returns
#'   \code{1, 5} }}
#'
#' @param mat (Sparse matrix of class 'dgCmatrix' or a integer/numeric matrix or
#'   'big.matrix') Rating matrix.
#' @param row (positive integer) Row number in which top columns are to be
#'   selected.
#' @param k (positive integer) Number of column numbers to be recommended. This
#'   might not be strictly adhered to, see Details.
#' @param ignore (integer vector) Column numbers to be ignored.
#'
#' @examples
#' \dontrun{
#' temp <- slim(mat           = ft_implicit # input sparse ratings matrix
#'              , alpha       = 0.5         # 0 for ridge, 1 for lasso
#'              #, lambda                   # suggested not to set lambda
#'              #, nlambda                  # using default nlambda = 100
#'              , nonNegCoeff = TRUE        # better accuracy, lower interpretability
#'              , directory   = td          # dir where output matrices are stored
#'              , coeffMat    = TRUE        # helpful in 'predict'ing later
#'              , returnMat   = TRUE        # return matrices in memory
#'              , computeRMSE = TRUE        # RMSE over rated items
#'              , nproc       = 2L          # number of concurrent processes
#'              , progress    = TRUE        # show a progressbar
#'              , check       = TRUE        # do basic checks on input params
#'              , cleanup     = FALSE       # keep output matrices on disk
#'              )
#' str(temp)
#' # output ratings matrix would be comparatively denser
#' predMat <- temp[["ratingMat"]] != 0
#' sum(predMat)/((dim(predMat)[1])*(dim(predMat)[2]))
#' # recommend top 5 items for a user 10
#' top_cols(temp[["ratingMat"]]
#'          , row = 10
#'          , k   = 5
#'          )
#' # if you intend to avoid recommending 10, 215 and 3
#' top_cols(temp[["ratingMat"]]
#'          , row = 10
#'          , k   = 5
#'          , ignore = c(10, 215, 3)
#'          )
#' }
#'
#' @export
#'
top_cols <- function(mat
                     , row
                     , k = 5
                     , ignore){
  # assertions         ----
  if(!(inherits(mat, "dgCMatrix") ||
       inherits(mat, "matrix") ||
       inherits(mat, "big.matrix"))){
    stop("'mat' should be a sparse matrix of class 'dgCMatrix' OR a numeric/integer matrix OR an object of class 'big.matrix'")
  }

  if(!inherits(mat, "big.matrix")){
    if(anyNA(mat)){
      stop("'mat' should not contain NA")
    }
  }

  n <- ncol(mat)
  m <- nrow(mat)

  if(!(is.count(row) && row <= m)){
    stop("'row' should be a positive integer not greater than the number of rows")
  }

  if(!(is.count(k))){
    stop("'k' should be a positive integer")
  }
  if(!missing(ignore)){
    if(!(all(ignore == floor(ignore))) ||
       !(max(ignore) <= n) ||
       !(min(ignore) > 0)
    ){
      stop("'ignore' should be integer vector corresponding to the column numbers to be ignored")
    }
    ignore <- unique(ignore)
    if(length(ignore) == n){
      return(integer(0))
    }
  }

  # compute top_k cols ----
  theRow        <- as.list(mat[row, ])
  names(theRow) <- 1:n
  if(!missing(ignore)){
    theRow      <- theRow[-ignore]
  }
  k         <- min(k, length(theRow))
  tops      <- sort(unlist(theRow), decreasing = TRUE)[1:k]
  top_k_min <- min(tops)
  top_k_max <- max(tops)
  top_index <- names(theRow)[theRow >= top_k_min & theRow <= top_k_max]
  top_k     <- top_index[order(unlist(theRow[top_index]), decreasing = TRUE)]
  return(as.integer(top_k))
}