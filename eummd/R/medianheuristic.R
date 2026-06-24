#' Compute the median difference between pairs of values
#'
#' Compute the median of all differences between distinct 
#' pairs in vectors or matrices.
#'
#' @param X Numeric vector or matrix of length \code{n}.
#'
#' @param Y Numeric vector or matrix of length \code{m}, or \code{NULL}.
#'
#' @param kernel String, either \code{"Laplacian"} or \code{"Gaussian"}.
#'
#' @param fast Boolean; if \code{TRUE} will run \eqn{O(N \log N)} algorithm,
#'           where \code{N = n + m},
#'           but if \code{FALSE} (default)
#'           will run naive \eqn{O(N^2 \log N)} algorithm.
#'
#' @details The median difference is defined as follows:
#'
#'    \eqn{Z} is the combined \eqn{X} and \eqn{Y} values into a single vector 
#'    or matrix.
#'    Number of columns is the dimension, and these need to be equal 
#'    for \eqn{X} and \eqn{Y}. Then, if the Laplacian kernel is used,
#'
#'  \deqn{ m = \textnormal{median} \{ || x_i - x_j ||_1; \,\, i>j, \,\, 
#'         i=1, 2,\dots, n+m,\,\,\textnormal{ and } j=1, 2,\dots, i-1 \}, }
#'          
#'   where \eqn{ || z_i - z_j ||_1} is the 1-norm, and so if the data 
#'   are \code{d}-dimensional then
#'          
#'   \deqn{ || z_i - z_j ||_1 = \sum_{k=1}^{d} |z_{i,k} - z_{j,k}|. }
#'        
#'   If the Gaussian kernel is specified, then the square of the two-norm is 
#'   used.
#'        
#'   The median heuristic is defined as \code{beta = 1/m}.
#'          
#'   Naive method will compute all distinct pairs, of which there are 
#'   \eqn{N(N+1)/2} differences. These are then sorted using 
#'   a \eqn{O(N \log N)} algorithm, so overall \eqn{O(N^2 \log N)}. 
#'
#'   The fast method is \eqn{O(N \log N)} is from Croux and Rousseeuw (1992), 
#'   which is based on Johnson and Mizoguchi (1978). 
#'
#' @return A scalar, the median of all pairwise differences.
#'
#' @references
#'    Croux, C. and Rousseeuw, P. J. (1992), 
#'    "Time-Efficient Algorithms for Two Highly Robust Estimators of Scale"
#'    In Computational Statistics: Volume 1: Proceedings of the 10th 
#'    Symposium on Computational Statistics (pp. 411-428).
#'
#'    Johnson, D.B., and Mizoguchi, T. (1978), 
#'    "Selecting the Kth Element in X + Y and X_1 + X_2 + ... + X_m", 
#'    SIAM Journal of Computing, 7, 147-153.
#'
#' @seealso \code{medianheuristic}
#'
#' @examples 
#'
#' X <- c(7.1, 1.2, 4.3, 0.4)
#' Y <- c(5.5, 2.6, 8.7)
#' #using fast method, Laplacian kernel, loglinear in number of observations
#' md <- mediandiff(X, Y, fast=TRUE)
#'
#' #using fast method, Gaussian kernel, loglinear in number of observations
#' md <- mediandiff(X, Y, fast=TRUE, kernel="Gaussian")
#'
#' #using naive method (default), with Laplacian kernel
#' md <- mediandiff(X, Y)
#'
#' @export
mediandiff <- function(X, Y=NULL, kernel=c("Laplacian", "Gaussian"), fast=FALSE){

    # check vectors/matrices are numetic
    if ( !(is.numeric(X)) ){
        stop("X needs to be numeric.")
    }
    if ( !(is.null(Y)) ){
        if ( !(is.numeric(Y)) ){
            stop("Y needs to be numeric.")
        }
    }

    # check kernel is correct
    kernel <- kernel[1]
    if ( (kernel != "Laplacian") && (kernel != "Gaussian") ){
        stop("kernel needs to be either 'Laplacian' or 'Gaussian'.")
    }

    # initialising here; will be updated later
    Xvec <- c()
    nX <- 0
    dX <- 0

    # deal with matrix case first
    if ( is.matrix(X) ){
        # get dimensions of matrices
        nX <- dim(X)[1]
        dX <- dim(X)[2]

        if ( is.matrix(Y) ){
            # get dimensions of matrices
            nY <- dim(Y)[1]
            dY <- dim(Y)[2]

            # check dimensions compatible
            if (dX != dY){
                stop("Dimension (number of columns) of matrices need to be equal.")
            }

            # if compatible columns, rbind with X
            X <- rbind(X, Y)

        } else {
            if ( !(is.null(Y)) ){
                stop("Y must be either null or a matrix.")
            }
        } # end of is.matrix(Y)

        # update dimensions
        nX <- dim(X)[1]
        dX <- dim(X)[2]

        # flatten to vector
        Xvec <- as.vector(t(X))

    } else if ( is.vector(X) ){
        if (is.null(Y)){
            Xvec <- X
        } else if (is.vector(Y)){
            Xvec <- c(X, Y)
        } else {
            stop("Y must be either null or a vector.")
        }
        # if vector; specify dimensions
        nX <- length(Xvec)
        dX <- 1
    } else {
        stop("X needs to be a numeric vector or a matrix")
    }


    md <- 0
    if (fast){
        if (is.vector(X)){
            # cpp fast median heuristic
            md <- fast_median_diff_Rcpp(Xvec)
            if (kernel=="Gaussian"){
                md <- md^2
            }
            #md <- -1
        } else {
            stop("Can only compute fast version when X is vector.")
        }
    } else {
        # using naive method
        # if kernel is Laplacian
        kmethod <- 1
        if (kernel=="Gaussian"){
            # anything other than 1 will be Gaussian kernel
            kmethod <- 2
        }
        md <- naive_median_diff_Rcpp(Xvec, nX, dX, kmethod)
    }

    return(md)
}



#' Compute the median heuristic
#'
#' Computes the inverse of the median difference of 
#' all distinct pairs in vectors or matrices.
#'
#' @param X Numeric vector or matrix of length \code{n}.
#'
#' @param Y Numeric vector or matrix of length \code{m}, or \code{NULL}.
#'
#' @param kernel String, either \code{"Laplacian"} or \code{"Gaussian"}.
#'
#' @param fast Boolean; if \code{TRUE} will run \code{O(N log N)} algorithm,
#'           where \code{N = n + m},
#'           but if \code{FALSE} will run naive \code{O(N^2 log(N))} algorithm.
#'
#' @details Computes median of differences \code{md} using \code{mediandiff} 
#'          and then returns \code{1 / md}. See \code{mediandiff} for details.
#'
#' @return A scalar, the inverse of the median of all pairwise differences.
#'
#' @seealso \code{mediandiff}
#'
#' @examples 
#'
#' X <- c(7.1, 1.2, 4.3, 0.4)
#' Y <- c(5.5, 2.6, 8.7)
#' mh <- medianheuristic(X, Y, kernel="Laplacian", fast=TRUE)
#'
#' #using fast method, Gaussian kernel, loglinear in number of observations
#' mh <- medianheuristic(X, Y, fast=TRUE, kernel="Gaussian")
#'
#' #using naive method (default), with Laplacian kernel
#' mh <- medianheuristic(X, Y)
#'
#'
#' @export
medianheuristic <- function(X, Y=NULL, kernel=c("Laplacian", "Gaussian"), 
                            fast=FALSE){

    # compute median difference
    md <- mediandiff(X, Y, kernel, fast)
    return(1.0/md)
}
