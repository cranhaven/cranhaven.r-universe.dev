#' Naive computation for Maximum Mean Discrepancy
#'
#' Computes maximum mean discrepancy statistics with Laplacian 
#' or Gaussian kernel. 
#' Suitable for multivariate data. Naive approach, quadratic in number
#' of observations.
#' 
#' @param X Matrix (or vector) of observations in first sample.
#' 
#' @param Y Matrix (or vector) of observations in second sample.
#' 
#' @param beta kernel parameter. Must be positive; if not, computes
#'             median heuristic in quadratic time. Default value
#'             is \code{-0.1}, which will force median heuristic to be used.
#' 
#' @param pval Boolean for whether to compute p-value or not. 
#' 
#' @param kernel String, either \code{"Laplacian"} or \code{"Gaussian"}. 
#'               Default is \code{"Laplacian"}.
#'
#' @param numperm Number of permutations. Default is \code{200}.
#'
#' @param seednum Seed number for generating permutations. Default is \code{0}, 
#'                which means seed is set randomly. For values larger than 
#'                \code{0}, results will be reproducible.
#' 
#' @param alternative A character string specifying the alternative hypothesis,
#'                    which must be either \code{"greater"} (default) or
#'                    \code{"two.sided"}. In Gretton et al., the 
#'                    MMD test statistic is specified so that if it is 
#'                    significantly larger than zero, then the null hypothesis
#'                    that the two samples come from the same distribution 
#'                    should be rejected. For this reason, \code{"greater"}
#'                    is recommended. The test will still work 
#'                    in many cases with \code{"two.sided"} specified, but this
#'                    could lead to problems in certain cases.
#'
#' @param allowzeropval A boolean, specifying whether we will allow zero 
#'                      p-values or not. Default is \code{FALSE}; then 
#'                      a threshold of \code{0.5 / (numperm+1)} is used, 
#'                      and if the computed p-value is less than this
#'                      threshold, it is then set to be this value.
#'                      this avoids the possibility of zero p-values.
#'
#' @details First checks number of columns (dimension) are equal. 
#'          Suppose matrix \eqn{X} has \eqn{n} rows and \eqn{d} columns, 
#'          and matrix \eqn{Y} has \eqn{m} rows; checks that \eqn{Y} 
#'          has \eqn{d} columns (if not, then throws error). 
#'          Then flattens matrices to vectors (or, if \eqn{d=1}, they are
#'          already vectors.
#'          Then calls C++ method. If the first sample has \eqn{n} 
#'          \eqn{d}-dimensional samples and the second sample has 
#'          \eqn{m} \eqn{d}-dimensional samples, then the algorithm
#'          computes the statistic in \eqn{O((n+m)^2)} time.
#'          
#'  Median difference is as follows:
#'          
#'  \deqn{ m = \textnormal{median} \{ || x_i - x_j ||_1; \,\, i>j, \,\, 
#'         i=1, 2,\dots, n+m,\,\,\textnormal{ and } j=1, 2,\dots, i-1 \}, }
#'    
#'          
#'  where \eqn{ || x_i - x_j ||_1} is the 1-norm, and so if the data 
#'  are \eqn{d}-dimensional then
#'          
#'  \deqn{ || x_i - x_j ||_1 = \sum_{k=1}^{d} |x_{i,k} - x_{j,k}|, }
#'          
#'  and finally median heuristic is \code{beta = 1/m}.
#'  This can be computed in \eqn{O( (n+m)^2 )} time.
#'          
#'  The Laplacian kernel \eqn{k} is defined as 
#'         
#'  \deqn{ k(x,y) = \exp( -\beta || x_i - x_j ||_1 ). }
#'
#'  Random seed is set for \code{std::mt19937} and \code{std::shuffle} in C++.
#'
#' @return A list with the following elements:
#'         \describe{
#'             \item{\code{pval}}{The p-value of the test, if it is  
#'                                computed (\code{pval=TRUE}). }
#'             \item{\code{stat}}{The statistic of the test, which
#'                                is always computed. }
#'             \item{\code{beta}}{The kernel parameter used in the test.
#'                                If \code{beta} was not initialised or
#'                                negative, this will be the median heuristic
#'                                value.}
#'          }
#'
#' @references
#'    Gretton, A., Borgwardt, K. M., Rasch M. J., Schölkopf, B. and Smola, A. 
#'    (2012) "A kernel two-sample test." The Journal of Machine Learning Research 
#'     13, no. 1, 723-773.
#'
#' @examples
#'
#' X <- matrix(c(1:12), ncol=2, byrow=TRUE)
#' Y <- matrix(c(13:20), ncol=2, byrow=TRUE)
#' mmdList <- mmd(X=X, Y=Y, beta=0.1, pval=FALSE)
#'
#' #using median heuristic
#' mmdList <- mmd(X=X, Y=Y, pval=FALSE)
#'
#' #using median heuristic and computing p-value
#' mmdList <- mmd(X=X, Y=Y)
#' \donttest{
#' #using median heuristic and computing p-value
#' #using 1000 permutations and seed 1 for reproducibility.
#' mmdList <- mmd(X=X, Y=Y, numperm=1000, seednum=1)
#' }
#'
#' @export
mmd <- function(X, Y, beta=-0.1, pval=TRUE, kernel=c("Laplacian", "Gaussian"), 
                numperm=200, seednum=0, alternative=c("greater", "two.sided"),
                allowzeropval=FALSE){

    # check vectors/matrices are numeric
    if ( !(is.numeric(X)) ){
        stop("X needs to be numeric.")
    }
    if ( !(is.numeric(Y)) ){
        stop("Y needs to be numeric.")
    }

    # check kernel is correct
    kernel <- kernel[1]
    if ( (kernel != "Laplacian") && (kernel != "Gaussian") ){
        stop("kernel needs to be either 'Laplacian' or 'Gaussian'.")
    }

    # if alternative is 'greater', default
    alternative <- alternative[1]
    if ( (alternative != "greater") && (alternative != "two.sided") ){
        stop("alternative needs to be either 'greater' or 'two.sided'.")
    }

    # two sided is false by default; false is 0
    twosided <- 0
    if (alternative == "two.sided")
        twosided <- 1

    # boundminpval is true by default; true is 1, false is 0
    boundminpval <- 1
    if (allowzeropval==TRUE)
        boundminpval <- 0

    # initialise, will update later
    nX <- 0
    dX <- 0
    nY <- 0
    dY <- 0
    Xvec <- c()
    Yvec <- c()

    # add checks for matrix/vectors
    if (is.matrix(X)){
        # get dimensions of matrices
        nX <- dim(X)[1]
        dX <- dim(X)[2]

        if (!(is.matrix(Y))){
            stop("If X is a matrix, Y must also be a matrix.")
        }

        nY <- dim(Y)[1]
        dY <- dim(Y)[2]

        # check dimensions compatible
        if (dX != dY){
            stop("Dimension (number of columns) of matrices need to be equal.")
        }

        # flatten to vectors; we use transpose
        # > X
        #      [,1] [,2] [,3]
        # [1,]    1    3    5
        # [2,]    2    4    6
        #
        # will be flattened to 
        # [1] 1 3 5 2 4 6
        # which is what we want

        Xvec <- as.vector(t(X))
        Yvec <- as.vector(t(Y))
    } else if (is.vector(X)){
        if (!(is.vector(Y))){
            stop("If X is a vector, Y must also be a vector.")
        }
        nX <- length(X)
        dX <- 1
        nY <- length(Y)
        dY <- 1
        Xvec <- X
        Yvec <- Y
    } else {
        stop("X must be a vector or a matrix.")
    }



    # if beta not positive, compute median heuristic (also from C++)
    # finally, compute MMD 
    mmdList <- list()
    if (kernel=="Gaussian"){
        if (pval){
            mmdList <- mmd_gau_pval_Rcpp(Xvec, Yvec, nX, dX, nY, dY, 
                                         numperm, seednum, beta, twosided, 
                                         boundminpval)
        } else {
            mmdList <- mmd_gau_Rcpp(Xvec, Yvec, nX, dX, nY, dY, beta)
        }
    } else {
        if (pval){
            mmdList <- mmd_lap_pval_Rcpp(Xvec, Yvec, nX, dX, nY, dY, 
                                         numperm, seednum, beta, twosided, 
                                         boundminpval)
        } else {
            mmdList <- mmd_lap_Rcpp(Xvec, Yvec, nX, dX, nY, dY, beta)
        }
    }

    # check pval; if no pval, functions return -1, then changes to NA
    if (mmdList$pval < 0){
        mmdList$pval <- NA
    }
    return(mmdList)
}

