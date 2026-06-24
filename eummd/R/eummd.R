#' euMMD: Efficient Univariate Maximum Mean Discrepancy
#'
#' Computes the maximum mean discrepancy statistic with the Laplacian kernel. 
#' Suitable only for univariate data. Computing the statistic alone
#' for \eqn{n} observations is \eqn{O(n \log n)}, and computing the 
#' p-value for \eqn{L} permutations is \eqn{O(n \log n + Ln)}.
#' 
#' @param x Univariate vector of observations in first sample.
#' 
#' @param y Univariate vector of observations in second sample.
#' 
#' @param beta kernel parameter. Must be positive; if not, computes
#'             median heuristic in quadratic time. Default value
#'             is \code{-0.1}, which will force median heuristic to be used.
#' 
#' @param pval Boolean for whether to compute p-value or not. 
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
#' @details If the total number of observations in both samples is \code{n}, 
#'          first sort combined sample in \eqn{O(n \log n)} before remaining
#'          steps are linear in \code{n}.
#' 
#'    If \code{beta} is not a positive value, 
#'    median difference is computed as follows:
#'          
#'    \deqn{ m = \textnormal{median} \{ || x_i - x_j ||_1; \,\, i>j, \,\, 
#'           i=1, 2,\dots, n+m,\,\,\textnormal{ and } j=1, 2,\dots, i-1 \}, }
#'    
#'    where \eqn{ || x_i - x_j ||_1} is the 1-norm, and 
#'    so if the data are univariate then
#'    
#'    \deqn{ || x_i - x_j ||_1 = |x_{i} - x_{j}|. }
#'    
#'    and finally median heuristic is \code{beta = 1/m}.
#'    This can be computed in \eqn{O(n \log n )} time
#'    using the algorithms of Johnson and Mizoguchi (1978) 
#'    and Croux and Rousseuw (1992); see \code{mediandiff}
#'    for references.
#'    
#'    The Laplacian kernel \eqn{k} is defined as 
#'    
#'    \deqn{ k(x,y) = \exp [ -\beta ||x - y||_1  ]. }
#'
#'    The random seed is set for \code{std::mt19937} and \code{std::shuffle} 
#'    in C++.
#'
#'
#'
#' @return A list with the following elements:
#'         \describe{
#'             \item{\code{pval}}{The p-value of the test, if it is  
#'                                computed (\code{pval=TRUE}). Otherwise, 
#'                                it is set to \code{NA}.}
#'             \item{\code{stat}}{The statistic of the test, which
#'                                is always computed. }
#'             \item{\code{beta}}{The kernel parameter used in the test.
#'                                If \code{beta} was not initialised or
#'                                negative, this will be the median heuristic
#'                                value.}
#'          }
#' 
#' @references
#'    Bodenham, D. A., and Kawahara, Y. (2023)
#'    "euMMD: efficiently computing the MMD two-sample test statistic for 
#'    univariate data." Statistics and Computing 33.5 (2023): 110.
#' 
#'    Croux, C. and Rousseeuw, P. J. (1992), 
#'    "Time-Efficient Algorithms for Two Highly Robust Estimators of Scale"
#'    In Computational Statistics: Volume 1: Proceedings of the 10th 
#'    Symposium on Computational Statistics (pp. 411-428).
#'
#'    Johnson, D.B., and Mizoguchi, T. (1978), 
#'    "Selecting the Kth Element in X + Y and X_1 + X_2 + ... + X_m", 
#'    SIAM Journal of Computing, 7, 147-153.
#' 
#' @seealso \code{mediandiff}
#'
#' @examples 
#' x <- c(7.1, 1.2, 4.3, 0.4)
#' y <- c(5.5, 2.6, 8.7)
#' #setting the kernel parameter to be 0.1; setting seed=1 for reproducibility
#' mmd_list <- eummd(x, y, beta=0.1, seednum=1)
#'
#' #now using median heuristic (default)
#' mmd_list <- eummd(x, y, seednum=1)
#'
#' #now not computing the p-value, only the statistic
#' mmd_list <- eummd(x, y, pval=FALSE, seednum=1)
#' \donttest{
#' #now using a larger number of permutations 
#' mmd_list <- eummd(x, y, numperm=1000, seednum=1)
#' }
#'
#'
#' @export
eummd <- function(x, y, beta=-0.1, pval=TRUE, numperm=200, seednum=0, 
                  alternative=c("greater", "two.sided"), allowzeropval=FALSE){

    # check vectors are numeric
    if ( !(is.numeric(x)) || !(is.vector(x)) ){
        stop("x needs to be a numeric vector.")
    }
    if ( !(is.numeric(y)) || !(is.vector(y))){
        stop("y needs to be a numeric vector.")
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


    mmdList <- list()
    if (pval){
        mmdList <- eummd_pval_Rcpp(x, y, beta, numperm, seednum, 
                                   twosided, boundminpval)
    } else {
        mmdList <- eummd_Rcpp(x, y, beta)
    }

    # check pval; if no pval, functions return -1, then changes to NA
    if (mmdList$pval < 0){
        mmdList$pval <- NA
    }
    return(mmdList)
}

