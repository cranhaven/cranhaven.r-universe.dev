#' MEA-MMD: Multivariate Efficient Approximate Maximum Mean Discrepancy
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
#'             median heuristic in quadratic time for each projection. 
#'             Default value
#'             is \code{-0.1}, which will force median heuristic to be used.
#' 
#' @param pval Boolean for whether to compute p-value or not. 
#' 
#' @param type The type of projection used. Either \code{"proj"} for 
#'             random projections (default) or \code{"dist"} for interpoint
#'             distances.
#' 
#' @param numproj Number of projections (only used if \code{type="proj"}).
#'                Default is \code{20}.
#' 
#' @param nmethod Norm used for interpoint distances, if \code{type="dist"}.
#'                Needs to be either \code{2} (for two-norm, default) or 
#'                \code{1} (for one-norm).
#' 
#' @param distpval The p-value combination procedure if \code{type="dist"}.
#'                 Options are \code{"Hommel"} (default) or \code{"Fisher"}.
#'                 The Hommel method is preferred since the Type I error does 
#'                 not seem to be controlled if the Fisher method is used.
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
#' @param faster A boolean, specifying if to use faster algorithm
#'               when computing p-value. Default is \code{TRUE}.
#'
#'
#' @return A list with the following elements:
#'         \describe{
#'             \item{\code{pval}}{The p-value of the test, if it is  
#'                                computed (\code{pval=TRUE}). Otherwise, 
#'                                it is set to \code{NA}.}
#'             \item{\code{stat}}{The statistic of the test, which
#'                                is only returned when \code{type="proj"},
#'                                otherwise it is set to \code{NA}.}
#'          }
#'
#' @references
#'    Bodenham, D. A., and Kawahara, Y. (2023)
#'    "euMMD: efficiently computing the MMD two-sample test statistic for 
#'    univariate data." Statistics and Computing 33.5 (2023): 110.
#' 
#' @examples
#' X <- matrix(c(1:12), ncol=2, byrow=TRUE)
#' Y <- matrix(c(13:20), ncol=2, byrow=TRUE)
#' # using the random projections method
#' mmdList <- meammd(X=X, Y=Y, pval=TRUE, type="proj", numproj=50)
#'
#' # using the method were distances are computed to the various points 
#' mmdList <- meammd(X=X, Y=Y, pval=TRUE, type="dist")
#'
#'
#' @export 
meammd <- function(X, Y, beta=-0.1, pval=TRUE, 
                   type=c("proj", "dist"), numproj=20, nmethod=c(2, 1),
                   distpval=c("Hommel", "Fisher"), numperm=200, seednum=0, 
                   alternative=c("greater", "two.sided"), allowzeropval=FALSE, 
                   faster=TRUE){

    # check vectors/matrices are numeric
    if ( !(is.numeric(X)) && !(is.matrix(X)) ){
        stop("X needs to be a numeric matrix.")
    }
    if ( !(is.numeric(Y)) && !(is.matrix(Y)) ){
        stop("Y needs to be a numeric matrix.")
    }

    # check kernel is correct
    type <- type[1]
    if ( (type != "proj") && (type != "dist") ){
        stop("type needs to be either 'proj' or 'dist'.")
    }

    # for distpval; default is Hommel
    pmethod <- 0
    if (type=="dist") {
        if (pval==FALSE) {
            stop("MEA-MMD-Dist does not return a statistic, only a p-value.")
        }
        distpval <- distpval[1]
        if ( (distpval != "Hommel") && (distpval != "Fisher") ) {
            stop("distpval needs to be either 'Hommel' or 'Fisher'.")
        }

        ## Temporarily block Fisher method
        if (distpval == "Fisher") {
            stop("'Fisher' not supported in this version.")
        }


        if (distpval != "Hommel"){
            pmethod <- 1
            # then will be Fisher
        }

        nmethod <- nmethod[1]
        if ( (nmethod != 1) && (nmethod != 2) ){
            stop("nmethod needs to be either 1 or 2")
        }
        if (nmethod==2){
            # C++ code expects 0 for 2 norm to be used
            nmethod <- 0
        }
    }

    nX <- dim(X)[1]
    dX <- dim(X)[2]
    nY <- dim(Y)[1]
    dY <- dim(Y)[2]

    # check dimensions compatible
    if (dX != dY){
        stop("Dimension (number of columns) of matrices need to be equal.")
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

    fasterInt <- 1
    if (faster==TRUE)
        fasterInt <- 0


    # flatten to vectors;  we use transpose
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

    meammdList <- list()
    if (type=="proj"){
        if (pval){
            meammdList <- meammd_proj_pval_Rcpp(Xvec, 
                                                Yvec,
                                                nX, dX,
                                                nY, dY, 
                                                numperm, 
                                                numproj, 
                                                seednum, 
                                                beta, 
                                                twosided, 
                                                boundminpval, 
                                                fasterInt)
        } else {
            stat <- meammd_proj_Rcpp(Xvec, 
                                     Yvec,
                                     nX, dX,
                                     nY, dY, 
                                     numproj, 
                                     seednum,
                                     beta)
            meammdList$stat <- stat
            meammdList$pval <- NA
        }
    } else {
        # type is dist
         pval <- meammd_dist_pval_Rcpp(Xvec, 
                                       Yvec,
                                       nX, dX,
                                       nY, dY, 
                                       numperm, 
                                       seednum, 
                                       beta, 
                                       pmethod, 
                                       nmethod,
                                       twosided, 
                                       boundminpval)

            meammdList$stat <- NA
            meammdList$pval <- pval

    }
    # return list
    return(meammdList)
}
