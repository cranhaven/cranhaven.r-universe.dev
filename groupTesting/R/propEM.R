#' EM Algorithm to Estimate the Prevalence of a Disease from Group Testing Data
#'
#' This function implements an expectation-maximization (EM) algorithm to find the maximum likelihood estimate (MLE) of a disease prevalence, p, based on group testing data. The EM algorithm, which is outlined in Warasi (2023), can model pooling data observed from \strong{any} group testing protocol used in practice, including hierarchical and array testing (Kim et al., 2007).
#'
#' @useDynLib groupTesting, .registration=TRUE
#' 
#' @param p0 An initial value of the prevalence.
#' @param gtData A matrix or data.frame consisting of the pooled test outcomes and other information from a group testing application. Needs to be specified as shown in the example below.
#' @param covariance When TRUE, the variance is calculated at the MLE.
#' @param nburn The number of initial Gibbs iterates to be discarded.
#' @param ngit The number of Gibbs iterates to be used in the E-step after discarding the initial iterates as a burn-in period.
#' @param maxit The maximum number of EM steps (iterations) allowed in the EM algorithm.
#' @param tol Convergence tolerance used in the EM algorithm.
#' @param tracing When TRUE, progress in the EM algorithm is displayed.
#' @param conf.level Confidence level to be used for the Wald confidence interval.
#'
#' @importFrom stats rbinom
#' @importFrom stats runif
#'
#' @details
#'
#' \code{gtData} must be specified as follows. Columns 1-5 consist of the pooled test outcomes (0 for negative and 1 for positive), pool sizes, pool-specific sensitivities, pool-specific specificities, and assay ID numbers, respectively. From column 6 onward, the pool member ID numbers need to be specified. Note that the ID numbers must start with 1 and increase consecutively up to \code{N}, the total number of individuals tested. \strong{For smaller pools, incomplete ID numbers must be filled out by -9 or any non-positive numbers} as shown in the example below. 
#' 
#' |     |  Z  |  psz  |   Se   |   Sp   |  Assay  |  Mem1  |  Mem2  |  Mem3  |  Mem4  |  Mem5  |  Mem6  |
#' |:---:|:---:|:-----:|:------:|:------:|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|       
#' | Pool:1   |  1  |   6   |  0.90  |  0.92  |    1    |    1    |    2    |    3    |    4    |    5    |    6    |
#' | Pool:2   |  0  |   6   |  0.90  |  0.92  |    1    |    7    |    8    |    9    |   10    |    11   |   12    |
#' | Pool:3   |  1  |   2   |  0.95  |  0.96  |    2    |    1    |    2    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:4   |  0  |   2   |  0.95  |  0.96  |    2    |    3    |    4    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:5   |  1  |   2   |  0.95  |  0.96  |    2    |    5    |    6    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:6   |  0  |   1   |  0.92  |  0.90  |    3    |    1    |   -9    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:7   |  1  |   1   |  0.92  |  0.90  |    3    |    2    |   -9    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:8   |  0  |   1   |  0.92  |  0.90  |    3    |    5    |   -9    |   -9    |   -9    |    -9   |   -9    |
#' | Pool:9   |  0  |   1   |  0.92  |  0.90  |    3    |    6    |   -9    |   -9    |   -9    |    -9   |   -9    |
#' 
#' This is an example of \code{gtData}, where 12 individuals are assigned to 2 non-overlapping initial pools and then tested based on the 3-stage hierarchical protocol. The test outcomes, \code{Z}, from 9 pools are in column 1. In three stages, different pool sizes (6, 2, and 1), sensitivities, specificities, and assays are used. The ID numbers of the pool members are shown in columns 6-11. The row names and column names are not required. Note that the EM algorithm can accommodate any group testing data including those described in Kim et al. (2007). For individual testing data, the pool size in column 2 is 1 for all pools.
#'
#' The EM algorithm implements a Gibbs sampler to approximate quantities required to complete the E-step. Under each EM iteration, \code{ngit} Gibbs samples are retained for these purposes after discarding the initial \code{nburn} samples.  
#' 
#' The variance of the MLE is calculated by an appeal to the missing data principle and the method outlined in Louis (1982).
#' 
#' @return A list with components:
#' \item{param}{The MLE of the disease prevalence.}
#' \item{covariance}{Estimated variance for the disease prevalence.}
#' \item{iterUsed}{The number of EM iterations used for convergence.}
#' \item{convergence}{0 if the EM algorithm converges successfully and 1 if the iteration limit \code{maxit} has been reached.}
#' \item{summary}{Estimation summary with Wald confidence interval.}
#' 
#' @export
#'
#' @references
#' Kim HY, Hudgens M, Dreyfuss J, Westreich D, and Pilcher C. (2007). Comparison of Group Testing Algorithms for Case Identification in the Presence of Testing Error. \emph{Biometrics}, 63:1152-1163.
#' 
#' Litvak E, Tu X, and Pagano M. (1994). Screening for the Presence of a Disease by Pooling Sera Samples. \emph{Journal of the American Statistical Association}, 89:424-434.
#' 
#' Liu A, Liu C, Zhang Z, and Albert P. (2012). Optimality of Group Testing in the Presence of Misclassification. \emph{Biometrika}, 99:245-251.
#' 
#' Louis T. (1982). Finding the Observed Information Matrix when Using the EM algorithm. \emph{Journal of the Royal Statistical Society: Series B}, 44:226-233.
#' 
#' Warasi M. (2023). groupTesting: An R Package for Group Testing Estimation. \emph{Communications in Statistics-Simulation and Computation}, 52:6210-6224.
#' 
#' @seealso
#' \code{\link{hier.gt.simulation}} and \code{\link{array.gt.simulation}} for group testing data simulation, and \code{\link{glm.gt}} for group testing regression models.
#'
#' @examples
#' 
#' library(groupTesting)
#' 
#' ## To illustrate 'prop.gt', we use data simulated by 
#' ## the R functions 'hier.gt.simulation' and 'array.gt.simulation'.
#' 
#' ## The simulated data-structures are consistent  
#' ## with the data-structure required for 'gtData'.
#' 
#' ## Example 1: MLE from 3-stage hierarchical group testing data.
#' ## The data used is simulated by 'hier.gt.simulation'.
#'
#' N <- 90               # Sample size
#' S <- 3                # 3-stage hierarchical testing
#' psz <- c(6,2,1)       # Pool sizes used in stages 1-3
#' Se <- c(.95,.95,.98)  # Sensitivities in stages 1-3
#' Sp <- c(.95,.98,.96)  # Specificities in stages 1-3
#' assayID <- c(1,2,3)   # Assays used in stages 1-3
#' p.t <- 0.05           # The TRUE parameter to be estimated
#'
#' # Simulating data:
#' set.seed(123)
#' gtOut <- hier.gt.simulation(N,p.t,S,psz,Se,Sp,assayID)$gtData
#'
#' # Running the EM algorithm:
#' pStart <- p.t + 0.2   # Initial value
#' res <- prop.gt(p0=pStart,gtData=gtOut,covariance=TRUE,
#'                nburn=2000,ngit=5000,maxit=200,tol=1e-03,
#'                tracing=TRUE,conf.level=0.95)
#' 
#' # Estimation results:
#' # > res
#' 
#' # $param
#' # [1] 0.05158
#' 
#' # $covariance
#' #              [,1]
#' # [1,] 0.0006374296
#' 
#' # $iterUsed
#' # [1] 4
#' 
#' # $convergence
#' # [1] 0
#' 
#' # $summary
#' #      Estimate StdErr 95%lower 95%upper
#' # prop    0.052  0.025    0.002    0.101
#' 
#' ## Example 2: MLE from two-dimensional array testing data.
#' ## The data used is simulated by 'array.gt.simulation'.
#' 
#' N <- 100             # Sample size
#' protocol <- "A2"     # 2-stage array without testing the initial master pool
#' n <- 5               # Row/column size
#' Se <- c(0.95, 0.95)  # Sensitivities
#' Sp <- c(0.98, 0.98)  # Specificities
#' assayID <- c(1, 1)   # The same assay in both stages
#' p.true <- 0.05       # The TRUE parameter to be estimated
#' 
#' # Simulating data:
#' set.seed(123)
#' gtOut <- array.gt.simulation(N,p.true,protocol,n,Se,Sp,assayID)$gtData
#' 
#' # Fitting the model:
#' pStart <- p.true + 0.2  # Initial value
#' res <- prop.gt(p0=pStart,gtData=gtOut,covariance=TRUE)
#' print(res)
#'
#' \donttest{
#' ## Example 3: MLE from non-overlapping initial pooled responses.
#' ## The data used is simulated by 'hier.gt.simulation'. 
#' 
#' ## Note: With initial pooled responses, our MLE is equivalent  
#' ## to the MLE in Litvak et al. (1994) and Liu et al. (2012).
#' 
#' N <- 1000             # Sample size
#' psz <- 5              # Pool size
#' S <- 1                # 1-stage testing
#' Se <- 0.95            # Sensitivity
#' Sp <- 0.99            # Specificity
#' assayID <- 1          # Assay used for all pools
#' p.true <- 0.05        # True parameter
#' 
#' set.seed(123)
#' gtOut <- hier.gt.simulation(N,p.true,S,psz,Se,Sp,assayID)$gtData
#' 
#' pStart <- p.true + 0.2   # Initial value
#' res <- prop.gt(p0=pStart,gtData=gtOut,
#'                covariance=TRUE,nburn=2000,ngit=5000,
#'                maxit=200,tol=1e-03,tracing=TRUE)
#' print(res)
#'
#' ## Example 4: MLE from individual (one-by-one) testing data.
#' ## The data used is simulated by 'hier.gt.simulation'.
#' 
#' N <- 1000             # Sample size
#' psz <- 1              # Pool size 1 (i.e., individual testing)
#' S <- 1                # 1-stage testing
#' Se <- 0.95            # Sensitivity
#' Sp <- 0.99            # Specificity
#' assayID <- 1          # Assay used for all pools
#' p.true <- 0.05        # True parameter
#' 
#' set.seed(123)
#' gtOut <- hier.gt.simulation(N,p.true,S,psz,Se,Sp,assayID)$gtData
#'
#' pStart <- p.true + 0.2   # Initial value
#' res <- prop.gt(p0=pStart,gtData=gtOut,
#'                covariance=TRUE,nburn=2000,
#'                ngit=5000,maxit=200,
#'                tol=1e-03,tracing=TRUE)
#' print(res)
#' 
#' ## Example 5: Using pooled testing data.
#'
#' # Pooled test outcomes:
#' Z <- c(1, 0, 1, 0, 1, 0, 1, 0, 0)     
#' 
#' # Pool sizes used:
#' psz <- c(6, 6, 2, 2, 2, 1, 1, 1, 1)
#' 
#' # Pool-specific Se & Sp:
#' Se <- c(.90, .90, .95, .95, .95, .92, .92, .92, .92)
#' Sp <- c(.92, .92, .96, .96, .96, .90, .90, .90, .90)
#' 
#' # Assays used:
#' Assay <- c(1, 1, 2, 2, 2, 3, 3, 3, 3)
#' 
#' # Pool members:
#' Memb <- rbind( 
#'    c(1, 2,  3,  4,  5,  6),
#'    c(7, 8,  9, 10, 11, 12),
#'    c(1, 2, -9, -9, -9, -9),
#'    c(3, 4, -9, -9, -9, -9),
#'    c(5, 6, -9, -9, -9, -9),
#'    c(1,-9, -9, -9, -9, -9),
#'    c(2,-9, -9, -9, -9, -9),
#'    c(5,-9, -9, -9, -9, -9),
#'    c(6,-9, -9, -9, -9, -9)
#' )
#' # The data-structure suited for 'gtData':
#' gtOut <- cbind(Z, psz, Se, Sp, Assay, Memb)
#' 
#' # Fitting the model:
#' pStart <- 0.10
#' res <- prop.gt(p0=pStart,gtData=gtOut,
#'                covariance=TRUE,nburn=2000,
#'                ngit=5000,maxit=200,
#'                tol=1e-03,tracing=TRUE)
#' print(res)
#' }
#'
prop.gt <- function(p0,gtData,covariance=FALSE,nburn=2000,ngit=5000,maxit=200,tol=1e-03,tracing=TRUE,conf.level=0.95){

  ## This block tracks the individuals assigned to a pool.
  ## Note: 'gtData' must have the required specific structure.
  Memb <- gtData[ ,-(1:5)]
  N <- max(Memb)
  maxAssign <- max(as.numeric(table(Memb[Memb > 0])))
  ytm <- matrix(-9,N,maxAssign)
  tmp <- as.matrix( Memb )
  vec <- 1:nrow(gtData)
  for(d in 1:N){
    tid <- tmp==d
    store <- NULL
    for(i in 1:ncol(tmp)){
      store <- c(store,vec[tid[ ,i]])
    }
    ytm[d,1:length(store)] <- sort(store)
  }

  ## Generating individual true disease statuses
  ## at the initial parameter value p0
  Yt <- stats::rbinom(N,1,p0)
  Ytmat <- cbind(Yt,rowSums(ytm>0),ytm)

  ## Some global variables
  Ycol <- ncol(Ytmat)
  SeSp <- gtData[ ,3:4]
  Z <- gtData[ ,-(3:5)]
  Zrow <- nrow(Z)
  Zcol <- ncol(Z)
  
  ## The total number of Gibbs iterates
  GI <- ngit + nburn
  
  ## Initial value of the parameter
  p1 <- p0
  p0 <- p0 + 2*tol
  s <- 1
  convergence <- 0
  
  ## The EM algorithm starts here
  while(abs(p1-p0) > tol){ 
    p0 <- p1
    U <- matrix(stats::runif(N*GI),nrow=N,ncol=GI)
    
    ## Gibbs sampling in Fortran to approximate the E-step
    res <- .Call("gbsonedhom_c",as.double(p0),as.integer(Ytmat),
                 as.integer(Z),as.integer(N),as.double(SeSp),as.integer(Ycol),
                 as.integer(Zrow),as.integer(Zcol),as.double(U),as.integer(GI),
                 as.integer(nburn), PACKAGE="groupTesting")
    temp <- sum( res )/ngit
    
    ## M-step: The parameter p is updated here
    p1 <- temp/N
    
    ## Terminate the EM algorithm if it exceeds max iteration
    if(s >= maxit){
      convergence <- 1
      break
    }
    s <- s + 1
    if(tracing){
      cat(s-1, p1, "\n")
    }
  }

  ## Covariance matrix in Fortran using Louis's (1982) method
  covr2 <- NULL
  if(covariance){
    U <- matrix(stats::runif(N*GI),nrow=N,ncol=GI)
    Info <- .Call("cvondknachom_c",as.double(p1),as.integer(Ytmat),
                  as.integer(Z),as.integer(N),as.double(SeSp),as.integer(Ycol),
                  as.integer(Zrow),as.integer(Zcol),as.double(U),as.integer(GI),
                  as.integer(nburn), PACKAGE="groupTesting")
    covr2 <- solve( Info )
  }

  ## Univariate Wald Inference
  pHat <- p1
  if(covariance){
    se <- sqrt(covr2)
    alternative <- "two.sided"
    ## Calculate the test statistic:
    z <- qnorm(ifelse(alternative=="two.sided",
                     (1+conf.level)/2, conf.level))
    ## Find the confidence interval:
    CI <- c(pHat-z*se, pHat+z*se)
    ## Organizing and reporting the output:
    res <- data.frame(round(pHat, 3), round(se, 3), 
                      round(CI[1],3), round(CI[2],3) )
  }else{
    res <- data.frame(round(pHat, 3), NA, NA, NA) 
  }

  rownames(res) <- colnames(res) <- NULL
  rownames(res) <- "prop"
  colnames(res) <- c("Estimate", "StdErr",                    
                     paste(conf.level*100, "%lower", sep=""),
                     paste(conf.level*100, "%upper", sep=""))

  # Output
  list("param"       = p1,
       "covariance"  = covr2,
	   "iterUsed"    = s-1,
       "convergence" = convergence,
	   "summary"     = res
	   )
}
