#'fit lasso for conditional logistic regression for matched case-control studies
#'
#' Fit a sequence of conditional logistic regression with lasso penalty, for small to large sized samples
#'
#' @param X Input matrix, of dimension nobs x nvars; each row is an observation vector
#' @param y Binary response variable, with 1 for cases and 0 for controls
#' @param strata Vector with stratum membership of each observation
#' @param fraction Sequence of lambda values
#' @param nbfraction The number of lambda values - default is 100
#' @param nopenalize List of coefficients not to penalize starting at 0
#' @param BACK If TRUE, use Backtracking-line search -default is TRUE
#' @param standardize Logical flag for x variable standardization, prior to fitting the model sequence.
#' @param maxit Maximum number of iterations of outer loop - default is 100
#' @param maxitB Maximum number of iterations  in Backtracking-line search - default is 100
#' @param thr Threshold for convergence in lassoshooting. Default value is 1e-10. Iterations stop when max absolute parameter change is less than thr
#' @param tol Threshold for convergence-default value is 1e-10
#' @param epsilon ratio of smallest to largest value of regularisation parameter  at which we find parameter estimates
#' @param trace If TRUE the algorithm will print out information as iterations  proceed -default is TRUE
#' @param log If TRUE, fraction are spaced uniformly on the log scale
#' @param adaptive If TRUE adaptive lasso is fitted-default is FALSE
#' @param separate If TRUE, the weights in adaptive lasso are build separately using univariate models. Default is FALSE,
#' weights are build using multivariate model
#' @param ols If TRUE, weights less than 1 in adaptive lasso are set to 1. Default is FALSE
#' @param p.fact Weights for adaptive lasso
#' @param remove If TRUE, invariable covariates are removed-default is FALSE
#' @return An object of type \code{clogitLasso} which is a list with the following
#' components:
#'
#' \item{beta}{nbfraction-by-ncol matrix of estimated coefficients. First row has all 0s}
#'
#' \item{fraction}{A sequence of regularisation parameters at which we obtained the fits}
#'
#' \item{nz}{A vector of length nbfraction containing the number of nonzero parameter estimates for
#' the fit at the corresponding regularisation parameter}
#'
#' \item{arg}{List of arguments}
#' @author Marta Avalos, Helene Pouyes, Marius Kwemou and Binbin Xu
#' @details The sequence of models implied by fraction  is fit by  IRLS (iteratively
#' reweighted least squares) algorithm.
#' by coordinate descent with warm starts and sequential strong rules
#' @references Avalos, M., Pouyes, H., Grandvalet, Y., Orriols, L., & Lagarde, E. (2015). \emph{Sparse conditional logistic
#' regression for analyzing large-scale matched data from epidemiological studies: a simple algorithm.} BMC bioinformatics, 16(6), S1. \doi{10.1186/1471-2105-16-S6-S1}.
#' @importFrom lassoshooting lassoshooting
#' @importFrom stats var aggregate
#' @importFrom foreach "%do%" foreach
#' @examples
#' \dontrun{
#' # generate data
#' y <- rep(c(1,0), 100)
#' X <- matrix (rnorm(20000, 0, 1), ncol = 100) # pure noise
#' strata <- sort(rep(1:100, 2))
#' 
#' # 1:1
#' fitLasso <- clogitLasso(X,y,strata,log=TRUE)
#' }
#' @export

clogitLasso <- function(X,
                       y,
                       strata,
                       fraction = NULL,
                       nbfraction = 100,
                       nopenalize = NULL,
                       BACK = TRUE,
                       standardize = FALSE,
                       maxit = 100,
                       maxitB = 500,
                       thr = 1e-10,
                       tol = 1e-10,
                       epsilon = 0.0001,
                       trace = TRUE,
                       log = TRUE,
                       adaptive = FALSE,
                       separate = FALSE,
                       ols = FALSE,
                       p.fact = NULL,
                       remove = FALSE) {
  if (table(strata)[1] == 2) {
    res <- reg.diff(
      X = X,
      y = y,
      strata      = strata,
      fraction    = fraction,
      nbfraction  = nbfraction,
      nopenalize  = nopenalize,
      BACK        =  BACK,
      standardize = standardize,
      maxit  = maxit,
      maxitB = maxitB,
      thr = thr,
      tol = tol,
      epsilon = epsilon,
      trace   = trace,
      log     = log,
      adaptive = adaptive,
      separate = separate,
      ols    = ols,
      p.fact = p.fact,
      remove = remove
    )
  } else{
    res <- reg.diff1M(
      X = X,
      y = y,
      strata   = strata,
      fraction = fraction,
      nbfraction = nbfraction,
      nopenalize = nopenalize,
      BACK  = BACK,
      standardize = standardize,
      maxit  = maxit,
      maxitB = maxitB,
      thr = thr,
      tol = tol,
      epsilon = epsilon,
      trace   = trace,
      log     = log,
      adaptive = adaptive,
      separate = separate,
      ols    = ols,
      p.fact = p.fact,
      remove = remove
    )
  }
  class(res) <- "clogitLasso"
  return(res)
}