#' The optimal alpha for rSDR via cross-validation
#' @description
#' Performs k-folds cross-validation for rSDR method and returns the corresponding optimal alpha.
#' @usage optimal_alpha_cv(alpha.v,X,Y,d,kfolds=10,maxiter=1000,tol=1e-7)
#' @param alpha.v user-supplied alpha sequence. The default is alpha.v=c(0.3,0.4,0.5,0.6,0.7).
#' @param X an \eqn{n \times p} numeric matrix, where \eqn{n} is the number of observations and \eqn{p} is the number of variable.
#' @param Y an \eqn{n \times k} numeric response matrix, where \eqn{k (\geq 1)} is the number of response variables.
#' @param d the number of reduced dimension. The default is d=3.
#' @param kfolds the number of folds - default is 10.
#' @param maxiter maxiter is the maximum number of iterations allowed for the solver (a non-negative integer). See the Max_Iteration parameter in \code{\link[ManifoldOptim]{get.solver.params}} for details.
#' @param tol tol is used to assess convergence, see the Tolerance parameter in \code{\link[ManifoldOptim]{get.solver.params}} for details.
#'
#' @return
#' An object of class "optimal_alpha_cv" is returned. The returned value contains the following components:
#'  \describe{
#'   \item{opt.alpha}{value of alpha that gives minimum f_test.meam.}
#'   \item{f_test.mean}{The mean of cost value by the alpha sequence - a vector of length length(alpha.v).}
#'   \item{f_test.sd}{The standard deviation of cost value by the alpha sequence - a vector of length length(alpha.v).}
#'   \item{f_test}{A kfolds \eqn{\times} length(alpha.v) matrix. The cost value for each fold at a given alpha.}
#'   \item{d}{The value of d as passed to optimal_alpha_cv.}
#'   \item{kfolds}{The value of kfolds as passed to optimal_alpha_cv.}
#' }
#'
#' @export
#'
#' @importFrom future plan
#' @importFrom future.apply future_sapply future_lapply
#'
#' @examples
#' library(ManifoldOptim)
#' library(rSDR)
#' library(future)
#' library(future.apply)
#' utils::data("ionosphere", package = "fdm2id")
#' X<-as.matrix(ionosphere[,c(1:33)])
#' Y<-ifelse(ionosphere[,34]=='b',0,1)
#' Y<-matrix(Y,length(Y),1)
#' set.seed(2435)
#' # plan(multisession) will launch parallel workers running in the background
#' # to save running time. To shut down background workers launched this way, call
#' # plan(sequential)
#' # use all local cores except one
#' # future::plan(future::multisession, workers = future::availableCores() - 1)
#' # use 2 cores for parallel
#' \donttest{
#' future::plan("multisession", workers = 2)
#' opt_results<-optimal_alpha_cv(alpha.v=c(0.3, 0.5, 0.7),X=X,Y=Y,d=3,kfolds=5)
#' opt_results
#' }
optimal_alpha_cv <- function(alpha.v=c(0.3,0.4,0.5,0.6,0.7), X, Y, d=3, kfolds = 10,
                             maxiter = 1000, tol = 1e-7
) {
  # ---- Setup & input hygiene ----
  if (is.matrix(X)==FALSE) stop("X must be a matrix.")
  n<-nrow(X)
  if (length(Y) != n) stop("Y length must match nrow(X).")
  alpha.l<-length(alpha.v)

  # Precompute folds (as index vectors)
  # single shuffle; no need to re-shuffle each fold
  idx <- base::sample.int(n, size = n, replace = FALSE)
  # cut() is fine but split() on integer groups is faster
  grp <- as.integer(cut(seq_len(n), breaks = kfolds, labels = FALSE))
  folds <- split(idx, grp)


  message('This will take a few seconds or minutes to get the results.')

  # ---- Worker over a single fold ----
  #... was used for all the variable in this function
  # make sure to pass '...' via arguments all the way through
  fold_worker <- function(i,...) {## outer '...'
    test_idx  <- folds[[i]]
    train_flag <- rep(TRUE, n)
    train_flag[test_idx] <- FALSE

    X_train <- X[train_flag,]
    Y_train <- as.matrix(Y[train_flag,],n,1)
    X_test  <- X[test_idx, ]
    Y_test  <- as.matrix(Y[test_idx,],n,1)

    out<-future.apply::future_sapply(alpha.v,
                       function(a,...){
                         f_opt_testing(a,
                                       d=d,X_train=X_train,Y_train=Y_train,X_test=X_test,Y_test=Y_test,maxiter=maxiter,tol=tol)
                       },
                       future.seed=TRUE,
                       future.packages=c('ManifoldOptim','Rcpp'))
    out

  }


  res_list<-future.apply::future_lapply(seq_len(kfolds),
                          function(i,...){
                            fold_worker(i,...)
                          },
                          future.seed=TRUE,
                          future.packages=c('ManifoldOptim','Rcpp')
  )


  # Bind results

  f_test.matrix <- base::do.call(rbind, res_list)
  colnames(f_test.matrix)<-alpha.v

  # ---- Summaries ----
  f_test.mean <- base::colMeans(f_test.matrix)
  f_test.sd   <- base::apply(f_test.matrix, 2, stats::sd)
  opt.alpha   <- alpha.v[which.min(f_test.mean)]

  list(opt.alpha = opt.alpha,
       f_test.mean = f_test.mean,
       f_test.sd = f_test.sd,
       f_test = f_test.matrix,
       d = d,
       kfolds = kfolds)
}
