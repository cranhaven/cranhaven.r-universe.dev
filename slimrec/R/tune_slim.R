#' @title tune_slim
#'
#' @description Arrive at an optimal value of \code{alpha} for
#'   \code{\link{slim}}
#'
#' @details Runs \code{nfold} cross-validation to aid determining the optimal
#'   value of \code{alpha}(see \code{\link{slim}} for details). The coefficient
#'   matrix obtained from the training fold is used to predict ratings of the
#'   validation fold. The RMSE is evaluated for non-zero ratings and averaged
#'   over all the folds. Note that coefficient matrix is held in memory while
#'   computing RMSE. \code{\link{slim}} adjusts \code{lambda} while fitting an
#'   elastic-net, hence advantages in searching for optimal \code{alpha} might
#'   be limited.
#'
#' @param mat (sparse matrix of class 'dgCMatrix') Rating matrix with items
#'   along columns and users along rows.
#'
#' @param alphaRange (numeric vector) A vector of alpha values with 0 <= alpha
#'   <= 1. Default is values 0 to 1, with a difference of 0.1.
#'
#' @param nonNegCoeff (flag) Whether the regression coefficients should be
#'   non-negative. Default is TRUE.
#'
#' @param nfold (positive integer) Number of folds for cross-validation. Only
#'   values between(inclusive) 2 and 10 are allowed.
#'
#' @param seed (positive integer) Seed to be used to create folds for
#'   cross-validation. If missing, a random integer is chosen. Setting this is
#'   helpful for reproduciing the results. Default is 5.
#'
#' @param directory (string) A writable directory where a sub-directory is
#'   created at the run time and \code{bigmatrix} objects will be written to.
#'   The sub-directories are deleted at the end. If missing, this is set using
#'   \code{tempdir()}
#'
#' @param nproc (positive integer) Number of parallel processes to be used to
#'   compute coefficients for items. If the machine has \code{k} (>1) cores, the
#'   function does not employ more than \code{k - 1} cores. This is set to 1L by
#'   default.
#'
#' @param progress (flag) If TRUE(default), shows a progress bar and expected
#'   time. This is set to TRUE by default.
#'
#' @return A dataframe with two columns: \code{alpha} and \code{error}.
#'
#' @examples
#' require("slimrec")
#' data(ft_small)
#' \dontrun{
#' temp <- tune_slim(ft_small)
#' temp
#' temp <- tune_slim(ft_small, alphaRange = c(0, 0.5, 1))
#' temp
#' temp <- tune_slim(ft_small, alphaRange = c(0, 0.5, 1), nproc = 2)
#' temp
#' temp <- tune_slim(ft_small, nonNegCoeff = FALSE)
#' temp
#' }
#'
#' @export
#'
tune_slim <- function(mat
                      , alphaRange  = seq(0, 1, 0.1)
                      , nonNegCoeff = TRUE
                      , nfold       = 5L
                      , seed
                      , directory
                      , nproc       = 1L
                      , progress    = TRUE
                      ){
  # assertions ----
  if(!(inherits(mat, "dgCMatrix"))){
    stop("'mat' should be a sparse matrix of class 'dgCMatrix'")
  }
  if(anyNA(mat)){
    stop("'mat' should not contain NA")
  }
  if(!(all(alphaRange >= 0) && all(alphaRange <= 1))){
    stop("'alpha' should be a number between(inclusive) 0 and 1")
  }
  alphaRange <- sort(alphaRange)
  if(!(is.flag(nonNegCoeff))){
    stop("'nonNegCoeff' has to either TRUE or FALSE")
  }
  if(!(is.count(nfold) && nfold >= 2 && nfold <= 10)){
    stop("'nfold' has to be positive integer between(inclusive) 2 and 10")
  }
  if(missing(seed)){
    seed <- ceiling(runif(1, min = 1, max = 1000))
    message("Using seed: ", seed)
  } else {
    if(!(is.count(seed))){
      stop("'seed' should be a positive integer")
    }
  }
  if(missing(directory)){
    directory <- tempdir()
  } else {
    if(!(is.string(directory))){
      stop("'directory' should be a string")
    }
    if(!(dir.exists(directory))){
      stop("'directory' should exist")
    }
    if(!(is.writeable(directory))){
      stop("'directory' is not writable. Check user permissions")
    }
  }
  if(!(is.count(nproc))){
    stop("'nproc' should be a positive integer")
  }
  if(!(is.flag(progress))){
    stop("'progress' should be either TRUE or FALSE")
  }

  # cv ----
  # create partition for cross-validation
  m <- ncol(mat)
  n <- nrow(mat)
  foldSize  <- floor(m/nfold)
  leftout   <- m - (foldSize * nfold)
  folds     <- rep(foldSize, nfold)
  folds[1]  <- folds[1] + leftout
  set.seed(seed)
  cvSample  <- sample(unlist(Map(rep, 1:nfold, folds)))
  cvIndex   <- lapply(1:nfold, function(x) which(cvSample == x) )
  rm(m, n, foldSize, folds, leftout,cvSample)

  # function to compute error on a fold
  error_alpha <- function(alpha){

    errorVec <- numeric(nfold)
    for(foldNumber in 1:nfold){
      slim_result <- slim(mat           = mat[-cvIndex[[foldNumber]],]
                          , alpha       = alpha
                          , nonNegCoeff = nonNegCoeff
                          , returnMat   = TRUE
                          , coeffMat    = TRUE
                          , check       = FALSE
                          , directory   = directory
                          , progress    = FALSE
                          , cleanup     = TRUE
                          , nproc       = nproc
                          )
      validMat             <- mat[cvIndex[[foldNumber]],]
      pred                 <- validMat %*% slim_result[["coeffMat"]]
      nz                   <- which(validMat != 0)
      errorVec[foldNumber] <- sqrt(sum((validMat[nz] - pred[nz])^2)/length(nz))
    }
    return(mean(errorVec))
  }

  # run tune function over alphaRange ----
  if(progress){
    errorVals <- pblapply(alphaRange, error_alpha)
  } else {
    errorVals <- mclapply(alphaRange, error_alpha)
  }

  # handle return ----
  return(data.frame(alpha = alphaRange, error = unlist(errorVals)))
}