#' @title slim
#'
#' @description Compute ratings and coefficient matrix for the sparse ratings
#'   matrix using SLIM
#'
#' @param mat (sparse matrix of class 'dgCMatrix') Rating matrix with items
#'   along columns and users along rows.
#'
#' @param alpha (0 <= alpha <= 1) Parameter to decide the relative weightage
#'   between the L1 and L2 penalities. See \link[glmnet]{glmnet} for more
#'   details. This is set by default at \code{0.5}.
#'
#' @param lambda (positive real number) Parameter to control shrinkage of
#'   coefficients. See \link[glmnet]{glmnet} for more details. Its advisable not
#'   to provide the lambda value, as the function figures out the optimal value
#'   by itself.
#'
#' @param nlambda (positive integer) Maximum length of the lambda sequence. See
#'   \link[glmnet]{glmnet} for more details. If \code{nlambda} argument is
#'   missing, it will be set to 100L. This is overridden if \code{lambda} is
#'   specified.
#'
#' @param nonNegCoeff (flag) Whether the regression coefficients should be
#'   non-negative. There are instances where setting to FALSE decreases the
#'   RMSE, but sometimes this could lead to overfitting. Setting
#'   \code{nonNegCoeff} is FALSE, helps interpreting coefficients in the case of
#'   implicit feedback. This is set to TRUE by default.
#'
#' @param coeffMat (flag) Whether coeffMat is to be computed. This can be later
#'   used to predict recommendations for users not present in the \code{mat}
#'   (although \code{slimrec} package does not provide a \code{predict function}
#'   ). Setting it TRUE increases the computation time. This is set to FALSE by
#'   default.
#'
#' @param returnMat (flag) Whether the predicted ratings matrix and coefficient
#'   matrix (only if \code{coeffMat} is TRUE) to be read into memory as matrices
#'   and delete on disk \code{bigmatrix} objects. When output matrices are
#'   large, setting \code{returnMat} to TRUE is not advisable. This is set to
#'   FALSE by default.
#'
#' @param nproc (positive integer) Number of parallel processes to be used to
#'   compute coefficients for items. If the machine has \code{k} (>1) cores, the
#'   function does not employ more than \code{k - 1} cores. This is set to 1L by
#'   default.
#'
#' @param progress (flag) If TRUE(default), shows a progress bar and expected
#'   time. This is set to TRUE by default.
#'
#' @param check (flag) If TRUE(default), ckecks like whether the matrix is
#'   sparse, matrix does not contains NAs, alpha lies between 0 and 1, directory
#'   if specified is writable and so on. This is set to TRUE by default.
#'
#' @param directory (string) A writable directory where a sub-directory is
#'   created at the run time and \code{bigmatrix} objects will be written to.
#'   Predicted ratings data is stored in \code{ratingMat} file and the
#'   description is written to \code{ratingMat.desc} file. If \code{coeffMat} is
#'   TRUE, the coefficents matrix is stored in the file \code{coeffMat} and the
#'   description is written to \code{coeffMat.desc} file. When directory
#'   argument is missing, directory is set via \code{tempdir()}.
#'
#' @param computeRMSE (flag) Whether RMSE values have to be computed
#'   corresponding to non-zero values of the \code{mat}, both overall and
#'   columnwise.
#'
#' @param cleanup (flag) Whether to delete the sub-directory. Note that
#'   \code{returnMat}  cannot be set to FALSE when \code{cleanup} is TRUE. This
#'   is set to FALSE by default.
#'
#' @return A list with these elements: \itemize{
#'
#'   \item ratingMat: If \code{returnMat} is TRUE, the predicted ratings matrix.
#'   Else, \code{NULL}
#'
#'   \item coeffMat: If \code{returnMat} is TRUE and \code{coeffMat} is TRUE,
#'   the coefficient matrix. Else, \code{NULL}
#'
#'   \item lambdas: When \code{lambda} is not specified, a vector(length of
#'   number of columns of \code{mat}) of lambda values chosen. When
#'   \code{lambda} is specified, it is singleton \code{lambda} value.
#'
#'   \item columnwiseNonZeroRMSE: If \code{computeRMSE} is TRUE, vector of RMSE
#'   for each column. The errors are computed over only non-zero values of the
#'   column of \code{mat}. If \code{computeRMSE} is FALSE, value is set to
#'   \code{NULL}.
#'
#'   \item nonZeroRMSE: If \code{computeRMSE} is TRUE, RMSE value. The errors
#'   are computed over only non-zero values of the \code{mat}. If
#'   \code{computeRMSE} is FALSE, value is set to \code{NULL}.
#'
#'   \item subdir: Path to the sub-directory where output are placed.
#'
#'   \item call: function call
#'
#'   }
#'
#' @details \strong{Sparse linear method}
#'   (\href{http://glaros.dtc.umn.edu/gkhome/node/774}{DOI:
#'   10.1109/ICDM.2011.134}): The method predicts ratings of a user for a given
#'   item as a linear combination ratings of all other items provided by the
#'   user. The coefficients for an item are determined elastic-net regression
#'   (both L1 and L2 regularization) over ratings matrix.
#'
#'   The optimization problem solves:
#'
#'   \deqn{\min_{c_{j,.}} \frac{1}{2} \|a_{j,.} - Ac_{j,.}\|^2_{2} +
#'   \frac{\beta}{2} \|c_{j,.}\|^2_{2} + \gamma \|c_{j,.}\|_{1}} subject to
#'   \eqn{c_{j,j} = 0} and optional non-negative constraint \eqn{c_{j,.} >= 0}
#'   where \eqn{a_{j,.}} is the j th column of the input ratings matrix and
#'   \eqn{c_{j,.}} is the j th column of the coefficient matrix(to be
#'   determined).
#'
#'   The method assumes that unknown rating values to be zero. Hence, it is
#'   primarily designed for implicit feeback mechanisms, but not restricted
#'   them. The main use of the ratings is to generate top-n lists of users and
#'   items.
#'
#'   \strong{Implementation}: The non-negative ratings data is input as a sparse
#'   matrix of class \code{dgCMatrix} without any \code{NA}. The items should
#'   constitute columns and users should constitute rows. The elastic-net
#'   regression problem is solved using \code{glmnet} package. The coefficients
#'   for each item (a column of the ratings matrix) is computed, in parallel. To
#'   avoid memory overload, the output(s) is written to a disk based bigmatrix
#'   (using \code{bigmemory} package). The predicted rating matrix is the
#'   primary output. It is possible to obtain the matrix of coefficients, which
#'   will be helpful later to 'predict' the ratings for users not present in the
#'   ratings matrix. The RMSE may be computed itemwise and for the entire
#'   non-zero values of the ratings matrix. Since, \code{lambda} is
#'   auto-adjusted, change in \code{alpha} might not have significant impact on
#'   the RMSE. When it is necessary to get the best accuracy, there is a 'tune'
#'   function to arrive at the optimal \code{alpha} value by cross-validation.
#'   There are options to read the disk based matrix(s) into memory (as
#'   matrices) and remove the disk based ones.
#'
#' @examples
#' require("slimrec")
#' data(ft_small)
#' temp <- slim(ft_small)
#' str(temp)
#'
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

slim <- function(mat
                 , alpha       = 0.5
                 , lambda
                 , nlambda
                 , nonNegCoeff = TRUE
                 , directory
                 , coeffMat    = FALSE
                 , returnMat   = FALSE
                 , computeRMSE = FALSE
                 , nproc       = 1L
                 , progress    = TRUE
                 , check       = TRUE
                 , cleanup     = FALSE
                 ){
  # assertions                                            ----
  if(check){
    if(!(inherits(mat, "dgCMatrix"))){
      stop("'mat' should be a sparse matrix of class 'dgCMatrix'")
    }
    if(anyNA(mat)){
      stop("'mat' should not contain NA")
    }
    if(!(is.number(alpha) || alpha >= 0 || alpha <= 1)){
      stop("'alpha' should be a number between(inclusive) 0 and 1")
    }
    if((!missing(lambda)) && (!missing(nlambda))){
      stop("Specify only one among 'lambda' and 'nlambda'")
    }
    if(!(missing(lambda))){
      if(!is.number(lambda)){
        stop("'lambda' has to be a positive real number")
      }
    }
    if(!(missing(nlambda))){
      if(!is.count(nlambda)){
        stop("'nlambda' has to be a postive integer")
      }
    }
    if(!(is.flag(nonNegCoeff))){
      stop("'nonNegCoeff' has to either TRUE or FALSE")
    }
    if(!(is.count(nproc))){
      stop("'nproc' should be a positive integer")
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
    if(!(is.flag(coeffMat))){
      stop("'coeffMat' should be either TRUE or FALSE")
    }
    if(!(is.flag(returnMat))){
      stop("'returnMat' should be either TRUE or FALSE")
    }
    if(!(is.flag(computeRMSE))){
      stop("'computeRMSE' should be either TRUE or FALSE")
    }
    if(!(is.flag(check))){
      stop("'check' should be either TRUE or FALSE")
    }
    if(!(is.flag(progress))){
      stop("'progress' should be either TRUE or FALSE")
    }
    if(!(is.flag(cleanup))){
      stop("'cleanup' should be either TRUE or FALSE")
    }
    if(cleanup && !returnMat){
      stop("Simultaneously, 'cleanup' cannot be TRUE and 'returnMat' cannot be FALSE")
    }
  }

  # prelim setup                                          ----
  singleLambda <- FALSE
  if(!(missing(lambda))){
    singleLambda <- TRUE
  }
  if(missing(lambda) && missing(nlambda)){
    nlambda <- 100L
  }
  # set nproc
  nproc <- min(nproc, max(parallel::detectCores() - 1, 1))
  # set m and n
  m <- nrow(mat)
  n <- ncol(mat)
  if(!(m > 1L && n > 1L)){
    stop("'mat' should have at least two rows and two columns")
  }
  # replicate ll
  if(nonNegCoeff){
    ll <- rep(0L, n)
  } else {
    ll <- rep(-Inf, n)
  }
  # define upper limits, contrain W_{ii} to zero
  ul <- rep(Inf, n)

  # create a shared filebacked big matrices               ----
  dirInt <- file.path(normalizePath(directory)
                      , paste(sample(c(letters, LETTERS), 20), collapse = "")
                      )
  dir.create(dirInt)
  RM <- filebacked.big.matrix(nrow             = m
                              , ncol           = n
                              , type           = "double"
                              , separated      = FALSE
                              , backingfile    = "ratingMat"
                              , backingpath    = dirInt
                              , descriptorfile = "ratingMat.desc"
  )
  if(coeffMat){
    CM <- filebacked.big.matrix(nrow             = n
                                , ncol           = n
                                , type           = "double"
                                , separated      = FALSE
                                , backingfile    = "coeffMat"
                                , backingpath    = dirInt
                                , descriptorfile = "coeffMat.desc"
    )
  }

  # function to get coefficients and ratings for a column ----
  genRat <- function(acolIndex){
    # contrain w_{ii} to zero
    ul[acolIndex] <- 0L
    ll[acolIndex] <- 0L
    if(singleLambda){
      en_model      <- glmnet(x              = mat
                              , y            = mat[, acolIndex]
                              , alpha        = alpha
                              , lambda       = lambda
                              , intercept    = FALSE
                              , standardize  = FALSE
                              , lower.limits = ll
                              , upper.limits = ul
                              )
      lambdaVal     <- lambda
      coeff         <- en_model$beta
    } else{
      en_model      <- glmnet(x              = mat
                              , y            = mat[, acolIndex]
                              , alpha        = alpha
                              , nlambda      = nlambda
                              , intercept    = FALSE
                              , standardize  = FALSE
                              , lower.limits = ll
                              , upper.limits = ul
                              )
      lambdaVal     <- tail(en_model$lambda, 1L)
      lambdaLen     <- length(en_model$lambda)
      coeff         <- en_model$beta[, lambdaLen]
    }

    RM          <- attach.big.matrix(file.path(dirInt, "ratingMat.desc"))
    r_col       <- mat %*% coeff
    RM[, acolIndex] <- r_col
    flush(RM)
    if(coeffMat){
      CM          <- attach.big.matrix(file.path(dirInt, "coeffMat.desc"))
      CM[, acolIndex] <- coeff
      flush(CM)
    }
    return(lambdaVal)
  }

  # compute coefficients and ratings                      ----
  if(nproc == 1){
    if(progress){
      lambdas <- pblapply(1:n, genRat)
    } else {
      lambdas <- mclapply(1:n, genRat)
      }
  } else {
    cl   <- makeCluster(nproc)

    clusterEvalQ(cl, require("bigmemory"))
    clusterEvalQ(cl, require("glmnet"))
    if(progress){
      lambdas <- pblapply(1:n, genRat, cl = cl)
    } else {
      lambdas <- parLapply(cl = cl, 1:n, genRat)
    }
    stopCluster(cl)
  }
  lambdas <- unlist(lambdas)

  # computing RMSE                                        ----
  if(computeRMSE){

    cnzr <- function(cn){
      nz <- (mat[,cn] != 0)
      return( mat[nz, cn] - RM[nz, cn] )
    }

    colwiseNonZeroDiff    <- lapply(1:n, cnzr)

    fRMSE <- function(errorVec) { sqrt(sum(errorVec^2)/length(errorVec)) }

    columnwiseNonZeroRMSE <- vapply(colwiseNonZeroDiff, fRMSE, numeric(1))
    nonZeroRMSE           <- fRMSE(unlist(colwiseNonZeroDiff))
  }

  # handle return                                         ----
  out        <- vector(mode = "list", length = 7)
  names(out) <- c("ratingMat"
                  , "coeffMat"
                  , "lambdas"
                  , "columnwiseNonZeroRMSE"
                  , "nonZeroRMSE"
                  , "subdir"
                  , "call")
  if(coeffMat && returnMat){
    out[["coeffMat"]]   <- as.matrix(CM)
  }
  if(returnMat){
    out[["ratingMat"]]  <- as.matrix(RM)
  }
  if(singleLambda){
    out[["lambdas"]] <- lambda
  } else {
    out[["lambdas"]] <- lambdas
  }
  if(computeRMSE){
    out[["columnwiseNonZeroRMSE"]] <- columnwiseNonZeroRMSE
    out[["nonZeroRMSE"]]           <- nonZeroRMSE
  }
  out[["subdir"]]    <- dirInt
  out[["call"]]      <- match.call()

  if(cleanup){
    unlink(dirInt, recursive = TRUE)
  }
  return(out)
}
