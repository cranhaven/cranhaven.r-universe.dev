#' Checks for the prediction methods
#'
#' It contains all the check functions in the package.
#' All the arguments are identical to the inputs of conformal.split.multi in the file
#' split.multi.R.
#'
#' @param x The feature variables, a matrix n x p.
#' @param y The matrix of multivariate responses (dimension n x q)
#' @param x0 The new points to evaluate, a matrix of dimension n0 x p.
#' @param train.fun A function to perform model training, i.e., to produce an
#'   estimator of E(Y|X), the conditional expectation of the response variable
#'   Y given features X. Its input arguments should be x: matrix of features,
#'   and y: matrix of responses.
#' @param predict.fun A function to perform prediction for the (mean of the)
#'   responses at new feature values. Its input arguments should be out: output
#'   produced by train.fun, and newx: feature values at which we want to make
#'   predictions.
#' @param alpha Miscoverage level for the prediction intervals, i.e., intervals
#'   with coverage 1-alpha are formed. Default for alpha is 0.1.
#' @param seed Integer to be passed to set.seed before defining the random
#'   data-split to be used. Default is FALSE, which effectively sets no seed.
#'   If both split and seed are passed, the former takes priority and the latter
#'   is ignored.
#' @param randomized Should the randomized approach be used? Default is FALSE.
#' @param seed.tau The seed for the randomized version. Default is FALSE.
#' @param training_size Split proportion between training and calibration set.
#' Default is 0.5.
#' @param score The non-conformity measure. It can either be "max", "l2", "mahalanobis".
#' The default is "l2".
#' @param mad.train.fun A function to perform training on the absolute residuals
#'   i.e., to produce an estimator of E(R|X) where R is the absolute residual
#'   R = |Y - m(X)|, and m denotes the estimator produced by train.fun.
#'   This is used to scale the conformal score, to produce a prediction interval
#'   with varying local width. The input arguments to mad.train.fun should be
#'   x: matrix of features, y: vector of absolute residuals, and out: the output
#'   produced by a previous call to mad.train.fun, at the \emph{same} features
#'   x. The function mad.train.fun may (optionally) leverage this returned
#'   output for efficiency purposes. See details below. The default for
#'   mad.train.fun is NULL, which means that no training is done on the absolute
#'   residuals, and the usual (unscaled) conformal score is used. Note that if
#'   mad.train.fun is non-NULL, then so must be mad.predict.fun (next).
#' @param mad.predict.fun A function to perform prediction for the (mean of the)
#'   absolute residuals at new feature values. Its input arguments should be
#'   out: output produced by mad.train.fun, and newx: feature values at which we
#'   want to make predictions. The default for mad.predict.fun is NULL, which
#'   means that no local scaling is done for the conformal score, i.e., the
#'   usual (unscaled) conformal score is used.
#' @noRd

check.split=function(x,y,x0,train.fun,
               predict.fun, alpha, seed=1, training_size=0.5, seed.rand=2, randomized=FALSE,mad.train.fun=NULL, mad.predict.fun=NULL, score="l2"){


  check.null.data(y)
  check.null.data(x)
  check.null.data(x0)

  if ( is.data.frame(x)==FALSE && is.matrix(x)==FALSE){

    stop("x must be a matrix or a df of dimension n x p")}



  if ( is.data.frame(y)==FALSE && is.matrix(y)==FALSE){

    stop("y must be a matrix or a df of dimension n x q")}


  if ( is.data.frame(x0)==FALSE && is.matrix(x0)==FALSE){

    stop("x0 must be a matrix or a df of dimension n0 x p")}


 if (dim(x)[1] != dim(y)[1]) stop("x and y must have the same number of rows")

  if (dim(x)[2] != dim(x0)[2]) stop("x and x0 must have the same number of columns")


  if (is.null(train.fun) || !is.function(train.fun))
    stop("train.fun must be a function")


  if (is.null(predict.fun) || !is.function(predict.fun))
    stop("predict.fun must be a function")

  if (!is.null(mad.train.fun) && !is.function(mad.train.fun))
    stop("mad.train.fun must be a function")
  if (!is.null(mad.predict.fun) && !is.function(mad.predict.fun))
    stop("mad.predict.fun must be a function")
  if ((!is.null(mad.train.fun) && is.null(mad.predict.fun)) ||
      (is.null(mad.train.fun) && !is.null(mad.predict.fun)))
    stop("mad.train.fun and mad.predict.fun must both be provided")


  check.num.01(alpha)


  if (is.null(seed)==TRUE || (seed!=FALSE & is.numeric(seed)==FALSE))
    stop("Argument 'seed' must be either FALSE or an integer.")



  if (is.null(training_size)==TRUE || (training_size!=FALSE & is.numeric(training_size)==FALSE)) stop("Argument 'training_size' must be either FALSE or an integer.")


  check.num.01(training_size)


  if (is.null(randomized)==TRUE || randomized %in% c("TRUE","FALSE")==FALSE)
    stop("Argument 'randomized' must be either TRUE or FALSE")

  if(score == "scaled.max")
    stop("Argument 'score' cannot take value 'scaled.max' in the multivariate conformal split")

  check.score(score)

}

check.full=function(x,y,x0,train.fun,predict.fun, alpha, num.grid.pts.dim,grid.factor,score,mad.train.fun=NULL, mad.predict.fun=NULL){

  check.null.data(y)
  check.null.data(x)
  check.null.data(x0)

  if ( is.data.frame(x)==FALSE && is.matrix(x)==FALSE){

    stop("x must be a matrix or a df of dimension n x p")}



  if ( is.data.frame(y)==FALSE && is.matrix(y)==FALSE){

    stop("y must be a matrix or a df of dimension n x q")}

  if(ncol(y)>2){
    stop("Too many dimensions for the response. The full conformal method is too computationally extensive. Try with the split conformal method.")
  }


  if ( is.data.frame(x0)==FALSE && is.matrix(x0)==FALSE){

    stop("x0 must be a matrix or a df of dimension n0 x p")}


  if (dim(x)[1] != dim(y)[1]) stop("x and y must have the same number of rows")

  if (dim(x)[2] != dim(x0)[2]) stop("x and x0 must have the same number of columns")


  if (is.null(train.fun) || !is.function(train.fun))
    stop("train.fun must be a function")


  if (is.null(predict.fun) || !is.function(predict.fun))
    stop("predict.fun must be a function")

  if (!is.null(mad.train.fun) && !is.function(mad.train.fun))
    stop("mad.train.fun must be a function")
  if (!is.null(mad.predict.fun) && !is.function(mad.predict.fun))
    stop("mad.predict.fun must be a function")
  if ((!is.null(mad.train.fun) && is.null(mad.predict.fun)) ||
      (is.null(mad.train.fun) && !is.null(mad.predict.fun)))
    stop("mad.train.fun and mad.predict.fun must both be provided")


  check.num.01(alpha)

  if (length(num.grid.pts.dim) != 1 || !is.numeric(num.grid.pts.dim)
      || num.grid.pts.dim <= 1 || num.grid.pts.dim >= 1000
      || round(num.grid.pts.dim) != num.grid.pts.dim) {
    stop("num.grid.pts must be an integer between 1 and 1000")
  }
  check.pos.num(grid.factor)


    possible_score_functions=c('l2','mahalanobis','max')
    if (is.null(score) || score %in% possible_score_functions==FALSE) {
      stop(c("The 'score' argument is not correct. Please select one of the following:",paste(possible_score_functions,collapse=", "),"."))
    }
}





check.null.data=function(y){


  if (is.null(y)) stop("y must be either be a matrix or a df")
}



check.bool = function(b) {
  if (is.null(b) || length(b)!=1 || !is.logical(b))
    stop(paste(deparse(substitute(b)),"must be a Boolean"))
}

check.num = function(a) {
  if (is.null(a) || length(a)!= 1 || !is.numeric(a))
    stop(paste(deparse(substitute(a)),"must be a number"))
}

check.int = function(i) {
  if (is.null(i) || length(i)!= 1 || !is.numeric(i) || round(i) != i)
    stop(paste(deparse(substitute(i)),"must be an integer"))
}

check.pos.num = function(a) {
  if (is.null(a) || length(a)!= 1 || !is.numeric(a) || a<0)
    stop(paste(deparse(substitute(a)),"must be a positive number"))
}

check.pos.int = function(i) {
  if (is.null(i) || length(i)!= 1 || !is.numeric(i) || round(i) != i || i<1)
    stop(paste(deparse(substitute(i)),"must be a positive integer"))
}

check.num.01 = function(a) {
  if (is.null(a) || length(a)!= 1 || !is.numeric(a) || a<0 || a>1)
    stop(paste(deparse(substitute(a)),"must be a number between 0 and 1"))
}

check.tau=function(alpha,tau,l){

  if (alpha<tau/(l+1) & alpha>0)
    stop ("The prediction band obtained with such a small value of alpha is the entire space.
                                       If you are using the non randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than 1/(l+1) and less than 1.
                                       If you are using the randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than tau/(l+1) and less than (tau+l)/(l+1).")


  if (alpha>=(l+tau)/(l+1) || alpha<=0)
    stop("The alpha value is not admissible.
                                                   If you are using the non randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than 1/(l+1) and less than 1.
                                                   If you are using the randomized version of the algorithm, non-trivial prediction bands can be obtained by using an alpha greater or equal than tau/(l+1) and less than (tau+l)/(l+1).")

}

check.s_regression=function(mat_residual,type){


    if( is.matrix(mat_residual)==FALSE & is.data.frame(mat_residual)==FALSE & (is.atomic(mat_residual)==FALSE || is.vector(mat_residual)==FALSE)) stop("vec_residual must be either a matrix, a dataframe or an atomic vector (naive case).")



  #check on 'type' argument
  possible_s_functions=c("identity","st-dev","alpha-max")
  if (is.null(type) || type %in% possible_s_functions==FALSE) {
    stop(c("The 'type' argument is not correct. Please select one of the following:",paste(possible_s_functions,collapse=", "),"."))
  }

}

check.score = function(score){

  #check on 'score' argument
  possible_scores=c("l2","mahalanobis","max","scaled.max")
  if (is.null(score) || score %in% possible_scores==FALSE) {
    stop(c("The 'score' argument is not correct. Please select one of the following:",paste(possible_scores,collapse=", "),"."))
  }
}
