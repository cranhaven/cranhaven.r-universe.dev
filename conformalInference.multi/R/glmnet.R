#' Elastic Net, Lasso, Ridge Regression Training and Prediction Functions
#'
#' Construct training and prediction functions for the elastic net, the lasso,
#'   or ridge regression, based on the \code{\link{glmnet}} package, over a
#'   sequence of (given or internally computed) lambda values.
#'
#'
#' @param gamma Mixing parameter (between 0 and 1) for the elastic net, where
#'   0 corresponds to ridge regression, and 1 to the lasso. Default is 0.5.
#' @param standardize,intercept Should the data be standardized, and should an
#'   intercept be included? Default for both is TRUE.
#' @param lambda Sequence of lambda values over which training is performed.
#'   This must be in decreasing order, and --- this argument should be used with
#'   caution! When used, it is usually best to grab the sequence constructed by
#'   one initial call to glmnet (see examples). Default is NULL, which means that
#'   the nlambda, lambda.min.ratio arguments will define the lambda sequence
#'   (see next).
#' @param nlambda Number of lambda values over which training is performed. In
#'   particular, the lambda sequence is defined by nlambda log-spaced values
#'   between lambda.max and lambda.min.ratio * lambda.max, where lambda.max is
#'   the smallest value of lambda at which the solution has all zero components,
#'   and lambda.min.ratio is a small fraction (see next). Default is 50.
#' @param lambda.min.ratio Small fraction that gets used in conjunction with
#'   nlambda to specify a lambda sequence (see above). Default is 1e-4.
#' @param cv.rule If the cv argument is TRUE, then cv.rule determines which rule
#'   should be used for the predict function, either "min" (the usual rule) or
#'   "1se" (the one-standard-error rule). See the glmnet help files for details.
#'   Default is "min".
#'
#' @return A list with three components: train.fun, predict.fun, active.fun.
#'   The third function is designed to take the output of train.fun, and
#'   reports which features are active for each fitted model contained in
#'   this output.
#'
#' @details This function is based on the packages \code{\link{glmnet}}.
#'   Notice that Cross Validation to select the best lambda value is compulsory!
#'   The functions lasso.funs and ridge.funs are convenience
#'   functions, they simply call elastic.funs with gamma = 1 and gamma = 0,
#'   respectively.
#' @importFrom stats predict
#'
#' @rdname glmnet.funs
#' @export elastic.funs

elastic.funs = function(gamma=0.5, standardize=TRUE, intercept=TRUE, lambda=NULL,
                        nlambda=50, lambda.min.ratio=1e-4, cv.rule=c("min","1se")) {



  # Check arguments
  check.num.01(gamma)
  check.bool(standardize)
  check.bool(intercept)
  if (is.null(lambda) && (length(nlambda)!= 1 ||
                          round(nlambda) != nlambda ||
                          nlambda < 1 || nlambda > 100)) {
    stop("nlambda must be an integer between 1 and 100")
  }
  if (!is.null(lambda) && (!is.numeric(lambda) || min(lambda)<=0 ||
                           (order(lambda) != length(lambda):1))) {
    stop("lambda must be a decreasing sequence of positive numbers")
  }
  check.pos.num(lambda.min.ratio)
  cv.rule = match.arg(cv.rule)

  # They want training to be done over a sequence of lambdas, and prediction
  # at the lambda chosen by either the min CV rule or 1se CV rule


    train.fun.mono = function(x,y,out=NULL) {
      return(glmnet::cv.glmnet(x,y,alpha=gamma,nlambda=nlambda, #10 fold cv
                       lambda.min.ratio=lambda.min.ratio,lambda=lambda,
                       standardize=standardize,intercept=intercept))
    }

    train.fun=function(x,y,out=NULL){

      q=dim(y)[2]

      o=lapply(1:q, function(i) train.fun.mono(x,y[,i]))


      return(o)
    }

    predict.fun.mono = function(out,newx) {
      return(predict(out$glmnet.fit,newx,s=ifelse(cv.rule=="min",
                                                  out$lambda.min,out$lambda.1se)))
    }

    predict.fun = function(out,newx) {

      q=length(out)

      sol=lapply(1:q, function(i) predict.fun.mono(out[[i]],newx))



      sol_mat=do.call(cbind,sol)


      return(sol_mat)

    }


    active.fun = function(out) {
      b = glmnet::coef.glmnet(out$glmnet.fit,s=ifelse(cv.rule=="min",
                                       out$lambda.min,out$lambda.1se))
      if (intercept) b = b[-1]
      return(list(which(b!=0)))
    }


  return(list(train.fun=train.fun, predict.fun=predict.fun,
              active.fun=active.fun))
}

#' @rdname glmnet.funs
#' @export lasso.funs

lasso.funs = function(standardize=TRUE, intercept=TRUE, lambda=NULL,
                      nlambda=50, lambda.min.ratio=1e-4, cv.rule=c("min","1se")) {

  return(elastic.funs(gamma=1, #P# lasso
                      standardize=standardize,intercept=intercept,
                      lambda=lambda,nlambda=nlambda,
                      lambda.min.ratio=lambda.min.ratio,
                      cv.rule=cv.rule))
}

#' @rdname glmnet.funs
#' @export ridge.funs

ridge.funs = function(standardize=TRUE, intercept=TRUE, lambda=NULL,
                      nlambda=50, lambda.min.ratio=1e-4, cv.rule=c("min","1se")) {

  return(elastic.funs(gamma=0, #P# ridge
                      standardize=standardize,intercept=intercept,
                      lambda=lambda,nlambda=nlambda,
                      lambda.min.ratio=lambda.min.ratio,
                      cv.rule=cv.rule))
}
