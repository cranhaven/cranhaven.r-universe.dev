#########1#########2#########3#########4#########5#########6#########7#########8
#' Best Linear Model in Subset Selection 
#'
#' Produces the best linear model for a specific number of predictors in
#' a subset selection.
#'
#' @param object An object of type "regsubsets"
#' @param d Number of data predictors
#' @return The best linear model with \code{d} predictors
#' @examples
#' subs=leaps::regsubsets(mpg~.,mtcars)
#' summary(lmSub(subs,3))
#' @import data.table
#' @export
################################################################################
lmSub<-function(object,d) {
  if (!inherits(object,"regsubsets")) stop("object must be a regsubsets object")
  if (!isInt(d) || d < 1) stop("d must be a positive integer")
  fr=as.formula(object$call[[2]])
  response=as.character(fr[[2]])
  dat=as.data.table(eval(object$call[[3]]))
  subsum=summary(object)
  dataSet=cbind(dat[,response,drop=F,with=FALSE],
                model.matrix(fr,dat)[,-1][,subsum$which[d,-1],drop=F])
  form=formula(paste(response,"~."))
  stats::lm(form,dataSet)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Obtain Predictions using Subset Selection
#'
#' Predict responses for the best model in a subset selection with a specific
#' number of predictors.
#'
#' @param object An object of type "regsubsets"
#' @param d Number of data predictors
#' @param newdata Dataset for which to predict responses
#' @param ... Additional arguments
#' @return A set of predicted responses for \code{newdata}
#' @examples
#' subs=leaps::regsubsets(mpg~.,mtcars,subset=1:25)
#' predict(subs,3L,mtcars[26:32,])
#' @import data.table
#' @export
################################################################################
predict.regsubsets<-function(object,d,newdata,...) {
  if (!isInt(d) || d < 1) stop("d must be a positive integer")
  response=as.character(object$call[[2]][[2]])
  #Remove the response if needed
  newdata=as.data.table(newdata)
  if (response %in% colnames(newdata)) newdata=newdata[,-response,with=FALSE]
  fr=as.formula(object$call[[2]][-2])
  m=model.matrix(fr,newdata)
  beta=coef(object,d)
  m[,names(beta)]%*%beta
}
