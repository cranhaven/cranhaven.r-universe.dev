#########1#########2#########3#########4#########5#########6#########7#########8
#' k-Fold Cross Validation Error Rate 
#'
#' Given a logistic regression model (via glm), or an LDA or QDA model, and a
#' number of folds k, the k-Fold CV error rate is calculated.
#'
#' @param mod A logistic regression, LDA, or QDA model
#' @param k Number of folds; by default LOOCV will be returned
#' @return The k-fold CV error rate if k is entered, otherwise the LOOCV error rate.
#' @examples
#' mtcars$am=as.factor(mtcars$am)
#' gmod=glm(am~mpg,binomial,mtcars)
#' CVerror(gmod)
#' @export
################################################################################
CVerror<-function(mod, k=nrow(stats::model.frame(mod))) {
  if (!inherits(mod,"glm")&&!inherits(mod,"lda")&&!inherits(mod,"qda")) {
    stop("Not a supported model")
  }
  if (!isInt(k)|k<1) stop("k must be an integer greater than or equal to 1")
  dat=stats::model.frame(mod)
  n=nrow(dat)
  split=sample(rep(1:k,length.out=n))
  if (!inherits(mod,"glm")) {
    resp=eval(parse(text=paste0(as.character(mod$call[3]),"$",
                                strsplit(as.character(mod$call)[2]," ")[[1]][1])))
  }
  errk=NULL
  for (i in 1:k) {
    mod2=eval(pryr::modify_call(mod$call,list(subset=which(split!=i))))
    if (inherits(mod,"glm")) {
      errk[i]=logistErrorRate(mod2,dat[split==i,])$errorRate 
    } else {
      errk[i]=mean(predict(mod2,dat[split==i,])$class!=resp[split==i])*100
    }
  }
  mean(errk)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' k-Fold Cross Validation Error Rate for KNN
#'
#' Given a dataset with predictors and a vector with responses, a number of
#' neighbors K, and a number of folds k, the k-fold CV error rate for KNN is 
#' calculated.
#'
#' @param pred A dataset with predictors
#' @param resp A vector with responses
#' @param K The number of neighborhoods to consider when performing KNN
#' @param k The number of folds
#' @return The k-fold CV error rate if k is entered, otherwise the LOOCV error rate.
#' @examples
#' mtcars$am=as.factor(mtcars$am)
#' CVerrorknn(mtcars[,c("mpg","hp")],mtcars$am)
#' @export
################################################################################
CVerrorknn<-function(pred,resp,K=1,k=nrow(pred)) {
  if (!isInt(K)|K<1) stop("K must be an integer greater than or equal to 1")
  if (!isInt(k)|k<1) stop("k must be an integer greater than or equal to 1")
  n=nrow(pred)
  split=sample(rep(1:k,length.out=n))
  errk=NULL
  for (i in 1:k) {
    kk=class::knn(pred[split!=i,,drop=F],pred[split==i,,drop=F],resp[split!=i],K)
    errk[i]=mean(kk!=resp[split==i])*100
  }
  mean(errk)
}
