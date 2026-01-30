#########1#########2#########3#########4#########5#########6#########7#########8
#' Impute Missing Values 
#'
#' Replace missing values in a vector using a function (by default the mean) on 
#' this vector.
#'
#' @param x A numeric vector
#' @param fn A function to apply to all values in the vector \code{x}
#' @param ... Additional arguments to be passed to function \code{fn}
#' @return Vector \code{x} with all missing values replaced
#' @examples
#' v1=c(2,5,3,NA,2,4,1,NA)
#' #Replace values with the mean
#' impNA(v1,na.rm=TRUE)
#' #Replace values with the minimum
#' impNA(v1,min,na.rm=TRUE)
#' @export
################################################################################
impNA <- function(x,fn=mean,...) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be a numeric vector")
  if (!inherits(fn,"function")) stop("fn must be a function")
  x[is.na(x)]=fn(x,...)[1]
  x
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate the Error Rate and Results Table for Logistic Regression Models 
#'
#' Calculate the testing error rate for a dataset on a logistic regression 
#' model (or the training error rate if no dataset is entered), and a results 
#' table with responses versus predicted responses.
#'
#' @param gmod A logistic regression model
#' @param nw A dataset for which a testing error rate should be calculated 
#' using the model in \code{gmod}. Note that it must contain the predictors as
#' well as the responses. If this argument is NULL (the default) the training
#' error rate will be calculated.
#' @param p Probability (default .5) above which the observation is assigned to
#' the second level of the response.
#' @return List with training error rate if \code{nw} is NULL, testing error
#' rate otherwise, and a results table with responses versus predicted 
#' responses.
#' @examples
#' gmod=glm(state~.,binomial,Puromycin)
#' logistErrorRate(gmod)
#' @export
################################################################################
logistErrorRate<-function(gmod,nw=NULL,p=.5) {
  if (!inherits(gmod,"glm")) 
    stop("Argument must be a logistic regression model")
  if (as.character(gmod$call)[3]!="binomial") stop("Model must be binomial")
  #Testing error rate
  if (!is.null(nw)) {
    if (!inherits(nw,"data.frame")) stop("nw must be a data frame type object")
    response=strsplit(as.character(gmod$call)[2]," ")[[1]][1]
    if (!(response %in% colnames(nw))) 
      stop("nw must contain the response variable")
    prob=predict(gmod,nw,"r")   #Responses from the test set
    m=nrow(nw)
    preds=rep(levels(gmod$model[,1])[1],m)
    preds[prob>p]=levels(gmod$model[,1])[2]
    preds=factor(preds,levels=levels(gmod$model[,1]))
    return(list(
      errorRate=eval(parse(text=paste0("mean(preds!=nw$",response,")*100"))),
      result=stats::addmargins(eval(parse(
        text=paste0("table(predicted=preds,response=nw$",response,")"))))))
  }
  #Training error rate
  n=nrow(gmod$model)
  preds=rep(levels(gmod$model[,1])[1],n)
  preds[fitted(gmod)>p]=levels(gmod$model[,1])[2]
  preds=factor(preds,levels=levels(gmod$model[,1]))
  list(errorRate=mean(preds!=gmod$model[,1])*100,
       result=stats::addmargins(table(fitted=preds,response=gmod$model[,1])))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot the ROC curve  
#'
#' Plot the ROC curve for logistic regression, LDA, or QDA models.
#'
#' @param mod A logistic regression, LDA, or QDA model
#' @param nw A dataset for which a testing ROC curve should be plotted 
#' using the model in \code{mod}. Note that it must contain the predictors as
#' well as the responses. If this argument is NULL (the default) the training
#' ROC curve will be plotted.
#' @return A plot with the ROC curve will be produced, nothing is returned.
#' @examples
#' gmod=glm(state~.,binomial,Puromycin)
#' ROCcurve(gmod)
#' @export
################################################################################
ROCcurve<-function(mod,nw=NULL) {
  if (!inherits(mod,c("glm","lda","qda"))) stop("Not a supported model")
  if (!is.null(nw)) {
    if (!inherits(nw,"data.frame")) stop("nw must be a data frame type")
    resp=strsplit(as.character(mod$call)[2]," ")[[1]][1]
    if (!(resp %in% colnames(nw))) stop("nw must contain the response variable")
    response=eval(parse(text=paste0("nw$",resp)))
  }
  if (inherits(mod,"glm")) {
    if (is.null(nw)) {
      prob=fitted(mod)
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw,"r")
  } else {
    if (is.null(nw)) {
      prob=predict(mod)$posterior[,2]
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw)$posterior[,2]
  }
  pr=ROCR::prediction(prob,response)
  plot(ROCR::performance(pr,"tpr","fpr"),
       main=c(paste("ROC curve for",class(mod)[1],"on",mod$call$data),
              deparse(mod$call[[2]])))
  auc=ROCR::performance(pr,"auc")@y.values[[1]]
  graphics::text(.5,.5,paste0("AUC = ",round2(auc,3)))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' KNN ROC curve
#'
#' Plot the ROC curve for a KNN model. Note that it can only be used when
#' the response is dichotomous.
#'
#' @param mod The output of the knn function, run with prob=TRUE
#' @param response A vector with responses for the testing dataset used to run
#' the knn function.
#' @return A plot with the ROC curve will be produced, nothing is returned.
#' @examples
#' yhat=class::knn(Puromycin[,c("conc","rate")],Puromycin[,c("conc","rate")],
#'          Puromycin$state,10,prob=TRUE)
#' ROCknn(yhat,Puromycin$state)
#' @export
################################################################################
ROCknn<-function(mod,response) {
  if (!inherits(mod,"factor")) stop("Not a supported model")
  if (is.null(attributes(mod)$prob)) stop("No probabilities")
  if (length(levels(mod))!=2) stop("Must have 2 levels")
  prob=attributes(mod)$prob
  prob[mod==levels(mod)[1]]=1-prob[mod==levels(mod)[1]]
  pr=ROCR::prediction(prob,factor(response,ordered=T))
  plot(ROCR::performance(pr,"tpr","fpr"),main="ROC curve for KNN")
  auc=ROCR::performance(pr,"auc")@y.values[[1]]
  graphics::text(.5,.5,paste0("AUC = ",round2(auc,3)))
}
