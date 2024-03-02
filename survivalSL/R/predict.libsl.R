
predict.libsl <- function(object, newdata=NULL, newtimes=NULL, ...){

  if(object$library=="LIB_COXen" | object$library=="LIB_COXlasso" | object$library=="LIB_COXridge" |
     object$library=="LIB_COXaic" | object$library=="LIB_COXall"){

  group=object$group
  cov.quanti=object$cov.quanti
  cov.quali=object$cov.quali

  if(is.null(newdata))  {
    .pred.temp <- object$predictions
    .time.temp <- object$times
  }
  else {
    if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
      .coxphsurv<-survfit(object$model, newdata = newdata,se.fit = F)

      .sumcoxphsurv<-summary(.coxphsurv, times=sort(unique(object$times)))
      .pred.temp <- t(.sumcoxphsurv$surv)
      .time.temp <- .sumcoxphsurv$time
    }
    else{
      .bs=NULL
      .bin=NULL
      if(is.null(cov.quanti)==F){
        .bs <- eval(parse(text=paste("cbind(",
                                     paste("bs(newdata$",cov.quanti,",df=3)", collapse = ", ")
                                     ,")") ) )
      }
      if(is.null(cov.quali)==F){
        .bin <- eval(parse(text=paste("cbind(",  paste("newdata$", cov.quali, collapse = ", "), ")") ) )
      }
      .cov <- cbind(.bs,.bin)
      if(!(is.null(object$group))){
        .x <- cbind(newdata[,object$group], .cov, .cov * newdata[,object$group])
      }
      else{
        .x <- .cov
      }
      if(class(object$model)[1]=="coxph"){
        .lp.coxph <- predict(object$model, newdata = newdata, type="lp")
        .pred.temp <- exp(matrix(exp(.lp.coxph)) %*% t(as.matrix(-1*object$hazard)))
        .time.temp <- object$times
      }
      else{
        .lp.lasso <- predict(object$model, newx = .x)
        .pred.temp <- exp(matrix(exp(.lp.lasso)) %*% t(as.matrix(-1*object$hazard)))
        .time.temp <- object$times
      }


    }

  }

  if(!is.null(newtimes)){
    .pred.temp <- cbind(rep(1, dim(.pred.temp )[1]), .pred.temp)
    .time.temp <- c(0, .time.temp)

    idx=findInterval(newtimes,.time.temp)
    .pred=.pred.temp[,pmax(1,idx)]
    .time.temp <- newtimes
    .pred.temp=.pred
  }


  return(list(times=.time.temp, predictions=.pred.temp)) }

  if(object$library=="LIB_AFTgamma" | object$library=="LIB_AFTggamma"  | object$library=="LIB_AFTweibull"  |
     object$library=="LIB_AFTllogis" | object$library=="LIB_PHexponential"| object$library=="LIB_PHgompertz" |
     object$library=="LIB_PHspline" ){

    .flex=object$model

    if(is.null(newdata)){
      if(is.null(newtimes)){
        .predlist<-summary(.flex, type = "survival", newdata=object$data, ci = F, se=F )
        .time.temp=.predlist[[1]]$time
      }
      else{
        .predlist<-summary(.flex, type = "survival", newdata=object$data, ci = F, se=F, t=newtimes)
        .time.temp=newtimes
      }
    }
    else{
      if(is.null(newtimes)){
        .predlist<-summary(.flex, type = "survival", newdata=newdata, ci = F, se=F)
        .time.temp=.predlist[[1]]$time
      }
      else{
        .predlist<-summary(.flex, type = "survival", newdata=newdata, ci = F, se=F, t=newtimes)
        .time.temp=newtimes
      }
    }

    .pred=matrix(nrow=length(.predlist), ncol=length(.predlist[[1]]$time))

    for (i in 1:length(.predlist)){
      .pred[i,]=.predlist[[i]]$est
    }


    return(list(times=.time.temp, predictions=.pred))
  }

  if(object$library=="LIB_RSF"){
    if(is.null(newdata))  {
      .survival <- object$predictions
      .time.interest <- object$times
    }
    else {
      .pred.rf <- predict(object$model, newdata = newdata)
      .survival <- cbind(rep(1, dim(.pred.rf$survival)[1]), .pred.rf$survival)
      .time.interest <- c(0, .pred.rf$time.interest)

      idx=findInterval(object$times,.time.interest)
      .pred=.survival[,pmax(1,idx)]

      .survival <- .pred
      .time.interest <- object$times
    }


    if(!is.null(newtimes)) {
      .survival <- cbind(rep(1, dim(.survival)[1]), .survival)
      .time.interest <- c(0, .time.interest)

      idx=findInterval(newtimes,.time.interest)
      .pred=.survival[,pmax(1,idx)]

      .survival <- .pred
      .time.interest <- newtimes
    }

    return(list(times=.time.interest, predictions=.survival))
  }


  if(object$library=="LIB_SNN"){

    .times <- object$times

    if(is.null(newdata))  {
      .pred <- object$predictions
    }
    else {

      .var <- c(object$cov.quanti, object$cov.quali)

      .newdata<-newdata[,.var]

      .pred <- predict(object$model, newdata=.newdata)
      .time.deepsurv<-as.numeric(dimnames(.pred)[[2]])

      idx=findInterval(.times, .time.deepsurv)
      .pred=.pred[,pmax(1,idx)]
    }


    if(!is.null(newtimes)) {
      .pred.deepsurv <- cbind(rep(1, dim(.pred)[1]), .pred)

      .time.deepsurv <- c(0, .times)

      idx=findInterval(newtimes, .time.deepsurv)
      .pred=.pred.deepsurv[,pmax(1,idx)]

      .times <- newtimes
    }

    return(list(times=.times, predictions=.pred))
  }
}


