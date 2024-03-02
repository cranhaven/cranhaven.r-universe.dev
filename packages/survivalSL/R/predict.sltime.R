
predict.sltime <-function(object, newdata=NULL, newtimes=NULL, ...){

  if(is.null(newtimes)==FALSE){ newtimes<-sort(unique(newtimes)) }

  M <- length(object$method)

  FitVA <- vector("list", M+1)

  names(FitVA)<-c(names(object$model), "sl")

  for (i in 1:M) { FitVA[[i]] <- predict(object$models[[i]], newdata=newdata, newtimes=newtimes)$predictions }

  FitVA[[M+1]] <- matrix(0, nrow=dim(FitVA[[1]])[1], ncol=dim(FitVA[[1]])[2])

  w.sl <- object$weights$values

  for (i in 1:M) { FitVA[[M+1]]  <- FitVA[[M+1]] + w.sl[i]*FitVA[[i]] }

  if(is.null(newtimes)) {time.pred <- object$times} else {time.pred <- newtimes}

  return(list(predictions=FitVA,
              methods=c(object$methods,"sl"),
              times=time.pred))
}
