#***********************************************************************************
#
# Prediction Method for gamcox() Fits
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
predict.gamcox <- function(object, newdata=object$data, se.fit=FALSE, type=c("spatial","all"),
                           reference="median", level=0.05, verbose=FALSE,...)
{
  type=type[1]
  if(all(is.na(match(type,c("spatial","all"))))) 
    stop("type should be either 'spatial' or 'all'")
  
  ## check whether newdata has geolocation variables 
  X.names = names(object$data)
  coords.name = colnames(object$smooth.frame)
  if(length(as.integer(dim(newdata))[1])==0) 
    stop("newdata should include at least two geolocation observations")
  index = match(coords.name, names(newdata))
  if(any(is.na(index))) 
    stop("The two spatial variables in newdata are not consistent with the original data")
  
  terms = object$term
  tt = delete.response(terms) 
  
  ## check reference type 
  if(!is.element(reference[1],c("median","mean"))){
    ref.name = names(reference)
    if(type=="spatial"){
      ref.index = match(coords.name,ref.name)
      if(any(is.na(ref.index)))
        stop("reference should be 'median' or 'mean' or a data frame that have the same spatial variables with the original data")
      ref.X = reference[ref.index]
    }else{
      mf.ref = model.frame(tt,rbind(reference,reference),xlev = object$xlevels)
      ref.X = model.matrix(tt,mf.ref)
      ref.index = match(names(coef(object)),colnames(ref.X))
      if(any(is.na(ref.index)))
        stop("reference should be 'none','median' or 'mean' or a data frame that have the same variables with the original data")
      ref.X = ref.X[1,-1]
    }
  }
  
  ## Fill in newdata
  N.new = dim(newdata)[1]
  og.new = dim(newdata)[2]
  factor.name = names(object$xlevels)
  sub = grep("factor\\([[:print:]]+\\)",factor.name)
  for(i in sub){
    start = gregexpr("\\(",factor.name[i])[[1]]
    end =rev(gregexpr("\\)",factor.name[i])[[1]]) 
    factor.name[i] = substr(factor.name[i],start[1]+1,end[1]-1)
  }
  addvars = X.names[!(X.names %in% names(newdata))]
  if (length(addvars) > 0) {
    newdata = cbind(newdata,matrix(0,N.new,length(addvars)))
    names(newdata)[og.new+1:length(addvars)]=addvars
    for (i in addvars) {
      if (i %in% factor.name | is.factor(object$data[,i])) 
        newdata[,i] = rep(factor(names(which.max(table(as.factor(object$data[,i])))), levels = levels(as.factor(object$data[, i]))),N.new)
      else 
        if(is.numeric(object$data[1,i]))
          newdata[,i] = rep(median(object$data[, i]),N.new)
    }
    if(verbose&type=="all") if(type=="all"&verbose) cat("GAM predictions will use median values at all grid points 
                                for variables not included in newdata\n")
  }
  
  mf.new = model.frame(tt,newdata,xlev=object$xlevels)
  newdata.matrix = model.matrix(tt, mf.new)[,-1]
  
  ## Make predictions
  order = attr(tt,"specials")$lo
  coords.order = 1:2
  labels = names(mf.new)
  factor.labels = names(object$xlevels)
  if(order>1)
    for (i in 1:(order-1)){
      if(!is.null(factor.labels))if(labels[i]%in%factor.labels)
        coords.order = coords.order + length(object$xlevels[[labels[i]]])-2
      coords.order = coords.order + 1
    }

    mat = newdata.matrix
  if(!is.element(reference[1],c("median","mean"))){
    if(type=="all")  mat = rbind(newdata.matrix,ref.X)
    if(type == "spatial"){
      mat = rbind(newdata.matrix,0)
      mat[N.new+1,coords.order] = as.matrix(ref.X)
    }
  }
  XX = object$smooth.frame[,1]
  XY = object$smooth.frame[,2] 
  z = object$smooth+object$residuals
  smooth.fit <- loess(z~XX+XY,weights=object$weights, span=object$span, degree=object$degree,normalize=FALSE,control=loess.control(trace.hat=object$loess.trace))	
  pred.object <- predict(smooth.fit,data.frame(XX=mat[,coords.order[1]],XY=mat[,coords.order[2]]),se=se.fit)

  pred.index = if(type=="spatial") coords.order else 1:dim(mat)[2]
  pred.s = if(se.fit)pred.object$fit else pred.object
  pred.l <- mat[,pred.index]%*%object$coefficients[pred.index]
  
  pred = pred.l+pred.s
  if(reference[1]=="median"|reference[1] =="mean")
    ref = get(reference,mode="function",envir=parent.frame())(pred,na.rm=TRUE)
  else ref = pred[N.new+1]
  pred = pred[1:N.new]
  if(!se.fit){
    return(list(pred=pred-ref,reference=ref,reference.type=reference,predict.type=type))
  }else{
  ## Confidence intervals
      rslt <- list(pred=pred-ref)
      cov.mat <- object$cov[pred.index,pred.index]
      csdata <- newdata.matrix - as.matrix(rep(1,N.new))%*%apply(object$X,2,mean)
      csdata <- csdata[,pred.index]
      var.l <- rowSums((csdata%*%cov.mat)*csdata)
    sde <- sqrt(var.l + pred.object$se.fit[1:N.new]^2)
    rslt=list(pred=pred-ref)
    rslt$conf.low <- rslt$pred + qnorm(level/2)*sde; rslt$conf.high=rslt$pred+qnorm(1-level/2)*sde
    rslt$se <- sde
  }
  rslt$level <- level
  rslt$reference = ref
  rslt$reference.type=reference
  rslt$predict.type=type
  return(rslt)
}