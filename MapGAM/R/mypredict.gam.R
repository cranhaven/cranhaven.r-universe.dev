#***********************************************************************************
#
# Prediction for GAM Fits
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
mypredict.gam <- function(object, newdata, se.fit=FALSE, type=c("all","spatial"), reference="median",
                          level=0.05, verbose=FALSE)
{
  type = type[1]
  if(all(is.na(match(type,c("spatial","all"))))) 
    stop("type should be either 'spatial' or 'all'")
  
  ## Check whether newdata has geolocation variables 
  X.names = names(object$data)
  coords.name = colnames(object$smooth.frame[[1]])
  if(missing(newdata)) 
    newdata = object$data    
  else{
    if(length(as.integer(dim(newdata))[1])==0) 
      stop("newdata should include at least two geolocation observations")
    index = match(coords.name, names(newdata))
    if(any(is.na(index))) 
      stop("The two spatial variables in newdata are not consistent with the original data")
  }
  terms = terms(object)
  tt = delete.response(terms)
  
  ## Check reference type
  if(!is.element(reference[1],c("none","median","mean"))){
    ref.name = names(reference)
    if(type=="spatial"){
       ref.index = match(coords.name,ref.name)
      if(any(is.na(ref.index)))
        stop("reference should be 'none','median' or 'mean' or a data frame that have the same spatial variables with the original data")
      ref.X = reference[ref.index]
    }else{
      mf.ref = model.frame(tt,rbind(reference,reference),xlev = object$xlevels)
      ref.X = model.matrix(tt,mf.ref)
      ref.index = match(names(coef(object)),colnames(ref.X))
      if(any(is.na(ref.index)))
        stop("reference should be 'none','median' or 'mean' or a data frame that have the same variables with the original data")
      ref.X = ref.X[1,]
      ref.offset = 0
      if (!is.null(off.num <- attr(tt, "offset"))) 
        for (i in off.num) ref.offset <- ref.offset + eval(attr(tt, 
                                                        "variables")[[i + 1]], reference)
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
    if(type=="all" & verbose) cat("GAM predictions will use median values at all grid points 
       for variables not included in newdata\n")
  }
  mf.new = model.frame(tt,newdata,xlev=object$xlevels)
  newdata.matrix = model.matrix(tt, mf.new)
  
  ### offsets
  offset = rep(0, nrow(newdata))
  if(type == 'spatial'){
     if (!is.null(off.num <- attr(tt, "offset"))) 
        for (i in off.num) offset = offset + eval(attr(tt, 
                                                    "variables")[[i + 1]], newdata)
      if (!is.null(object$call$offset)) 
      offset = offset + eval(object$call$offset, newdata)
     if(!is.element(reference[1],c("none","median","mean"))) offset = c(offset,ref.offset)
  }
  ## Make predictions  
  order = attr(tt,"specials")$lo
  coords.order = 2:3
  labels = names(mf.new)
  factor.labels = names(object$xlevels)
  if(order>1)
    for (i in 1:(order-1)){
      if(!is.null(factor.labels))
         if(labels[i] %in% factor.labels)
        coords.order = coords.order + length(object$xlevels[[labels[i]]])-2
      coords.order = coords.order + 1
    }
  span = 0.5; degree = 1;
  los = attr(tt,"term.labels")[order]
  los = gsub("[[:blank:]]","",los)
  start = gregexpr("\\(",los)[[1]]
  end =rev(gregexpr("\\)",los)[[1]]) 
  losub = substr(los,start[1]+1,end[1]-1)
  parts = unlist(strsplit(losub,"\\([^()]*\\)(*SKIP)(*F)|\\h*,\\h*", perl=T))
  if(length(parts)>2) eval(parse(text=paste(parts[-(1:2)],collapse=";")))
  
  R = object$smooth+object$residuals
  mat = newdata.matrix
  if(!is.element(reference,c("none","median","mean"))){
    if(type=="all")  mat = rbind(newdata.matrix,ref.X)
    if(type == "spatial"){
      mat = rbind(newdata.matrix,0)
      mat[N.new+1,coords.order] = ref.X
    }
  }
  lo.fit = gam.lo(as.matrix(object$smooth.frame),R,object$weights,span, degree,2,mat[,coords.order])
  pred.index = if(type=="spatial") coords.order else 1:dim(mat)[2]
  pred = mat[,pred.index]%*%object$coef[pred.index]+lo.fit
  if(type=="all") pred = pred + offset
  if(reference=="median"|reference =="mean")
    ref = get(reference,mode="function",envir=parent.frame())(pred,na.rm=TRUE)
  else{
    if(reference=="none") ref = 0
    else ref = pred[N.new+1]
  } 
  pred = pred[1:N.new]
  
  
  rslt = list(pred=pred-ref,reference=ref,reference.type=reference,predict.type = type)
  if(!se.fit) return(rslt)
  ## Refit 
  Y = object$y
  X = model.matrix(object)
  X.offset = object$offset  
  if(is.null(offset)) offset = rep(0, nrow(X))
  
  
  XX = object$smooth.frame[[1]][,1]
  XY = object$smooth.frame[[1]][,2]
  family = object$family
  variance = family$variance
  dev.resids = family$dev.resids
  mu.eta = family$mu.eta
  linkfun = family$linkfun
  linkinv = family$linkinv
  valideta = family$valideta
  if (is.null(valideta)) 
    valideta = function(eta) TRUE
  validmu = family$validmu
  if (is.null(validmu)) 
    validmu = function(mu) TRUE
  N = length(Y)
  mu = mean(Y)*rep(1,N)
  eta = linkfun(mu)
  w = rep(1,N)
  L.old = 99999
  
  for(i in 1:40){
    mu.eta.val = mu.eta(eta)
    varmu = variance(mu)
    good = w>0&(mu.eta.val!=0)
    Z = eta
    Z[good] = eta[good] + (Y-mu)[good]/mu.eta.val[good]
    w[good] = mu.eta.val[good]^2/varmu[good]
    residuals = Z
    fit = list(fitted.values=rep(0,N),coefficients=0)
    s = rep(0,N)
    ## Backfitting starts
    for (j in 1:40){
      deltaf = 0
      R = residuals + fit$fitted.values 
	    fit = lm(R~X, weights=w, offset=X.offset)
      residuals = fit$residuals
      old = s
      R = residuals + s
      smooth.fit = loess(R~XX+XY,weights=w,span=span, degree=degree,ncols=2,normalize=FALSE)
      s = smooth.fit$fitted-mean(smooth.fit$fitted)
      residuals = R-s
      deltaf = deltaf + weighted.mean((s - old)^2,w)
      RATIO = sqrt(deltaf/sum(w * sum(s)^2))
      if (RATIO <= 1e-5) break;
    }
    ## Backfitting ends
    eta = Z-residuals
    mu = linkinv(eta)
    L.new = sum(dev.resids(Y, mu, w))
    if(abs(L.old - L.new)/(L.old + 0.1) <=1e-6) break;
    L.old = L.new
  }
  if (i==40) 
    warning("mypredict.gam did not converge for se.fit=TRUE; predictions may not be reliable.")
  
  ### Prediction with confidence intervals
  pred.s = predict(smooth.fit,data.frame(XX=newdata.matrix[,coords.order[1]],XY=newdata.matrix[,coords.order[2]]),se=TRUE)
  if (object$family$family %in% c("poisson", "binomial")){ 
    dispersion <- 1
  }else{
    if (any(fit$weights == 0)) 
      warning("observations with zero weight not used for calculating dispersion")
    dispersion <- sum((fit$weights * fit$residuals^2)[fit$weights > 0])/fit$df.residual
  }
  rslt$residual.scale <- as.vector(sqrt(dispersion))
  cov.mat = vcov(fit,complete=FALSE)[pred.index,pred.index]
  if(type=="all")
    csdata = newdata.matrix
  else
    csdata = newdata.matrix[,pred.index] - as.matrix(rep(1,nrow(newdata.matrix)))%*%apply(X[,pred.index],2,mean)
  var.l = rowSums((csdata%*%cov.mat)*csdata)
  sde = sqrt(var.l + rslt$residual.scale^2*pred.s$se.fit^2)
  rslt$conf.low = rslt$pred + qnorm(level/2)*sde; rslt$conf.high=rslt$pred+qnorm(1-level/2)*sde
  rslt$se = sde
  rslt$level = level
  return(rslt)
}

