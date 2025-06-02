summary.TRMF = function(object,...){
  cat("------------------------------------------------------------------\n")
  if(is.null(object$Fit)){
    cat("  An untrained TRMF model\n")
  }else{
    varTot = sd(object$dataM,na.rm=TRUE)^2
    varResid = sd(object$Fit$resid,na.rm=TRUE)^2
    R2 = round(100*(1-varResid/varTot),2)
    cat("  A trained TRMF model that explains",R2,"% of the variance \n")
    if(length(object$Weight)>1 & any(object$Weight!=1)){
      
      sumW = sum(object$Weight)
      wmean = sum(object$Weight*object$dataM,na.rm=TRUE)/sumW
      zero_wmean = object$dataM-wmean
      wvarTot = sum((object$Weight*zero_wmean)^2,na.rm=TRUE)/sumW^2
      
      wmean = sum(object$Weight*object$Fit$resid,na.rm=TRUE)/sumW
      zero_wmean = object$Fit$resid-wmean
      wvarResid = sum((object$Weight*zero_wmean)^2,na.rm=TRUE)/sumW^2
      
      wR2 = round(100*(1-wvarResid/wvarTot),2)
     cat("                        ... and",wR2,"% of the weighted variance \n")
    }
  }
  cat("------------------------------------------------------------------\n")
  if(!is.null(object$Fm_Settings)){
    cat("Fm = {")
    cat("  Column regularization type:",object$Fm_Settings$type," with lambda =",object$Fm_Settings$lambda," }\n")
  }
  cat("\nXm = {")
  cat("  (",length(object$Xm_models),") time series models with a total of (",sep="")
  cat(object$Dims$numTS,") latent time series:\n",sep="")
  for(k in 1:length(object$Xm_models)){
    cat("       ->",object$Xm_models[[k]]$model$name,"\n")
  }
  cat("      ...}\n\n")
  if(!is.null(object$xReg_model)){
    cat("xReg = { External regressors: \n")
    if(!is.null(object$xReg_model$ColumnwiseXReg)){
      numRegs = dim(object$xReg_model$ColumnwiseXReg)[3]
      cat("        -> Column-wise regression model with (",numRegs,") regressors for each data column \n",sep="")
    }
    if(!is.null(object$xReg_model$GlobalXReg)){
      numRegs = dim(object$xReg_modeL$GlobalXReg)[2]
      if(is.null(numRegs)){ numRegs=1}
      cat("        -> Global regression model with (",numRegs,") regressors \n",sep="")
    }
    cat("       ...}\n")
   }
}

plot.TRMF= function(x, ...){
  dots = list(...)
  if(is.null(x$Fit)){
    if(is.null(dots$col)){
      col="black"
    }else{
      col=dots$col
    }
    ts.plot(x$dataM,col=col,ylab="dataM")

  }else{
    if(is.null(dots$col)){
      temp_cfun = colorRampPalette(c("black","#00007F", "blue", "cyan4","green4","yellow3","red2","saddlebrown"))
      n = dim(x$Factors$Xm)[2]
      col=temp_cfun(n)
    }else{
      col=dots$col
    }
    ts.plot(x$Factors$Xm,col=col,ylab="Latent Time Series")
  }

}

residuals.TRMF = function(object,...){
  if(is.null(object$Fit)){
    print("TRMF model has not been fitted yet")
  }else{
    return(object$Fit$resid)
  }
}

fitted.TRMF = function(object,impute=FALSE,...){
  if(is.null(object$Fit)){
    print("TRMF model has not been fitted yet")
  }else{
    fit = object$Fit$fitted
    if(!impute){
      fit[is.na(object$dataM)]=NA
    }
    return(fit)
  }
}

coef.TRMF = function(object, ...){
  if(is.null(object$Fit)){
    print("TRMF model has not been fitted yet")
  }else{
    return(t(object$Factors$Fm))
  }
}

predict.TRMF = function(object, newdata=NULL, ...){
 # dots = list(...)
  # Check object
  if(is.null(object$Fit)){
    print("TRMF model has not been fitted yet")
  }else if(is.null(newdata)){
    #warning("Didn't find newdata, returning fitted values.")
    return(object$Fit$fitted)
  }
  
  # Check Xm matrix
  Xm = newdata$Xm
  if(is.null(Xm)){
    stop("newdata doesn't contain new Xm matrix")
  }
  Xm = as.matrix(Xm)
  nrow = dim(Xm)[1]
  ncol = dim(Xm)[2]
  if(object$Dims$numTS != ncol){
    stop("Xm matrix in newdata has a different number of columns than model")
  }
  
  # Check Regressions
  gXreg = NULL
  cXreg = NULL
  if(!is.null(object$xReg_model)){
    
    if(!is.null(object$xReg_model$GlobalXReg)){
      
      # Check global regression
      gXreg = newdata$gXreg
      if(is.null(gXreg)){
        stop("External global regressor (gXreg) in newdata is missing")
      }
      gXreg = as.matrix(gXreg)
      if(dim(gXreg)[1] != nrow){
        stop("Number of rows of new external global regressor (gXreg) doesn't match new Xm ")
      }
      if(dim(gXreg)[2] != dim(object$xReg_model$GlobalXReg)[2]){
        stop("Number of columns of new external global regressor (gXreg) doesn't match model gXreg")
      }
    }

    # Check local regression
    if(!is.null(object$xReg_model$ColumnwiseXReg)){
      if(is.null(newdata$cXreg)){
        stop("External columnwise regressor (cXreg) in newdata is missing")
      }
      cXreg = as.array(newdata$cXreg)
      if(dim(cXreg)[3] != dim(object$xReg_model$ColumnwiseXReg)[3]){
        stop("Third dimension of (cXreg) doesn't match that of the model")
      }
      if(dim(cXreg)[1] != nrow){
        stop("Number of rows of new external local regressor (cXreg) doesn't match new Xm ")
      }
      if(dim(cXreg)[2] !=  dim(object$dataM)[2]){
        stop("Number of columns of new external local regressor (cXreg) doesn't match that of the model")
      }
    }
  }
  
  # Roll in the new predictors...
  nCol = dim(object$dataM)[2]
  pred = matrix(0,nrow,nCol)
  for(k in 1:nCol){
    newX = cbind(cXreg[,k,],gXreg,Xm)
    pred[,k] = newX%*%object$Factors$Fm[,k]
  }
  
  # Scale back.. 
  Shift = attr(object$NormalizedData,"shift")
  Scale = attr(object$NormalizedData,"scale")
  Type = attr(object$NormalizedData,"scale_type")
  nPred = unScale(pred,type=Type,Scale=Scale,Shift=Shift) 
  colnames(nPred) = colnames(object$dataM)
  return(nPred)
}

components.TRMF = function(object,XorF=c("Xm","Fm","Z","Fm_each"),...){
  XorF = match.arg(tolower(XorF),c("xm","fm","z","fm_each"))
  if(is.null(object$Fit)){
    print("TRMF model has not been fitted yet")
  }else{
    if(XorF == "xm"){
      return(object$Factors$Xm)
    }
    if(XorF == "fm"){
      return(object$Factors$Fm)
    }
    if(XorF == "z"){
      return(object$Factors$Z)
    }
    if(XorF == "fm_each"){
      fm_list = list()
      if(!is.null(object$xReg_model)){
        if(!is.null(object$xReg_model$cXregInd)){
          fm_list$Fm_cXreg = object$Factors$Fm[object$xReg_model$cXregInd,] 
        }
        if(!is.null(object$xReg_model$gXregInd)){
          fm_list$Fm_gXreg = object$Factors$Fm[object$xReg_model$gXregInd,] 
        }
        fm_list$Fm_ts = object$Factors$Fm[object$xReg_model$XmInd,]
      }else{
        fm_list$Fm_ts = object$Factors$Fm
      }
      return(fm_list)
    }
  }
  return(NULL)
}

retrain = function(obj,numit,fit_xm_first=TRUE,Xm=NULL,Fm=NULL,Z=NULL){
  
  ptr = list2env(obj,envir = new.env())
  if(is.null(obj$Factors)||is.null(obj$Fm_Settings$Constraints)||is.null(obj$Xm_Settings$RHS)){
    stop("retrain: train.TRMF must be called before retrain to set up calculations")
  }
  
  # Add in Z
  if(!is.null(Z)){
    Z = c(Z)
    if(length(Z) != length(ptr$Factors$Z)){
      stop("train.TRMF: provided Z is the wrong length")
    }
    ptr$Factors$Z = Z
  }
  
  # If Xm or Fm provided, check and put in
  if(!is.null(Xm)){
    if(any(dim(Xm) != dim(ptr$Factors$Xm))){
      stop("train.TRMF: provided Xm is the wrong size")
    }
    ptr$Factors$Xm = Xm
  }
  else if(!is.null(Fm)){
    fit_xm_first = TRUE
    if(any(dim(Fm) != c(ptr$Dims$total_coef,ptr$Dims$ncols))){
      stop("train.TRMF: provided Fm is the wrong size")
    }
    ptr$Factors$Fm = Fm
  }
  
  
  if(fit_xm_first){
    step =c(expression(FitXm(ptr)),expression(FitFm(ptr)))
  }else{
    step =c(expression(FitFm(ptr)),expression(FitXm(ptr)))
  }
  
  for(k in 1:numit){
    Get_XReg_fit(ptr)
    eval(step[1])
    eval(step[2])
  }
  
  # get the fit
  FitAll(ptr)
  
  # format back as object
  newobj=as.list(ptr)
  class(newobj) = class(obj)
  return(newobj)
}

