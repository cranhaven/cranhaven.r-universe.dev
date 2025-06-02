# Create initial model
create_TRMF = function(dataM,weight=1,normalize=c("none","standard","robust","range"),
                       normalize.type = c("global","columnwise","rowwise"),
                       na.action=c("impute","fail"),
                       scaleXm = c("no","project","track")){
  
  dataM = as.matrix(dataM)
  
  # Check NAs
  if(match.arg(na.action) == "fail"){
    if(any(is.na(dataM))){
      stop("Missing values in dataM")
    }
    if(any(is.na(weight))){
      stop("Missing values in weight")
    }
  }
  
  if(any(is.infinite(dataM))){
    stop("Infinite values in data matrix")
  }
  
  if(any(is.infinite(weight))){
    stop("Infinite values in weights")
  }
  
  # Check normalize types
  normalize = match.arg(normalize)
  normalize.type = match.arg(normalize.type)
  
  # Attributes
  Dims = list(nrows = dim(dataM)[1],ncols = dim(dataM)[2],numTS=0)
  
  # Normalize data
  normalize = match.arg(normalize)
  normalize.type = match.arg(normalize.type)
  NormalizedData = NormalizeMatrix(dataM,method=normalize,type=normalize.type)
  NormalizedData[is.na(dataM)]=0
  
  # Projection for missing values
  HadamardProjection = HadamardProjection4NA(dataM)
  
  # Get data weights
  weight[is.na(weight)]=0
  Weight = weight*HadamardProjection
  
  # Scale Xm?
  scaleXm = match.arg(tolower(scaleXm),c("no","project","track"))
  if(scaleXm != "no"){
    if(scaleXm != "project"&& scaleXm != "track"){
      stop("create_TRMF: scaleXm option not recognized")
    }
  }else{
    scaleXm = NULL
  }
  
  # Create TRMF object
  trmf_object = list(dataM = dataM,NormalizedData = NormalizedData,HadamardProjection=HadamardProjection,
                     Weight=Weight,Dims = Dims,HasXreg=FALSE,scaleXm=scaleXm)
  
  class(trmf_object) = "TRMF"
  

  
  return(trmf_object)
  
}


TRMF_columns = function(obj,reg_type =c("l2","nnls","constrain","interval","none"),lambda=0.0001,mu0=NULL){
  
  # check object
if(!inherits(obj,"TRMF")){
    stop("TRMF_columns: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(!is.null(obj$Fm_settings)){
    warning("TRMF_columns model already defined, overwriting")
  }
  
  # screen constraint type
  type = match.arg(reg_type)
  if(!(type %in%c("l2","nnls","constrain","interval","none"))){
    stop("TRMF_columns: columns regularization type not valid (at least not currently implemented)")
  }
  # verify lambda
  if(type=="none"||is.null(lambda)){
    lambda=0
  }

  if(is.null(mu0)){
    mu0 = 0
  }
  
  # update object
  obj$Fm_Settings = list(type=type,lambda=lambda,mu0=mu0)
  
  return(obj)
}

# Add slope constraint model
TRMF_trend = function(obj,numTS = 1,order = 1,lambdaD=1,lambdaA=0.0001,weight=1){
  
  # verify object
if(!inherits(obj,"TRMF")){
    stop("TRMF_trend: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(any(is.infinite(weight))){
    stop("TRMF_trend: Infinite values in weights")
  }
  
  # check inputs
  numTS = as.integer(numTS)
  if(numTS<1){
    return(obj)
  }
  
  
  if((length(lambdaD)!=1)||(length(lambdaA)!=1)){
    stop("TRMF_trend: the regularization parameters (lambda) must be scalars")
  }
  
  # list of rules for Xm
  if(is.null(obj$Xm_models)){
    xm_it = 1
    obj$Xm_models=list()
  }else{
    xm_it = length(obj$Xm_models)+1
  }
  
  
  # Check weight input, make weights have mean 1, put in matrix form
  nrows = obj$Dims$nrows
  if((length(weight)!=1)&&(length(weight)!=nrows)){
    stop("TRMF_trend: weight vector is wrong size")
  }
  
  
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  
  # Create finite difference constraint
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  Dmat = FiniteDiffM(nrows,order)
  Dmat = WeightD%*%Dmat
  
  # Overall regularization
  WeightA = diag(x=lambdaA,nrow=nrows)
  
  # Create Xm object
  XmObj = list(Rm = Dmat,WA = WeightA)
  XmObj$model =list(type = "trend",order=order,numTS=numTS)
  XmObj$model$name = paste("Order_",order," trend with ",numTS," latent time series",sep="",collapse="")
  XmObj$model$colnames = paste("D",round(order,2),"(",1:numTS,")",sep="")
  obj$Xm_models[[xm_it]] = XmObj
  obj$Dims$numTS=obj$Dims$numTS+numTS
  return(obj)
}

# No temporal structure
TRMF_simple = function(obj,numTS = 1,lambdaA=0.0001,weight=1){
  
  # verify object
if(!inherits(obj,"TRMF")){
    stop("TRMF_trend: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(any(is.infinite(weight))){
    stop("TRMF_simple: Infinite values in weights")
  }
  
  # check inputs
  numTS = as.integer(numTS)
  if(numTS<1){
    return(obj)
  }
  
  if(length(lambdaA)!=1){
    stop("TRMF_simple: the regularization parameter (lambda) must be scalar")
  }
  
  # list of rules for Xm
  if(is.null(obj$Xm_models)){
    xm_it = 1
    obj$Xm_models=list()
  }else{
    xm_it = length(obj$Xm_models)+1
  }
  
  # Check weight input, make weights have mean 1, put in matrix form
  nrows = obj$Dims$nrows
  if((length(weight)!=1)&&(length(weight)!=nrows)){
    stop("TRMF_simple: weight vector is wrong size")
  }
  
  WeightA = diag(x=lambdaA*weight,nrow=nrows)
  
  # Overall regularization
  WeightD = diag(x=0,nrow=nrows) # this is here to make consistent with other models
  
  # Create Xm object
  XmObj = list(Rm = WeightD ,WA = WeightA)
  XmObj$model =list(type = "simple",order=0,numTS=numTS)
  XmObj$model$name = paste0("L2 regularized with ",numTS," latent time series")
  XmObj$model$colnames = paste("L2(",1:numTS,")",sep="")
  obj$Xm_models[[xm_it]] = XmObj
  obj$Dims$numTS=obj$Dims$numTS+numTS
  return(obj)
}

# Add seasonal random walk model
TRMF_seasonal = function(obj,numTS = 1,freq = 12,sumFirst=FALSE,lambdaD=1,lambdaA=0.0001,weight=1){
  
  # verify object
if(!inherits(obj,"TRMF")){
    stop("TRMF_seasonal: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(any(is.infinite(weight))){
    stop("TRMF_seasonal: Infinite values in weights")
  }
  
  # check inputs
  numTS = as.integer(numTS)
  if(numTS<1){
    return(obj)
  }
  
  if(round(freq) != freq){
    message("TRMF_seasonal: Non-integer frequencies (freq) currently rounded to nearest integer")
  }
  if(freq<1){
    stop("TRMF_seasonal: freq value not valid")
  }
  if(freq==1){
    message("TRMF_seasonal: lag = freq, consider using TRMF_trend() instead")
  }
  
  if((length(lambdaD)!=1)||(length(lambdaA)!=1)){
    stop("TRMF_seasonal: the regularization parameters (lambda) must be scalars")
  }
  
  # list of rules for Xm
  if(is.null(obj$Xm_models)){
    xm_it = 1
    obj$Xm_models=list()
  }else{
    xm_it = length(obj$Xm_models)+1
  }
  
  # Check weight input, make weights have mean 1, put in matrix form
  nrows = obj$Dims$nrows
  if((length(weight)!=1)&&(length(weight)!=nrows)){
    stop("TRMF_seasonal: weight vector is wrong size")
  }
  
  
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  
  # Create finite difference constraint
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  if(sumFirst){
    Dmat = Seasonal_DM_t2(nrows,lag=freq)
  }else{
    Dmat = Seasonal_DM(nrows,lag=freq,sumFirst=sumFirst)
  }

  Dmat = WeightD%*%Dmat
  
  # Overall regularization
  WeightA = diag(x=lambdaA,nrow=nrows)
  
  # Create Xm object
  XmObj = list(Rm = Dmat,WA = WeightA)
  XmObj$model =list(type = "seasonal",freq=freq,numTS=numTS)
  XmObj$model$name = paste("Frequency = ",freq," seasonal random walk with ",numTS," latent time series",sep="",collapse="")
  XmObj$model$colnames = paste("L",freq,"(",1:numTS,")",sep="")
  obj$Xm_models[[xm_it]] = XmObj
  obj$Dims$numTS=obj$Dims$numTS+numTS
  return(obj)
}

# Add an Auto-Regressive model
TRMF_ar = function(obj,numTS = 1,AR,lambdaD=1,lambdaA=0.0001,weight=1){
  
  # verify object
if(!inherits(obj,"TRMF")){
    stop("TRMF_AR: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(any(is.infinite(weight))){
    stop("TRMF_ar: Infinite values in weights")
  }
  
  # check inputs
  numTS = as.integer(numTS)
  if(numTS<1){
    return(obj)
  }
  
  
  
  if((length(lambdaD)!=1)||(length(lambdaA)!=1)){
    stop("TRMF_AR: the regularization parameters (lambda) must be scalars")
  }
  
  # list of rules for Xm
  if(is.null(obj$Xm_models)){
    xm_it = 1
    obj$Xm_models=list()
  }else{
    xm_it = length(obj$Xm_models)+1
  }
  
  # Check weight input, make weights have mean 1, put in matrix form
  nrows = obj$Dims$nrows
  if((length(weight)!=1)&&(length(weight)!=nrows)){
    stop("TRMF_AR: weight vector is wrong size")
  }
  
  
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  
  # Create finite difference constraint
  WeightD = diag(x=lambdaD*weight,nrow=nrows)
  Amat = ARmat(nrows,AR)
  Amat = WeightD%*%Amat
  
  # Overall regularization
  WeightA = diag(x=lambdaA,nrow=nrows)
  
  
  # Create Xm object
  XmObj = list(Rm = Amat,WA = WeightA)
  XmObj$model =list(type = "auto-regressive",parms = AR,numTS=numTS)
  XmObj$model$name = paste("Auto-regressive model of order ",length(AR)," with ",numTS," latent time series",sep="",collapse="")
  XmObj$model$colnames = paste("AR",length(AR),"(",1:numTS,")",sep="")
  obj$Xm_models[[xm_it]] = XmObj
  obj$Dims$numTS=obj$Dims$numTS+numTS
  return(obj)
}

# Add a Regression model
TRMF_regression = function(obj,Xreg,type=c("global","columnwise")){
  
  # verify object
if(!inherits(obj,"TRMF")){
    stop("TRMF_Regression: Create a valid TRMF object first using create_TRMF()")
  }
  
  if(any(is.infinite(Xreg))){
    stop("TRMF_Regression: infinite values in external regressors")
  }
  
  if(any(is.na(Xreg))){
    stop("TRMF_Regression: Missing values not allowed in external regressors")
  }
  
  nrows = obj$Dims$nrows
  type = match.arg(type)
  
  dimX = dim(Xreg)
  
  # Global case, all the data time series regress off of the same external regressors
  if(type=="global"){
    
    # check dimensions
    if(is.null(dimX)){
      if(length(Xreg) != nrows){
        stop("TRMF_Regression: Xreg dimensions are incompatible with the data")
      }else{
        Xreg = matrix(Xreg,nrow=nrows)
        dimX = dim(Xreg)
      }
      
    }else{
      if(dimX[1] != nrows){
        stop("TRMF_Regression: Xreg dimensions are incompatible with the data")
      }
    }
    if(length(dimX)>2){
      stop("TRMF_Regression: Xreg has more then 2 dimensions, perhaps you meant to use type='columnwise'")
    }
    
    # only allow 1 global regression model
    if(!is.null(obj$GlobalXReg)){
      warning("TRMF_Regression: A global external regressor model has already been defined, over-writing...")
    }
    
    # store
    obj$xReg_model$GlobalXReg = Xreg
    
    # create name
    cnames = colnames(Xreg)
    if(is.null(cnames)){
      cnames = paste("gXreg(",1:dimX[2],")",sep="")
    }else{
      cnames = paste("gXreg(",cnames,")",sep="")
    }
    obj$xReg_model$gxname = cnames
    
    
  }else if(type=="columnwise"){   # local case, each data time series gets its own regressor
    
    
    # check dimensions
    if(is.null(dimX)){
      stop("TRMF_Regression: Xreg is not matrix or array, perhaps you meant to use type='global'")
    }
    if(dimX[1] != nrows){
      stop("TRMF_Regression: number of rows of Xreg do not match number of rows of data")
    }
    if(dimX[2] != obj$Dims$ncols){
      stop("TRMF_Regression: number of columns of Xreg do not match number of columns of data")
    }
    
    # only allow 1 column-wise regression model (can implement more later)
    if(!is.null(obj$ColumnwiseXReg)){
      warning("TRMF_Regression: A columnwise external regressor model has already been defined, over-writing...")
    }
    
    # store as array
    if(is.na(dimX[3])){dimX[3]=1}
    obj$xReg_model$ColumnwiseXReg = array(Xreg,dimX)
    
    # create names
    obj$xReg_model$cxname = paste("cXreg(",1:dimX[3],")",sep="")

  }
  
  return(obj)
}

# Fit TRMF model
train.TRMF = function(x,numit=10,Xm=NULL,Fm=NULL,Z=NULL,...){
  obj = x
  # check object to see if it valid object
if(!inherits(obj,"TRMF")){
    # should never get here...
    stop("Not a valid TRMF object")
  }
  
  
  # if no models have been added, create a simple one
  if(is.null(obj$Fm_Settings)){
    obj = TRMF_columns(obj)
  }
  
  if(is.null(obj$Xm_models)){
    obj = TRMF_simple(obj)
  }
  
  # Set up stuff
  ptr = list2env(obj,envir = new.env())
  Create_Xreg_Stuff(ptr)
  Create_Fm_Stuff(ptr)
  Create_Xm_Stuff(ptr)
  Create_Z_Stuff(ptr)
  
  # Add in Z
  if(!is.null(Z)){
    Z = c(Z)
    if(length(Z) != length(ptr$Factors$Z)){
      stop("train.TRMF: provided Z is the wrong length")
    }
    ptr$Factors$Z = Z
  }
  
  # If nothing, have to initialize
  if(is.null(Xm)&&is.null(Fm)){
    fit_xm_first = FALSE
    InitializeALS(ptr)
    FitXm(ptr)
  }
  
  # If Xm or Fm provided, check and put in
  if(!is.null(Xm)){
    if(!is.null(Fm)){
      warning("train.TRMF: both Xm and Fm are provided. Fm will be over-written")
    }
    fit_xm_first = FALSE
    if(any(dim(Xm) != dim(ptr$Factors$Xm))){
      stop("train.TRMF: provided Xm is the wrong size")
    }
    ptr$Factors$Xm = Xm
  }else if(!is.null(Fm)){
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

impute_TRMF= function(obj){
  
  # check object to see if it valid object
if(!inherits(obj,"TRMF")){
    stop("Not a valid TRMF object")
  }
  
  # Make sure everything we need is there
  if(is.null(obj$Fit)){
    stop("Train TRMF model first using 'train_TRMF()'")
  }
  
  # find missing values
  newM = obj$dataM
  ind = which(is.na(newM))
  newM[ind] = obj$Fit$fitted[ind]
  
  return(newM)
  
}

