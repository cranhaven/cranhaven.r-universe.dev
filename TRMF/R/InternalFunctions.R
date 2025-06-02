# Various internal functions

# Create structures for eXternal Regressors
Create_Xreg_Stuff =  function(ptr){
  if(!is.null(ptr$xReg_model)){
    ptr$xReg_model$XmInd = 1:ptr$Dims$numTS
    
    # get size of column-wise regressors
    if(!is.null(ptr$xReg_model$ColumnwiseXReg)){
      numCXR = dim(ptr$xReg_model$ColumnwiseXReg)[3]
      cXregInd = 1:numCXR
    }else{
      numCXR = 0
      cXregInd = NULL
    }
    
    # get size of external global regressors
    if(!is.null(ptr$xReg_model$GlobalXReg)){
      numGXR = dim(ptr$xReg_model$GlobalXReg)[2]
      gXregInd = numCXR + 1:numGXR
    }else{
      numGXR = 0
      gXregInd = NULL
    }

    numReg = numGXR+numCXR
    ptr$xReg_model$numReg = numReg
    ptr$xReg_model$Shift = 0*ptr$Weight
    ptr$xReg_model$XregInd = 1:numReg
    ptr$xReg_model$gXregInd = gXregInd
    ptr$xReg_model$cXregInd = cXregInd
    ptr$xReg_model$XmInd = numReg+ptr$xReg_model$XmInd
    ptr$xReg_model$colnames = c(ptr$xReg_model$cxname,ptr$xReg_model$gxname)
    ptr$Dims$total_coef = numReg+ptr$Dims$numTS
  }else{
    ptr$Dims$total_coef = ptr$Dims$numTS 
  }

  # object not returned, uses environment for pointer like behavior
}

# Create constraint matrices for Fm
Create_Fm_Stuff= function(ptr){

  # Get sizes
  totNum = ptr$Dims$numTS
  if(!is.null(ptr$xReg_model)){
    totNum = totNum + ptr$xReg_model$numReg
  }
  
  Constraints = list()
  Constraints$totNum = totNum

  # Create L2 regularization
  lambda = ptr$Fm_Settings$lambda
  if(length(lambda) == 1){
    Constraints$LambdaI = diag(x=lambda,nrow = totNum)
  }else if(length(lambda) == totNum){
    Constraints$LambdaI = diag(c(lambda))
  }else if(is.matrix(lambda)){
    if(!all(dim(lambda) == totNum)){
      stop("TRMF_columns: lambda is an incorrect shape")
    }
    Constraints$LambdaI = lambda
  }
  
  mu0 = ptr$Fm_Settings$mu0
  if(length(mu0) == 1){
    rhs0 = rep(mu0,totNum)
  }else if(length(mu0) == totNum){
    rhs0 = c(mu0)
  }else{
    stop("TRMF_columns: mu0 is an incorrect shape")
  }
  Constraints$rhs0 = Constraints$LambdaI%*%rhs0
 
  # set up constraints
  if(ptr$Fm_Settings$type =="constrain"){
    Constraints$E = matrix(1,1,totNum)
    Constraints$G =  diag(totNum)
    Constraints$H =  rhs0
  }else if(ptr$Fm_Settings$type =="interval"){
    ones = rep(1,totNum)
    Constraints$G =  rbind(diag(x=1,nrow=totNum),diag(x=-1,nrow=totNum))
    Constraints$H =  c(rhs0,-ones)
  }

  ptr$Fm_Settings$Constraints = Constraints
  ptr$Factors$Fm = matrix(0,nrow=totNum,ncol = ptr$Dims$ncols )

  # object not returned, uses environment for pointer like behavior
}

# Create constraint matrices for Xm
Create_Xm_Stuff= function(ptr){
  # Get sizes
  numTS = ptr$Dims$numTS
  numRows = ptr$Dims$nrows
  N  = numTS*numRows
  numModels = length(ptr$Xm_models)
  cnames=NULL
  
  # loop through and add constraint matrices, create overall regularization matrix
  list_LHS = list()
  for(k in 1:numModels){
    xmod = ptr$Xm_models[[k]]
    ModelConstraint = crossprod(xmod$Rm)+crossprod(xmod$WA)
    num_ts = xmod$model$numTS
    list_LHS[[k]] = Matrix::Diagonal(n=num_ts)%x%ModelConstraint
    
    # build model names
    cnames = c(cnames,paste(paste0("M[",k,"]:"),xmod$model$colnames,sep=""))
  }
  
  # Add fields
  ptr$Factors$Xm = matrix(0,nrow=numRows,ncol = numTS)
  ptr$Xm_Settings$ConstraintM = Matrix::.bdiag(list_LHS)
  ptr$Xm_Settings$RHS = c(ptr$NormalizedData*ptr$Weight)
  ptr$Xm_Settings$W =c(ptr$Weight)
  ptr$Xm_Settings$colnames=cnames

  # object not returned, uses environment for pointer like behavior
}

# Create diagonal matrix
Create_Z_Stuff= function(ptr){
  ptr$Factors$Z = rep(1, ptr$Dims$numTS)
}

# Function for initialization, remove the effect of external regressors
InitiallyRemoveXReg = function(ptr,ImputedM){

  xreg_fit = ImputedM
  numCol = ptr$Dims$ncols
  numReg = ptr$xReg_model$numReg+1
  ind = ptr$xReg_model$XregInd
  LambdaI = diag(numReg)*ptr$Fm_Settings$lambda

  LambdaI = diag(c(0,diag(ptr$Fm_Settings$Constraints$LambdaI)[ind]))
  rhs0 = c(0,ptr$Fm_Settings$Constraints$rhs0[ind])
           
           
  intercept = rep(1,ptr$Dims$nrows)
  type = ptr$Fm_Settings$type
  if(ptr$Fm_Settings$type =="constrain"){
    E = matrix(1,1,numReg)
    G =  diag(numReg)
    H =  rhs0
  }else if(ptr$Fm_Settings$type =="interval"){
    ones = rep(1,numReg)
    G =  rbind(diag(x=1,nrow=numReg),diag(x=-1,nrow=numReg))
    H =  c(rhs0,-ones)
  }

  lFm = matrix(0,numReg-1,numCol)

  for(k in 1:numCol){
    # loop over and fit each row
    wt = ptr$Weight[,k]
    Y = c(wt*ptr$NormalizedData[,k],rhs0)
    X = rbind(wt*cbind(intercept,ptr$xReg_model$ColumnwiseXReg[,k,],ptr$xReg_model$GlobalXReg),LambdaI)

    # Type of regression
    if(type=="nnls"){
      fm =limSolve::nnls(X,Y)$X # nonnegative
    }
    else if(type=="l2"){
      fm = limSolve::Solve(X,Y) # ridge regression
    }
    else if(type=="constrain"){
      fm= limSolve::lsei(A=X,B=Y,E=E,F=1,G=G,H=H)$X
    }
    else if(type=="interval"){
      fm= limSolve::lsei(A=X,B=Y,G=G,H=H,type=2)$X
    }

    # get fit
    fm[1]=0 # take out impact of intercept (make this an option?)
    newX = cbind(intercept,ptr$xReg_model$ColumnwiseXReg[,k,],ptr$xReg_model$GlobalXReg)
    xreg_fit[,k] =newX%*%fm
    lFm[,k] = fm[-1]
  }

  ind = ptr$xReg_model$XregInd
  ptr$xReg_model$Shift = xreg_fit
  ptr$Factors$Fm = lFm
  # object not returned, uses environment for pointer-like behavior

}

# Initialize ALS iteration
InitializeALS= function(ptr){

  # Get needed stuff
  M = ptr$NormalizedData
  PD = ptr$HadamardProjection
  numTS = ptr$Dims$numTS
  ncol = dim(M)[2]
  nrow = dim(M)[1]
  ptr$Factors$Z = rep(1,numTS) # Add in diagonal values
  if(any(PD==0)){
      M[PD==0]=NA # don't average these

    # Impute missing values with averages (iteration 1)
    rs = rowMeans(M,na.rm=TRUE)
    rs[rs==0|is.na(rs)] = 1
    ImputeM = M/rs
    cs = colMeans(ImputeM,na.rm=TRUE)
    cs[cs==0|is.na(cs)]=1
    ImputeM = rs%*%t(cs)
    ind=is.na(M)
    M[ind]=ImputeM[ind]

    # SVD to impute (iteration 2)
    num = min(2,dim(M))
    out = svd(M,nu=num,nv=num)
    ImputeM = out$u%*%diag(x=out$d[1:num],nrow=num)%*%t(out$v)
    M[ind]=ImputeM[ind]
}
  # If we have regressors subtract out their influence first
  if(!is.null(ptr$xReg_model)){
    InitiallyRemoveXReg(ptr,M)
    lFm = ptr$Factors$Fm
    M = M - ptr$xReg_model$Shift
  }else{
    lFm = NULL
  }

  # If we only have one latent time-series, just return 1s
  if(numTS==1){
    Fm = matrix(1,ncol,1)
    ptr$Factors$Fm =  rbind(lFm,t(Fm))
    return(NULL)
  }

  # SVD to initialize (iteration 3)
  out = svd(M,nu=0,nv=numTS)

  Fm = matrix(0.1,ncol,numTS) # fill matrix in case there is more models then time series
  Fm[1:dim(out$v)[1],] = out$v


  # Format according to regularization type...
  if(ptr$Fm_Settings$type=="nnls"){
    Fm = Fm+0.1
    Fm[Fm<0]=0
    Fm = Fm+0.001
  }else if(ptr$Fm_Settings$type=="constrain"){
    Fm = Fm+abs(min(Fm))+.001
    Fm = Fm/rowSums(Fm)
  }else if(ptr$Fm_Settings$type=="interval"){
    Fm = Fm+0.1
    Fm[Fm<0]=0
    Fm = Fm+0.01
    Fm[Fm>1] = 1
  }
  # add external regressors
  ptr$Factors$Fm =  rbind(lFm,t(Fm))
}

# Remove effect of external regressors based on prefit Fm coefficients
Get_XReg_fit= function(ptr){
  if(!is.null(ptr$xReg_model)){
    numCol = ptr$Dims$ncols
    ind = ptr$xReg_model$XregInd
     for(k in 1:numCol){
      newX = cbind(ptr$xReg_model$ColumnwiseXReg[,k,],ptr$xReg_model$GlobalXReg)
      ptr$xReg_model$Shift[,k] = newX%*%ptr$Factors$Fm[ind,k]
    }
  }

  # object not returned, uses environment for pointer like behavior
}

# Put data and Fm matrix in proper format, solve
FitXm = function(ptr){
  
  # adjust out external regressors if we have them
  if(!is.null(ptr$xReg_model)){
    ptr$Xm_Settings$RHS = c(ptr$Weight*(ptr$NormalizedData-ptr$xReg_model$Shift))
    tempFm = ptr$Factors$Fm[ptr$xReg_model$XmInd,]
  }else{
    tempFm = ptr$Factors$Fm
  }
  tempFm = diag(ptr$Factors$Z)%*%tempFm
  
  # Build relevant matrices
  nRows = ptr$Dims$nrows
  numTS = ptr$Dims$numTS
  wYdata = ptr$Xm_Settings$RHS
  wtFm = ptr$Xm_Settings$W*(t(tempFm)%x%Matrix::Diagonal(n=nRows))
  LHS = crossprod(wtFm)
  RHS = crossprod(wtFm,wYdata)
  
  # solve and store
  Xmv = Matrix::solve((LHS+ptr$Xm_Settings$ConstraintM),RHS)
  ptr$Factors$Xm = matrix(Xmv,nrow=nRows,ncol=numTS) # fix this
  
  # adjust diagonal
  if(!is.null(ptr$scaleXm)){
    chiSum = sqrt(colMeans(ptr$Factors$Xm^2))
    chiSum[chiSum<=0]=1
    ptr$Factors$Xm = ptr$Factors$Xm%*%diag(x=1.0/chiSum,nrow=numTS)
    if(ptr$scaleXm =='track'){
      ptr$Factors$Z = ptr$Factors$Z*chiSum
    }
  }
  
  # object not returned, uses environment for pointer like behavior
}

# Fit constrained least squares to find Fm matrix
FitFm = function(ptr){

  numCol = ptr$Dims$ncols
  numRow = ptr$Fm_Settings$Constraints$totNum

  # Rename for simplicity (should be fine with "copy on modify" semantics)
  type = ptr$Fm_Settings$type
  LambdaI = ptr$Fm_Settings$Constraints$LambdaI
  rhs0 = ptr$Fm_Settings$Constraints$rhs0
  E = ptr$Fm_Settings$Constraints$E
  G = ptr$Fm_Settings$Constraints$G
  H = ptr$Fm_Settings$Constraints$H

  cXreg = ptr$xReg_model$ColumnwiseXReg
  gXreg = ptr$xReg_model$GlobalXReg
  Xm = ptr$Factors$Xm%*%diag(ptr$Factors$Z)
  Wt = ptr$Weight
  Dm = ptr$NormalizedData

    # loop over and fit each row
  for(k in 1:numCol){
    wt = Wt[,k]
    Y = c(wt*Dm[,k],rhs0)
    if(!is.null(ptr$xReg_model)){
      X = rbind(wt*cbind(cXreg[,k,],gXreg,Xm),LambdaI)
    }else{
      X = rbind(wt*cbind(Xm),LambdaI)
    }

  # Type of regression
    if(type=="nnls"){
      fm =limSolve::nnls(X,Y)$X # nonnegative
    }
    else if(type=="l2"){
      fm = limSolve::Solve(X,Y) # ridge regression
    }
    else if(type=="constrain"){
      fm= limSolve::lsei(A=X,B=Y,E=E,F=1,G=G,H=H)$X
    }
    else if(type=="interval"){
      fm= limSolve::lsei(A=X,B=Y,G=G,H=H,type=2)$X
    }
    ptr$Factors$Fm[,k] =  fm
  }

  # object not returned, uses environment for pointer like behavior
}

# Get the fitted values after the fact...
FitAll = function(ptr){

  # reconstruct matrix
  numCol = ptr$Dims$ncols
  fitted = 0*ptr$dataM
  Xm = ptr$Factors$Xm%*%diag(ptr$Factors$Z)
  for(k in 1:numCol){
    newX = cbind(ptr$xReg_model$ColumnwiseXReg[,k,],ptr$xReg_model$GlobalXReg,Xm)
    fitted[,k] = newX%*%ptr$Factors$Fm[,k]
  }

  # rescale
  Shift = attr(ptr$NormalizedData,"shift")
  Scale = attr(ptr$NormalizedData,"scale")
  Type = attr(ptr$NormalizedData,"scale_type")
  ptr$Fit$fitted = unScale(fitted,Type,Scale=Scale,Shift=Shift)
  ptr$Fit$resid = ptr$dataM-ptr$Fit$fitted

  # Name Xm, Fm, fits,
  rownames(ptr$Factors$Fm) = c(ptr$xReg_model$colnames,ptr$Xm_Settings$colnames)
  colnames(ptr$Factors$Fm) = colnames(ptr$dataM)
  rownames(ptr$Factors$Xm) = rownames(ptr$dataM)
  colnames(ptr$Factors$Xm) = ptr$Xm_Settings$colnames
  colnames(ptr$Fit$fitted) = colnames(ptr$dataM)
  rownames(ptr$Fit$fitted) = rownames(ptr$dataM)
  colnames(ptr$Fit$resid) = colnames(ptr$dataM)
  rownames(ptr$Fit$resid) = rownames(ptr$dataM)

  # object not returned, uses environment for pointer like behavior

}



