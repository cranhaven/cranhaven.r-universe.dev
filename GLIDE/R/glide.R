
glide <- function(formula,exposure_coeff=NULL,genotype_columns=NULL,data,
                  np=100000,qcutoff=0.2,parallel=TRUE,corenumber=1,verbose=TRUE)
{
  gettime=function()
  {
    thetime=Sys.time()
    thetime=as.character(structure(thetime,class=c('POSIXt','POSIXct')))
  }
  if (inherits(formula,"character")) formula=as.formula(formula)
  if (inherits(exposure_coeff,"data.frame"))
  {
    tmp=exposure_coeff[,1]
    names(tmp)=rownames(exposure_coeff)
    exposure_coeff=tmp
  }
  #check intput
  checkdata(formula,exposure_coeff,genotype_columns,data)
  if (verbose) writeLines(paste0("GLIDE program starts at: ",gettime()))
  #remvoe rows containing NAs in the related columns
  idx=remove_missingdata(data)
  data=data[idx,]
  nsnp=length(exposure_coeff)
  outcome_coeff <- matrix(0,nsnp,2)
  formularchr=as.character(formula)
  for (i in 1:nsnp){
    snp=names(exposure_coeff)[i]
    fm=as.formula(paste0(formularchr[2],"~",snp,"+",formularchr[3]))
    fit <- glm(as.formula(fm),family=binomial,data=data)
    outcome_coeff[i,1] <- fit$coef[2]
    #variance
    outcome_coeff[i,2] <- summary(fit)$coef[2,2]^2
  }
  
  data$grs <- 0
  for (i in 1:nsnp){
    k<- which(colnames(data)==names(exposure_coeff)[i])
    data$grs <- data$grs + exposure_coeff[i]*data[,k]
  }
  
  outp <- rep(0,nsnp)
  for (i in 1:nsnp) {
    snp=names(exposure_coeff)[i]
    fm=as.formula(paste0(formularchr[2],"~grs+",snp,"+",formularchr[3]))
    fit <- glm(as.formula(fm),family=binomial,data=data)
    outp[i] <- summary(fit)$coef[3,4]
  }
  
  ### now work out the correlation matrix under the null hypothesis ####
  
  fm=as.formula(paste0(formularchr[2],"~grs+",formularchr[3]))
  fit <- glm(as.formula(fm),family=binomial,data=data,y=TRUE)
  yfit <- fit$fitted.values
  y <-fit$y #outcome
  xmat=model.matrix(as.formula(fm),data=data)
  grs=data$grs
  #generate genotype matrix, which columns are consistent with exposure_coeff
  data_genotype=data.frame(matrix(NA,nrow=nrow(data),ncol=nsnp))
  colnames(data_genotype)=names(exposure_coeff)
  for (i in 1:nsnp)
  {
    idx=which(colnames(data)==names(exposure_coeff)[i])
    data_genotype[,i]=data[,idx]
  }
  cormat <- matrix(1,nsnp,nsnp)
  # #storage.mode(x) <- storage.mode(y) <- "double"
  if (verbose) 
  {
    writeLines(paste0("Compute the correlation matrix at: ",gettime()))
    cat(paste0("Total ",nsnp," SNPs... "))
  }
  
  if (parallel)
  {
    #detect the number of CPU cores in the current machine
    numberofcores=detectCores()
    if (verbose) 
      writeLines(paste0("\nThere are ",numberofcores," cores available in the machine."))
    #if there are not enough cores available
    if (corenumber>numberofcores) corenumber=numberofcores
    #if there are much more cores available, use half of them
    #if (2*corenumber<=numberofcores) corenumber=as.integer(numberofcores/2)
    cl <- makeCluster(corenumber)
    registerDoParallel(cl)
    if (verbose & corenumber>1) 
      writeLines(paste0("Start parallel computation of the correlation matrix using ",corenumber," cores..."))
    #i=nsnp:1, do small jobs first to make sure the last job to finish in the last
    results <- foreach(i=nsnp:1,j=1:nsnp,.combine = data.frame, .packages = c("GLIDE")) %dopar%
    {
      #j measures the progress
      if (verbose & (j %% 10 == 0))
      {
        cat(j,"..")
      }
      cormat_col=rep(0,nsnp)
      result <- .C("compute_cormat_col",
                   nsnp=as.integer(nsnp),
                   n_subject=as.integer(nrow(data)),
                   colnumber=as.integer(i),
                   ncol_xmat=as.integer(ncol(xmat)),
                   yfit=as.double(yfit),
                   y=as.double(y),
                   xmat=as.double(xmat),
                   data_genotype=data.matrix(data_genotype),
                   cormat_col=as.double(cormat_col),
                   PACKAGE="GLIDE")
      result$cormat_col
    }
    for (i in 1:nsnp) cormat[,i]=results[,nsnp-i+1]
    
    for (i in 1:nsnp) cormat[i,i]=1
    for (i in 1:(nsnp-1))
    {
      for (j in (i+1):nsnp)
      {
        cormat[i,j]=cormat[j,i]
      }
    }
    #cormat=results
    if (verbose) writeLines("\n")
  }else
  {
    result <- .C("compute_cormat",
                 nsnp=as.integer(nsnp),
                 n_subject=as.integer(nrow(data)),
                 ncol_xmat=as.integer(ncol(xmat)),
                 yfit=as.double(yfit),
                 y=as.double(y),
                 xmat=as.double(xmat),
                 data_genotype=data.matrix(data_genotype),
                 cormat=as.double(cormat),
                 PACKAGE="GLIDE"
    )
    for (j in 1:nsnp)
    {
      for (i in 1:nsnp)
      {
        cormat[i,j]=result$cormat[(j-1)*nsnp+i]
      }
    }
  }
  
  
  cormat=as.matrix(cormat)
  
  zsim <- matrix(0,np,nsnp)
  if (verbose) writeLines(paste0("Compute the null p-values at: ", gettime()))
  
  zsim[,1] <- rnorm(np,0,1)
  if (parallel)
  {
    results <- foreach(i=2:nsnp) %dopar%
    {
      ginv(cormat[1:(i-1),1:(i-1)])%*% cormat[1:(i-1),i]
    }
  }
  for (k in 2:nsnp) {
    if (parallel)
    {
      mu <- drop(zsim[,1:(k-1)] %*% results[[k-1]])
    }else
    {
      mu <- drop(zsim[,1:(k-1)] %*% ginv(cormat[1:(k-1),1:(k-1)]) %*% cormat[1:(k-1),k])
    }
    if (k !=nsnp) 
      sig<- sqrt(1- drop(cormat[1:(k-1),k] %*% ginv(cormat[1:(k-1),1:(k-1)]) %*% cormat[1:(k-1),k]))
    if (k !=nsnp) 
      zsim[,k] <- rnorm(np,mu,sig) else zsim[,k] <- mu
  }
  for (k in 1:nsnp) zsim[,k] <- 2*(1-pnorm(abs(zsim[,k])))
  for (k in 1:np) {
    zsim[k,] <- zsim[k,order(zsim[k,])]
  }
  
  
  if (verbose) writeLines(paste0("\nCompute the FWER and FDR values at: ",gettime()))
  
  orderp <- apply(zsim,2,mean)
  
  ### compute the FWER and FDR values for each observed p-value
  
  fwer <- rep(0,length(outp))
  qval <- rep(0,length(outp))
  for (i in 1:length(outp)) {
    temp <- rowSums(zsim <= rep(outp[i],np))
    fwer[i] <- mean(temp>=1)
    qval[i] <- (sum(temp)/np)/sum(outp<=outp[i])
  }
  out <- data.frame(matrix(0,nsnp,6))
  out[,1] <- outp
  out[,2] <- orderp[rank(outp)]
  out[,3] <- fwer
  out[,4] <- qval
  qval <- rep(0,length(outp))
  for (i in 1:length(outp)) {
    qval[i] <- min(out[out[,1]>= out[i,1],4])
  }
  out[,4] <- qval
  out[,5:6] <- outcome_coeff
  colnames(out)=c("observed_pvalue","expected_pvalue","fwer","q_value",
                  "g_outcome","g_outcome_variance")
  rownames(out)=names(exposure_coeff)
  
  if (verbose) writeLines(paste0("\nGLIDE program ends at: ",gettime()))
  #  glide_plot(out,qcutoff)
  if (parallel)
    stopCluster(cl)
  out=out[order(out$observed_pvalue),]
  class(out)=c(class(out),"glide","egger")
  return(out)
}
