# v12 created on Oct. 9, 2019
#  (1) directly use limma instead of iCheck::lmFitWrapper
#
# v11 created on Sept. 28, 2019
#  (1) add input parameter 'method' to decide how to determine
#      SNP clusters based on responsibility matrix
#
# v10 created on Sept. 22, 2019
#  (1) add functions 'estMemSNPs.oneSetHyperPara' and 
#      'estMemSNPs.piAB.oneSetHyperPara'
#
# v9 created on March 25, 2018
#  (1) output res.limma in estMemSNPs()
#
# v8 created on March 24, 2018
#  (1) add na.rm=TRUE to 'sum', 'mean', 'var'
#  (2) use limma to get initial estimates
#
# v6 created on Jan. 20, 2018
#  (1) fixed bugs in estMemSNPs2.piAB
#
# v5 created on Jan. 19, 2018
#  (1) fixed a bug in EM loop: alpha02 and beta02 should be outside of while()
# v4 created on Jan. 19, 2018
#  (1) in EM algorithm, add update for alpha and beta for EE group
#
# v3 created on Jan. 19, 2018
#  (1) add wrapper function to estimate mixture proportions
#  (2) fixed a bug: the column order of wiMat should be the same as
#      that of piVec: -, 0, +
#
# v2 created on Jan. 19, 2018
#  (1) add functions to model data with SNP-cluster-specific and
#      subject-group-specific model parameters
#
# estimate SNP cluster membership

# log density for EE group
logD0Func=function(genoVec, alpha, beta)
{
  n0=sum(genoVec==0, na.rm=TRUE)  
  n1=sum(genoVec==1, na.rm=TRUE)  
  n2=sum(genoVec==2, na.rm=TRUE)  

  alpha0=2*n2+n1+alpha
  beta0=2*n0+n1+beta

  tt1=lbeta(a=alpha0, b=beta0)
  tt2=lbeta(a=alpha, b=beta)

  logD0=n1*log(2)+tt1-tt2
  res=list(logD0=logD0, alpha0=alpha0, beta0=beta0)
  return(res)
}

# Q function
QFunc=function(par, genoMat, wMat, piVec, memSubjs, bVec=rep(3,3))
{
  alpha=par[1]
  beta=par[2]
  logpi=log(piVec)
  const1=lgamma(sum(bVec, na.rm=TRUE))-lgamma(bVec[1])-lgamma(bVec[2])-
    lgamma(bVec[3])
  const2=sum((bVec-1)*logpi, na.rm=TRUE)

  tt1=sum(wMat%*%logpi, na.rm=TRUE)

  ttp=apply(genoMat, 1, function(x) {
    ttres=logD.pFunc(x,memSubjs=memSubjs, 
    alpha=alpha, beta=beta)$logD.p})
  tt0=apply(genoMat, 1, function(x) {
    ttres=logD0Func(x,alpha=alpha, beta=beta)$logD0
    return(ttres)})
  ttn=apply(genoMat, 1, function(x) {
    ttres=logD.nFunc(x, memSubjs=memSubjs,
    alpha=alpha, beta=beta)$logD.n
    return(ttres)})

  tt2=sum(wMat[,1]*ttn, na.rm=TRUE)+
    sum(wMat[,2]*tt0, na.rm=TRUE) +
    sum(wMat[,3]*ttp, na.rm=TRUE)

  myQ = tt1+tt2+const1+const2
  #cat("#ttp==-Inf>>", sum(ttp== -Inf),"\n")
  #cat("#tt0==-Inf>>", sum(tt0== -Inf),"\n")
  #cat("#ttn==-Inf>>", sum(ttn== -Inf),"\n")
  #cat(" alpha=", alpha, ", beta=", beta, "\n") 
  #cat(" tt1=", tt1, ", tt2=", tt2, ", myQ=", myQ, "\n") 
  return(myQ)
}



# partial derivative of log density for EE group to alpha and beta
dQFunc=function(par, genoMat, wMat, piVec, memSubjs, bVec=rep(3,3))
{
  alpha=par[1]
  beta=par[2]
  tt=apply(genoMat, 1, dlogD0Func, alpha=alpha, beta=beta)
  res1=sum(tt[1,]*wMat[,2], na.rm=TRUE)
  res2=sum(tt[2,]*wMat[,2], na.rm=TRUE)
  res=c(res1, res2)
  return(res)
}

# partial derivative of log density for EE group to alpha and beta
dlogD0Func=function(genoVec, alpha, beta)
{
  n0=sum(genoVec==0, na.rm=TRUE)  
  n1=sum(genoVec==1, na.rm=TRUE)  
  n2=sum(genoVec==2, na.rm=TRUE)  

  alpha0=2*n2+n1+alpha
  beta0=2*n0+n1+beta

  tt0=digamma(alpha0+beta0)
  tt=digamma(alpha+beta)
  d.alpha=digamma(alpha0)-tt0- digamma(alpha)+tt

  d.beta=digamma(beta0)-tt0- digamma(beta)+tt

  res=c(d.alpha, d.beta)
  return(res)
}

# log density for + group
logD.pFunc=function(genoVec, memSubjs, alpha, beta)
{
  genoVec.ca=genoVec[which(memSubjs==1)]
  genoVec.co=genoVec[which(memSubjs==0)]

  res.x=logD0Func(genoVec.ca, alpha, beta)
  lcx=res.x$logD0
  alpha.x=res.x$alpha0
  beta.x=res.x$beta0

  res.y=logD0Func(genoVec.co, alpha, beta)
  lcy=res.y$logD0
  alpha.y=res.y$alpha0
  beta.y=res.y$beta0

  tmpFunc=function(vy, alpha.x, beta.x,
    alpha.y, beta.y)
  {
    tt1=pbeta(vy, shape1=alpha.x, shape2=beta.x)
    tt2=dbeta(x=vy, shape1=alpha.y, shape2=beta.y) 
    return(tt1*tt2)
  }

  res.int=integrate(f=tmpFunc, lower=0, upper=1, 
    alpha.x=alpha.x, beta.x=beta.x,
    alpha.y=alpha.y, beta.y=beta.y)

  prVxVy=1-res.int$value
  # if prVxVy is very close to zero,
  #   it might be represented as negative value in computer
  if(prVxVy<0)
  {
    prVxVy = abs(prVxVy)
  }

  if(prVxVy<1.0e-300)
  {
    prVxVy=1.0e-300
  } 

  logD.p=log(2)+lcx+lcy+log(prVxVy)
  
  res=list(logD.p=logD.p, 
    alpha.x=alpha.x, beta.x=beta.x,
    alpha.y=alpha.y, beta.y=beta.y)
  #cat(" logD.p=", logD.p)
  return(res)
}

# log density for - group
logD.nFunc=function(genoVec, memSubjs, alpha, beta)
{
  genoVec.ca=genoVec[which(memSubjs==1)]
  genoVec.co=genoVec[which(memSubjs==0)]

  res.x=logD0Func(genoVec.ca, alpha, beta)
  lcx=res.x$logD0
  alpha.x=res.x$alpha0
  beta.x=res.x$beta0

  res.y=logD0Func(genoVec.co, alpha, beta)
  lcy=res.y$logD0
  alpha.y=res.y$alpha0
  beta.y=res.y$beta0

  tmpFunc=function(vy, alpha.x, beta.x,
    alpha.y, beta.y)
  {
    tt1=pbeta(vy, shape1=alpha.x, shape2=beta.x)
    tt2=dbeta(x=vy, shape1=alpha.y, shape2=beta.y) 
    return(tt1*tt2)
  }

  res.int=integrate(f=tmpFunc, lower=0, upper=1, 
    alpha.x=alpha.x, beta.x=beta.x,
    alpha.y=alpha.y, beta.y=beta.y)

  prVxVy=res.int$value
  # if prVxVy is very close to zero,
  #   it might be represented as negative value in computer
  if(prVxVy<0)
  {
    prVxVy= abs(prVxVy)
  }
  if(prVxVy<1.0e-300)
  {
    prVxVy=1.0e-300
  } 


  logD.n=log(2)+lcx+lcy+log(prVxVy)
  res=list(logD.n=logD.n, 
    alpha.x=alpha.x, beta.x=beta.x,
    alpha.y=alpha.y, beta.y=beta.y)

  return(res)
}

estMemSNPs.default=function(genoMat, memSubjs,
  alpha.p=2, beta.p=5, pi.p=0.1,
  alpha0=2, beta0=5, pi0=0.8,
  alpha.n=2, beta.n=5, pi.n=0.1, 
  method = "FDR", # possible methods are "FDR" and "max"
  fdr = 0.05,
  verbose=FALSE)
{
  nSNPs=nrow(genoMat)
  wMat=matrix(NA, nrow=nSNPs, ncol=3)
  colnames(wMat)=c("w-", "wEE", "w+")
  memSNPs=rep(NA, nSNPs)
  for(g in 1:nSNPs)
  {
    geno.g=genoMat[g,]
    logD.p=logD.pFunc(genoVec=geno.g, 
      memSubjs=memSubjs, 
      alpha=alpha.p, beta=beta.p)$logD.p

    logD0=logD0Func(genoVec=geno.g, 
      alpha=alpha0, beta=beta0)$logD0

    logD.n=logD.nFunc(genoVec=geno.g, 
      memSubjs=memSubjs, 
      alpha=alpha.p, beta=beta.p)$logD.n

    logVec=c(logD.n, logD0, logD.p)
    max.log=max(logVec, na.rm=TRUE)
    # log diff
    logdiff=logVec-max.log
    diff=exp(logdiff)

    piVec=c(pi.n, pi0, pi.p)
    pidiff=piVec*diff
    denom=sum(pidiff, na.rm=TRUE)

    wMat[g,]=pidiff/denom
    # + group will have mem=1
    # 0 group will have mem=0
    # - group will have mem=-1
    memSNPs[g]=which.max(wMat[g,])-2
  }

  if(method=="FDR")
  {
    memSNPs=callSNP(wmat=wMat, fdr=fdr)
  }
  memSNPs2=as.numeric(memSNPs != 0)

  res=list(wMat=wMat, memSNPs=memSNPs, 
    memSNPs2=memSNPs2)
  invisible(res)
}

# only update cluster mixture proportions
# and use initial estimates based on hyper-parameters estimated 
#   using all SNPs  as the final estimates for hyper-parameters.
# assume all 3 clusters have the same set of hyper-parameters.

# es - an ExpressionSet object
# var.memSubjs - phenotype variable name indicating subject group
#    1 means case and 0 means control
# eps - convergence criterion
# MaxIter - maximum iteration for EM algorithm
# bVec - parameters for the symmetric Dirichlet prior for proportion mixtures
estMemSNPs.oneSetHyperPara=function(
  es, 
  var.memSubjs="memSubjs", 
  eps=1.0e-3,
  MaxIter=50, 
  bVec=rep(3,3), 
  pvalAdjMethod="none",
  method = "FDR", # possible methods are "FDR" and "max"
  fdr = 0.05,
  verbose=FALSE)
{
  pDat=pData(es)
  memSubjs=pDat[, c(var.memSubjs)]
  tt=table(memSubjs)
  nm=names(tt)
  tt=sort(nm)
  if(!identical(tt, c("0","1")))
  {
    stop("memSubjs should be binary taking only values 0 or 1!")
  }

  # get initial 3-cluster partition
  fDat=fData(es)
  #nr=nrow(es)
  #fDat$probe=paste("probe", 1:nr, sep="")
  #fDat$gene=paste("gene", 1:nr, sep="")
  #fDat$chr=rep(1, nr)
  #rownames(fDat)=featureNames(es)
  #es2=es
  #fData(es2)=fDat
  pDat=pData(es)
  genoMat=exprs(es)

  fmla=as.formula(paste("~", var.memSubjs, sep=""))
  design=model.matrix(fmla, data=pDat)

  fit = lmFit(genoMat, design)
  ebFit = eBayes(fit)
  pval = ebFit$p.value[, 2]
  p.adj = p.adjust(pval, method=pvalAdjMethod)

  stats = ebFit$t[, 2]
  nSNPs=length(stats)
  memGenes.ini=rep("EE", nSNPs)
  memGenes.ini[which(stats>0 & p.adj < 0.05)] = "+" 
  memGenes.ini[which(stats<0 & p.adj < 0.05)] = "-" 

  memGenes2=rep(1, nSNPs)
  memGenes2[which(memGenes.ini == "EE")]=0

  frame.unsorted=data.frame(stats=stats, pval=pval, p.adj=p.adj)

  res.limma=list(ebFit=ebFit, frame.unsorted=frame.unsorted,
    memGenes.ini=memGenes.ini, memGenes2=memGenes2)

#  res.limma=iCheck::lmFitWrapper(
#    es=es2,
#    formula=fmla,
#    pvalAdjMethod=pvalAdjMethod,
#    probeID.var="probe",
#    gene.var="gene",
#    chr.var="chr",
#    verbose=FALSE)
#
#  tt.memGenes.ini=res.limma$memGenes
#  memGenes.ini=rep(NA, length(tt.memGenes.ini))
#  memGenes.ini[which(tt.memGenes.ini==1)]="+"
#  memGenes.ini[which(tt.memGenes.ini==2)]="EE"
#  memGenes.ini[which(tt.memGenes.ini==3)]="-"
#

  #res.limma$memGenes.ini=memGenes.ini

  #genoMat=exprs(es)
  # parameter estimates based on snp-wise test
  nSNPs2=nrow(genoMat)
  theta.hat=rep(NA, nSNPs2)
  nTotal=ncol(genoMat)
  for(g2 in 1:nSNPs2)
  {
    x=genoMat[g2,]
    theta.hat[g2]=(2*sum(x==2, na.rm=TRUE)+sum(x==1, na.rm=TRUE))/(2*nTotal)
  }
 
  n.p=sum(memGenes.ini=="+", na.rm=TRUE)
  n.n=sum(memGenes.ini=="-", na.rm=TRUE)
  n.0=sum(memGenes.ini=="EE", na.rm=TRUE)

  #####
  # piVec[1] for - group
  # piVec[2] for EE group
  # piVec[3] for + group
  nSNPs2=length(memGenes.ini)
  piVec = c(n.n, n.0, n.p)/nSNPs2
  names(piVec)= c("-", "EE", "+")
  piVec.ini=piVec

  if(verbose)
  {
    cat("\ninitial pi>>\n")
    print(piVec)
    cat("\n")
    cat("\nnSNPs*pi.ini>>\n")
    print(nSNPs2*piVec)
    cat("\n")
    cat("\ntable(memGenes.ini>>\n")
    print(table(memGenes.ini, useNA="ifany"))
    cat("\n")
  }

  # use estimated prior assuming alpha and beta are the same across 3 clusters
  est.MLE02=est.alpha.beta.MLE(x=theta.hat)
  alpha02=est.MLE02$res.moment[1]
  beta02=est.MLE02$res.moment[2]
 

  # estimate mixture proportions using EM algorithm
  loop=0
  diff = 10000
  while(loop < MaxIter)
  {
    loop = loop + 1
   
    res.est2=estMemSNPs.default(genoMat=genoMat, memSubjs=memSubjs,
                                alpha.p=alpha02, beta.p=beta02, pi.p=piVec[3],
                                alpha0=alpha02, beta0=beta02, pi0=piVec[2],
                                alpha.n=alpha02, beta.n=beta02, pi.n=piVec[1],
                                method = method, fdr=fdr,
                                verbose=FALSE)
   
    wMat=res.est2$wMat
    piVec.new=apply(wMat, 2, function(x) {
      tt=sum(x,na.rm=TRUE)
      numer=tt+bVec[1]-1
      denom=nSNPs2+sum(bVec, na.rm=TRUE)-3
      return(numer/denom)
    })
    diff=sum((piVec-piVec.new)^2, na.rm=TRUE)
    if(diff<eps)
    {
      break
    }
    piVec=piVec.new
  }

  wMat=res.est2$wMat
  memSNPs=res.est2$memSNPs
  memSNPs2=res.est2$memSNPs2

  if(verbose)
  {
    cat("\ndiff=", diff, "\n")
    cat("\nloop=", loop, ", MaxIter=", MaxIter, "\n")
    cat("\nfinal pi>>\n")
    print(piVec.new)
    cat("\n")
    cat("\nnSNPs*pi.final>>\n")
    print(nSNPs2*piVec.new)
    cat("\n")
    cat("table(memSNPs)>>\n")
    print(table(memSNPs, useNA="ifany"))
    cat("\n")

  }


  res=list(
    wMat=wMat, 
    memSNPs=memSNPs, 
    memSNPs2=memSNPs2, 
    piVec.ini=piVec.ini,
    piVec=piVec.new,
    alpha=alpha02, 
    beta=beta02, 
    loop=loop,
    diff=diff,
    res.limma=res.limma)
  invisible(res)
}

# 3 clusters have different sets of hyperparameters
# use initial estimates based on hyper-parameters estimated 
#   using all SNPs 
# es - an ExpressionSet object
# var.memSubjs - phenotype variable name indicating subject group
#    1 means case and 0 means control
# eps - convergence criterion
# MaxIter - maximum iteration for EM algorithm
# bVec - parameters for the symmetric Dirichlet prior for proportion mixtures
estMemSNPs=function(
  es, 
  var.memSubjs="memSubjs", 
  eps=1.0e-3,
  MaxIter=50, 
  bVec=rep(3,3), 
  pvalAdjMethod="fdr",
  method = "FDR", # possible methods are "FDR" and "max"
  fdr = 0.05,
  verbose=FALSE)
{
  pDat=pData(es)
  memSubjs=pDat[, c(var.memSubjs)]
  tt=table(memSubjs)
  nm=names(tt)
  tt=sort(nm)
  if(!identical(tt, c("0","1")))
  {
    stop("memSubjs should be binary taking only values 0 or 1!")
  }

  # get initial 3-cluster partition
  fDat=fData(es)
#  nr=nrow(es)
#  fDat$probe=paste("probe", 1:nr, sep="")
#  fDat$gene=paste("gene", 1:nr, sep="")
#  fDat$chr=rep(1, nr)
#  rownames(fDat)=featureNames(es)
#  es2=es
#  fData(es2)=fDat
#

  pDat=pData(es)
  genoMat=exprs(es)

  fmla=as.formula(paste("~", var.memSubjs, sep=""))
  design=model.matrix(fmla, data=pDat)

  fit = lmFit(genoMat, design)
  ebFit = eBayes(fit)
  pval = ebFit$p.value[, 2]
  p.adj = p.adjust(pval, method=pvalAdjMethod)

  stats = ebFit$t[, 2]
  nSNPs=length(stats)
  memGenes.ini=rep("EE", nSNPs)
  memGenes.ini[which(stats>0 & p.adj < 0.05)] = "+" 
  memGenes.ini[which(stats<0 & p.adj < 0.05)] = "-" 

  memGenes2=rep(1, nSNPs)
  memGenes2[which(memGenes.ini == "EE")]=0

  frame.unsorted=data.frame(stats=stats, pval=pval, p.adj=p.adj)

  res.limma=list(ebFit=ebFit, frame.unsorted=frame.unsorted,
    memGenes.ini=memGenes.ini, memGenes2=memGenes2)

#
#
#  fmla=as.formula(paste("~", var.memSubjs, sep=""))
#  res.limma=iCheck::lmFitWrapper(
#    es=es2,
#    formula=fmla,
#    pvalAdjMethod=pvalAdjMethod,
#    probeID.var="probe",
#    gene.var="gene",
#    chr.var="chr",
#    verbose=FALSE)
#
#  tt.memGenes.ini=res.limma$memGenes
#  memGenes.ini=rep(NA, length(tt.memGenes.ini))
#  memGenes.ini[which(tt.memGenes.ini==1)]="+"
#  memGenes.ini[which(tt.memGenes.ini==2)]="EE"
#  memGenes.ini[which(tt.memGenes.ini==3)]="-"
#
#  res.limma$memGenes.ini=memGenes.ini
  memSNPs2.snpTest=res.limma$memGenes2
 
  genoMat=exprs(es)
  # parameter estimates based on snp-wise test
  nSNPs2=nrow(genoMat)
  genoMat.ca=genoMat[, which(memSubjs==1)]
  genoMat.co=genoMat[, which(memSubjs==0)]
  theta.ca.hat=rep(NA, nSNPs2)
  theta.co.hat=rep(NA, nSNPs2)
  theta.hat=rep(NA, nSNPs2)
  nTotal=ncol(genoMat)
  for(g2 in 1:nSNPs2)
  {
    x.ca=genoMat.ca[g2,]
    x.co=genoMat.co[g2,]
    x=genoMat[g2,]
    theta.hat[g2]=(2*sum(x==2)+sum(x==1))/(2*nTotal)
    theta.ca.hat[g2]=(2*sum(x.ca==2)+sum(x.ca==1))/(2*nTotal)
    theta.co.hat[g2]=(2*sum(x.co==2)+sum(x.co==1))/(2*nTotal)
    
  }
  
#  pos.p=which(memSNPs2.snpTest==1 & theta.ca.hat>theta.co.hat)
#  est.MLE.ca.p=est.alpha.beta.MLE(x=theta.ca.hat[pos.p])
#  est.MLE.co.p=est.alpha.beta.MLE(x=theta.co.hat[pos.p])
#
#  alpha.p.ca=est.MLE.ca.p$res.moment[1]
#  beta.p.ca=est.MLE.ca.p$res.moment[2]
#  
#  alpha.p.co=est.MLE.co.p$res.moment[1]
#  beta.p.co=est.MLE.co.p$res.moment[2]
#  ####
#  
#  pos.n=which(memSNPs2.snpTest==1 & theta.ca.hat<theta.co.hat)
#  est.MLE.ca.n=est.alpha.beta.MLE(x=theta.ca.hat[pos.n])
#  est.MLE.co.n=est.alpha.beta.MLE(x=theta.co.hat[pos.n])
#  
#  alpha.n.ca=est.MLE.ca.n$res.moment[1]
#  beta.n.ca=est.MLE.ca.n$res.moment[2]
#  
#  alpha.n.co=est.MLE.co.n$res.moment[1]
#  beta.n.co=est.MLE.co.n$res.moment[2]
#  #####
  pos.p=which(memSNPs2.snpTest==1 & theta.ca.hat>theta.co.hat)
  est.MLE.p=est.alpha.beta.MLE(x=theta.hat[pos.p])
  alpha.p=est.MLE.p$res.moment[1]
  beta.p=est.MLE.p$res.moment[2]
    
  ####
  pos0=which(memSNPs2.snpTest==0)
  est.MLE0=est.alpha.beta.MLE(x=theta.hat[pos0])
  alpha0=est.MLE0$res.moment[1]
  beta0=est.MLE0$res.moment[2]
  
  pos.n=which(memSNPs2.snpTest==1 & theta.ca.hat<theta.co.hat)
  est.MLE.n=est.alpha.beta.MLE(x=theta.hat[pos.n])
  alpha.n=est.MLE.n$res.moment[1]
  beta.n=est.MLE.n$res.moment[2]

  # piVec[1] for - group
  # piVec[2] for EE group
  # piVec[3] for + group
  piVec = c(length(pos.n), length(pos0), length(pos.p))/nSNPs2

#  # use estimated prior assuming alpha and beta are the same across 3 clusters
#  est.MLE02=est.alpha.beta.MLE(x=theta.hat)
#  alpha02=est.MLE02$res.moment[1]
#  beta02=est.MLE02$res.moment[2]
# 

  # estimate mixture proportions using EM algorithm
  loop=0
  diff = 10000
  while(loop < MaxIter)
  {
    loop = loop + 1
   
    res.est2=estMemSNPs.default(genoMat=genoMat, memSubjs=memSubjs,
                                alpha.p=alpha.p, beta.p=beta.p, pi.p=piVec[3],
                                alpha0=alpha0, beta0=beta0, pi0=piVec[2],
                                alpha.n=alpha.n, beta.n=beta.n, pi.n=piVec[1], 
                                method = method, fdr=fdr,
                                verbose=FALSE)
    wMat=res.est2$wMat
    piVec.new=apply(wMat, 2, function(x) {
      tt=sum(x,na.rm=TRUE)
      numer=tt+bVec[1]-1
      denom=nSNPs2+sum(bVec)-3
      return(numer/denom)
    })
    diff=sum((piVec-piVec.new)^2)
    if(diff<eps)
    {
      break
    }
    piVec=piVec.new
  }

  res=list(
    wMat=res.est2$wMat, 
    memSNPs=res.est2$memSNPs,
    memSNPs2=res.est2$memSNPs2, 
    piVec=piVec,
    alpha.p=alpha.p,
    beta.p=beta.p,
    alpha0=alpha0,
    beta0=beta0,
    alpha.n=alpha.n,
    beta.n=beta.n,
    loop=loop,
    diff=diff,
    res.limma=res.limma)

  invisible(res)
}


