# created on Sept. 21, 2019
#  (1) rename 'simFunc.3cat' to 'simGenoFunc'
#  (1) rename 'simFunc.3cat2' to 'simGenoFuncDiffPriors'
#
# created on Dec. 27, 2018
#  (1) rename 'getSimDat' to 'genSimDat'
# v2 created on Jan. 19, 2018
#  (1) add function to simulate data with SNP-cluster-specific
#      and subject-group-specific model parameters
#
# created on Jan. 18, 2018
#  generate simulated data

# # for + group
# alpha.p=2
# beta.p=5
# nSNPs.p=100
# 
# # for EE group
# alpha0=2
# beta0=5
# nSNPs0=800
# 
# # for - group
# alpha.n=2
# beta.n=5
# nSNPs.n=100
# 
# ####
# 
# nCases=100
# nControls=100
# nTotal=nCases+nControls
# 

#set.seed(1234567)

# copied from iCheck BioConductor package
genExprSet2= function (ex, pDat, fDat = NULL, annotation = "") 
{
    cn.dat <- colnames(ex)
    rn.pdat <- rownames(pDat)
    aa <- match(cn.dat, rn.pdat)
    if (length(cn.dat) != length(rn.pdat)) {
        cat("Warning: No. of columns of ex=", length(cn.dat), 
            "\n")
        cat("not equalt to that of pDat =", length(rn.pdat), 
            "\n")
        diffxy <- setdiff(cn.dat, rn.pdat)
        if (length(diffxy)) {
            cat("The sample in ex, but not in pDat are>>\n")
            print(diffxy)
            cat("\n")
        }
        diffyx <- setdiff(rn.pdat, cn.dat)
        if (length(diffyx)) {
            cat("The sample in pDat, but not in ex are>>\n")
            print(diffyx)
            cat("\n")
        }
    }
    if (!any(is.na(aa) == TRUE)) {
        pDat2 <- pDat[aa, , drop = FALSE]
        identical(rownames(pDat2), colnames(ex))
        pDat3 <- as(pDat2, "data.frame")
        aa <- new("AnnotatedDataFrame", data = pDat3)
        exprs <- as(ex, "matrix")
        es.raw <- new("ExpressionSet", exprs = exprs, phenoData = aa, 
            annotation = annotation)
    }
    else {
        stop("Column names of ex != row names of pDat!\n")
    }
    if (!is.null(fDat)) {
        cn.fdat <- colnames(fDat)
        if (identical(sort(rownames(ex)), sort(rownames(fDat)))) {
            cc <- match(rownames(ex), rownames(fDat))
            Biobase::fData(es.raw) = fDat[cc, , drop = FALSE]
        }
        else {
            stop("Row names of ex != row names of dat.control!\n")
        }
    }
    invisible(es.raw)
}


est.alpha.beta.MLE=function(x)
{
  xbar=mean(x)
  vbar=var(x)
  
  x1x = xbar*(1-xbar)
  alpha.hat=NA
  beta.hat=NA
  if(vbar < x1x)
  {
    alpha.hat=xbar*(x1x/vbar -1)
    beta.hat=(1-xbar)*(x1x/vbar -1)
  }

  mu.hat=alpha.hat/(alpha.hat+beta.hat)
  sigma2.hat=alpha.hat*beta.hat/((alpha.hat+beta.hat)^2*(alpha.hat+beta.hat+1))
  md.hat=(alpha.hat-1)/(alpha.hat+beta.hat-2)
 
  N=length(x)
  Ghat.X=prod(x^(1/N))
  Ghat.1X=prod((1-x)^(1/N))
  
  alpha.hat2=NA
  beta.hat2=NA
  denom=2*(1-Ghat.X-Ghat.1X)
  if(alpha.hat >1)
  {
    alpha.hat2=0.5+Ghat.X/denom
  }
  if(beta.hat>1)
  {
    beta.hat2=0.5 + Ghat.1X/denom
  }
  
  mu.hat2=alpha.hat2/(alpha.hat2+beta.hat2)
  sigma2.hat2=alpha.hat2*beta.hat2/((alpha.hat2+beta.hat2)^2*(alpha.hat2+beta.hat2+1))
  md.hat2=(alpha.hat2-1)/(alpha.hat2+beta.hat2-2)
 
  ###
  # 3rd method
  ###
  myFuncs <- function(x, lnGX, lnG1X){
    alpha=x[1]
    beta=x[2]
    tt=digamma(alpha+beta)
    f1=digamma(alpha)-tt-lnGX
    f2=digamma(beta)-tt-lnG1X
    res=c(F1=f1, F2=f2)
    return(res)
    
  } 
  
  lnGX=log(Ghat.X)
  lnG1X=log(Ghat.1X)
  ss <- multiroot(f = myFuncs, start = c(alpha.hat2, beta.hat2), lnGX=lnGX, lnG1X=lnG1X)
  #print(ss)
  alpha.hat3=ss$root[1]
  beta.hat3=ss$root[2]
 
  mu.hat3=alpha.hat3/(alpha.hat3+beta.hat3)
  sigma2.hat3=alpha.hat3*beta.hat3/((alpha.hat3+beta.hat3)^2*(alpha.hat3+beta.hat3+1))
  md.hat3=(alpha.hat3-1)/(alpha.hat3+beta.hat3-2)
 
  res=c(alpha.hat3, beta.hat3, mu.hat3, sigma2.hat3, md.hat3)
  names(res) = c("alpha.hat", "beta.hat",
    "mean.hat", "variance.hat", "mode.hat")

  res.ini=c(alpha.hat2, beta.hat2, mu.hat2, sigma2.hat2, md.hat2)
  names(res.ini) = c("alpha.hat.ini", "beta.hat.ini",
    "mean.hat.ini", "variance.hat.ini", "mode.hat.ini")

  res.moment=c(alpha.hat, beta.hat, mu.hat, sigma2.hat, md.hat)
  names(res.moment) = c("alpha.hat.moment", "beta.hat.moment",
    "mean.hat.moment", "variance.hat.moment", "mode.hat.moment")


  res=list(res=res, res.ini=res.ini, res.moment=res.moment)
  return(res)

}




# simulate data in EE group
simFunc0=function(
  nCases=100,
  nControls=100,
  alpha0=2, beta0=5, nSNPs0=800,
  low=0.02, upp=0.5, verbose=FALSE

)
{
  
  nTotal=nCases+nControls
  # generate
  theta0Vec=rbeta(n=nSNPs0, shape1=alpha0, shape2=beta0)
  
  theta0Vec2=theta0Vec[which(theta0Vec>low & theta0Vec<upp)]
  nSNPs02=length(theta0Vec2)
  
  # mean=alpha/(alpha+beta)
  # var=alpha*beta/[(alpha+beta)^2*(alpha+beta+1)]
  # mode=(alpha-1)/(alpha+beta-2)
  mu=alpha0/(alpha0+beta0)
  sigma2=alpha0*beta0/((alpha0+beta0)^2*(alpha0+beta0+1))
  md=(alpha0-1)/(alpha0+beta0-2)
  #cat("mu=", mu, ", sigma2=", sigma2, ", md=", md, "\n")
  
  genoMat=matrix(NA, nrow=nSNPs02, ncol=nTotal)
  theta.hat=rep(NA, nSNPs02)
  
  for(g in 1:nSNPs02)
  {
    theta0.g=theta0Vec2[g]
    # mutation homozygote genotype
    p2=theta0.g^2
    
    # heterozygote genotype
    p1=2*theta0.g*(1-theta0.g)
    
    # wildtype homozygote genotype
    p0=(1-theta0.g)^2
    
    x=sample(x=c(0,1,2), size=nTotal, replace=TRUE, prob=c(p0, p1, p2))
    theta.hat[g]=(2*sum(x==2)+sum(x==1))/(2*nTotal)
    genoMat[g,]=x
    
  }
  
  # moment estimate of alpha and beta

  est.MLE=est.alpha.beta.MLE(x=theta.hat)
 
  #####
  if(verbose)
  {
    cat("\n********************************************\n")
    cat("Estimates of alpha and beta>>>\n")
    print(est.MLE)

    cat("\n********************************************\n")
  }
  res=list(genoMat=genoMat, theta.hat=theta.hat, est.MLE=est.MLE)
  invisible(res)
}

# simulate data in + group
simFunc.p=function(
  nCases=100,
  nControls=100,
  alpha.p=2, beta.p=5, nSNPs.p=100,
  low=0.02, upp=0.5, verbose=FALSE
)
{
  
  nTotal=nCases+nControls
  # generate
  theta.pVec.ca=rbeta(n=nSNPs.p, shape1=alpha.p, shape2=beta.p)
  theta.pVec.co=rbeta(n=nSNPs.p, shape1=alpha.p, shape2=beta.p)
  pos.t=which(theta.pVec.ca < theta.pVec.co)
  if(length(pos.t))
  {
    ca.tt=theta.pVec.ca[pos.t]
    co.tt=theta.pVec.co[pos.t]
    theta.pVec.ca[pos.t]=co.tt
    theta.pVec.co[pos.t]=ca.tt
  }
  
  pos.del.ca=which(theta.pVec.ca<low | theta.pVec.ca>upp)
  pos.del.co=which(theta.pVec.co<low | theta.pVec.co>upp)

  pos.del2=unique(c(pos.del.ca, pos.del.co))
  if(length(pos.del2))
  {
    theta.pVec.ca2=theta.pVec.ca[-pos.del2]
    theta.pVec.co2=theta.pVec.co[-pos.del2]
  } else {
    theta.pVec.ca2=theta.pVec.ca
    theta.pVec.co2=theta.pVec.co
  }

  nSNPs.p2=length(theta.pVec.ca2)
  
  genoMat=matrix(NA, nrow=nSNPs.p2, ncol=nTotal)
  theta.ca.hat=rep(NA, nSNPs.p2)
  theta.co.hat=rep(NA, nSNPs.p2)
  
  for(g in 1:nSNPs.p2)
  {
    theta.p.ca=theta.pVec.ca2[g]
    theta.p.co=theta.pVec.co2[g]

    # mutation homozygote genotype
    p2.ca=theta.p.ca^2
    p2.co=theta.p.co^2
    
    # heterozygote genotype
    p1.ca=2*theta.p.ca*(1-theta.p.ca)
    p1.co=2*theta.p.co*(1-theta.p.co)
    
    # wildtype homozygote genotype
    p0.ca=(1-theta.p.ca)^2
    p0.co=(1-theta.p.co)^2
    
    x.ca=sample(x=c(0,1,2), size=nCases, replace=TRUE, 
      prob=c(p0.ca, p1.ca, p2.ca))
    x.co=sample(x=c(0,1,2), size=nControls, replace=TRUE, 
      prob=c(p0.co, p1.co, p2.co))
    x=c(x.ca, x.co)

    theta.ca.hat[g]=(2*sum(x.ca==2)+sum(x.ca==1))/(2*nTotal)
    theta.co.hat[g]=(2*sum(x.co==2)+sum(x.co==1))/(2*nTotal)
    genoMat[g,]=x
  }
  

  est.MLE.ca=est.alpha.beta.MLE(x=theta.ca.hat)

  est.MLE.co=est.alpha.beta.MLE(x=theta.co.hat)

  #####
  if(verbose)
  {
    cat("\n********************************************\n")
    cat("Estimates of alpha and beta for cases>>>\n")
    print(est.MLE.ca)

    cat("\n**********************\n")

    cat("Estimates of alpha and beta for controls>>>\n")
    print(est.MLE.co)

    cat("\n********************************************\n")
  }
  res=list(genoMat=genoMat, 
    theta.ca.hat=theta.ca.hat, 
    theta.co.hat=theta.co.hat, 
    est.MLE.ca=est.MLE.ca,
    est.MLE.co=est.MLE.co
  )
  invisible(res)
}

# simulate data in - group
simFunc.n=function(
  nCases=100,
  nControls=100,
  alpha.n=2, beta.n=5, nSNPs.n=100,
  low=0.02, upp=0.5, verbose=FALSE

)
{
  
  nTotal=nCases+nControls
  # generate
  theta.pVec.ca=rbeta(n=nSNPs.n, shape1=alpha.n, shape2=beta.n)
  theta.pVec.co=rbeta(n=nSNPs.n, shape1=alpha.n, shape2=beta.n)
  pos.t=which(theta.pVec.ca > theta.pVec.co)
  if(length(pos.t))
  {
    ca.tt=theta.pVec.ca[pos.t]
    co.tt=theta.pVec.co[pos.t]
    theta.pVec.ca[pos.t]=co.tt
    theta.pVec.co[pos.t]=ca.tt
  }  

  pos.del.ca=which(theta.pVec.ca<low | theta.pVec.ca>upp)
  pos.del.co=which(theta.pVec.co<low | theta.pVec.co>upp)
 
  pos.del2=unique(c(pos.del.ca, pos.del.co))
  if(length(pos.del2))
  {
    theta.pVec.ca2=theta.pVec.ca[-pos.del2]
    theta.pVec.co2=theta.pVec.co[-pos.del2]
  } else {
    theta.pVec.ca2=theta.pVec.ca
    theta.pVec.co2=theta.pVec.co
  }

  nSNPs.n2=length(theta.pVec.ca2)
  
  genoMat=matrix(NA, nrow=nSNPs.n2, ncol=nTotal)
  theta.ca.hat=rep(NA, nSNPs.n2)
  theta.co.hat=rep(NA, nSNPs.n2)
  
  for(g in 1:nSNPs.n2)
  {
    theta.n.ca=theta.pVec.ca2[g]
    theta.n.co=theta.pVec.co2[g]

    # mutation homozygote genotype
    p2.ca=theta.n.ca^2
    p2.co=theta.n.co^2
    
    # heterozygote genotype
    p1.ca=2*theta.n.ca*(1-theta.n.ca)
    p1.co=2*theta.n.co*(1-theta.n.co)
    
    # wildtype homozygote genotype
    p0.ca=(1-theta.n.ca)^2
    p0.co=(1-theta.n.co)^2
    
    x.ca=sample(x=c(0,1,2), size=nCases, replace=TRUE, 
      prob=c(p0.ca, p1.ca, p2.ca))
    x.co=sample(x=c(0,1,2), size=nControls, replace=TRUE, 
      prob=c(p0.co, p1.co, p2.co))
    x=c(x.ca, x.co)

    theta.ca.hat[g]=(2*sum(x.ca==2)+sum(x.ca==1))/(2*nTotal)
    theta.co.hat[g]=(2*sum(x.co==2)+sum(x.co==1))/(2*nTotal)
    genoMat[g,]=x
  }
  
  est.MLE.ca=est.alpha.beta.MLE(x=theta.ca.hat)

  est.MLE.co=est.alpha.beta.MLE(x=theta.co.hat)

  #####
  if(verbose)
  {
    cat("\n********************************************\n")
    cat("Estimates of alpha and beta for cases>>>\n")
    print(est.MLE.ca)

    cat("\n**********************\n")

    cat("Estimates of alpha and beta for controls>>>\n")
    print(est.MLE.co)

    cat("\n********************************************\n")
  }
  res=list(genoMat=genoMat, 
    theta.ca.hat=theta.ca.hat, 
    theta.co.hat=theta.co.hat, 
    est.MLE.ca=est.MLE.ca,
    est.MLE.co=est.MLE.co
  )
  invisible(res)
}

# simulate data (case & control having same prior parameters 
# in each SNP cluster)
simGenoFunc=function(
  nCases=100,
  nControls=100,
  nSNPs=1000,
  alpha.p=2, beta.p=5, pi.p=0.1,
  alpha0=2, beta0=5, pi0=0.8,
  alpha.n=2, beta.n=5, pi.n=0.1,
  low=0.02, upp=0.5, verbose=FALSE
)
{
  nSNPs.p=ceiling(nSNPs*pi.p)
  nSNPs.n=ceiling(nSNPs*pi.n)
  nSNPs0=nSNPs-nSNPs.p-nSNPs.n

  genoMat.p=simFunc.p(
    nCases=nCases,
    nControls=nControls,
    alpha.p=alpha.p, beta.p=beta.p, nSNPs.p=nSNPs.p,
    low=low, upp=upp, verbose=FALSE
  )$genoMat


  genoMat0=simFunc0(
    nCases=nCases,
    nControls=nControls,
    alpha0=alpha0, beta0=beta0, nSNPs0=nSNPs0,
    low=low, upp=upp, verbose=FALSE
  )$genoMat

  genoMat.n=simFunc.n(
    nCases=nCases,
    nControls=nControls,
    alpha.n=alpha.n, beta.n=beta.n, nSNPs.n=nSNPs.n,
    low=low, upp=upp, verbose=FALSE
  )$genoMat

  genoMat=rbind(genoMat.p, genoMat0)
  genoMat=rbind(genoMat, genoMat.n)
 
  nSNPs.p2=nrow(genoMat.p)
  nSNPs02=nrow(genoMat0)
  nSNPs.n2=nrow(genoMat.n)
  nSNPs2=nSNPs.p2+nSNPs02+nSNPs.n2

  memGenes=c(rep(1, nSNPs.p2), rep(0, nSNPs02), rep(-1, nSNPs.n2))
  memGenes2=as.numeric(memGenes != 0)
  memSubjs=c(rep(1, nCases), rep(0, nControls))

  name.snps=paste("snp", 1:nSNPs2, sep="")
  nTotal=nCases+nControls
  name.subjs=paste("subj", 1:nTotal, sep="")

  rownames(genoMat)=name.snps
  colnames(genoMat)=name.subjs

  pDat=data.frame(sID=name.subjs, memSubjs=memSubjs)
  rownames(pDat)=name.subjs

  fDat=data.frame(snp=name.snps, 
    gene=paste("gene", 1:nSNPs2, sep=""),
    chr=rep(1, nSNPs2), memGenes=memGenes, memGenes2=memGenes2)
  rownames(fDat)=name.snps

  es=genExprSet2(ex=genoMat, pDat=pDat, fDat=fDat)
  
  invisible(es)
}

########### 
# simulate data in + group
simFunc.p2=function(
  nCases=100,
  nControls=100,
  alpha.p.ca=2, beta.p.ca=3, 
  alpha.p.co=2, beta.p.co=8, 
  nSNPs.p=100,
  low=0.02, upp=0.5, verbose=FALSE
)
{
  
  nTotal=nCases+nControls
  # generate
  theta.pVec.ca=rbeta(n=nSNPs.p, shape1=alpha.p.ca, shape2=beta.p.ca)
  theta.pVec.co=rbeta(n=nSNPs.p, shape1=alpha.p.co, shape2=beta.p.co)
  pos.t=which(theta.pVec.ca < theta.pVec.co)
  if(length(pos.t))
  {
    ca.tt=theta.pVec.ca[pos.t]
    co.tt=theta.pVec.co[pos.t]
    theta.pVec.ca[pos.t]=co.tt
    theta.pVec.co[pos.t]=ca.tt
  }
  
  pos.del.ca=which(theta.pVec.ca<low | theta.pVec.ca>upp)
  pos.del.co=which(theta.pVec.co<low | theta.pVec.co>upp)

  pos.del2=unique(c(pos.del.ca, pos.del.co))
  if(length(pos.del2))
  {
    theta.pVec.ca2=theta.pVec.ca[-pos.del2]
    theta.pVec.co2=theta.pVec.co[-pos.del2]
  } else {
    theta.pVec.ca2=theta.pVec.ca
    theta.pVec.co2=theta.pVec.co
  }

  nSNPs.p2=length(theta.pVec.ca2)
  
  genoMat=matrix(NA, nrow=nSNPs.p2, ncol=nTotal)
  theta.ca.hat=rep(NA, nSNPs.p2)
  theta.co.hat=rep(NA, nSNPs.p2)
  
  for(g in 1:nSNPs.p2)
  {
    theta.p.ca=theta.pVec.ca2[g]
    theta.p.co=theta.pVec.co2[g]

    # mutation homozygote genotype
    p2.ca=theta.p.ca^2
    p2.co=theta.p.co^2
    
    # heterozygote genotype
    p1.ca=2*theta.p.ca*(1-theta.p.ca)
    p1.co=2*theta.p.co*(1-theta.p.co)
    
    # wildtype homozygote genotype
    p0.ca=(1-theta.p.ca)^2
    p0.co=(1-theta.p.co)^2
    
    x.ca=sample(x=c(0,1,2), size=nCases, replace=TRUE, 
      prob=c(p0.ca, p1.ca, p2.ca))
    x.co=sample(x=c(0,1,2), size=nControls, replace=TRUE, 
      prob=c(p0.co, p1.co, p2.co))
    x=c(x.ca, x.co)

    theta.ca.hat[g]=(2*sum(x.ca==2)+sum(x.ca==1))/(2*nTotal)
    theta.co.hat[g]=(2*sum(x.co==2)+sum(x.co==1))/(2*nTotal)
    genoMat[g,]=x
  }
  

  est.MLE.ca=est.alpha.beta.MLE(x=theta.ca.hat)

  est.MLE.co=est.alpha.beta.MLE(x=theta.co.hat)

  #####
  if(verbose)
  {
    cat("\n********************************************\n")
    cat("Estimates of alpha and beta for cases>>>\n")
    print(est.MLE.ca)

    cat("\n**********************\n")

    cat("Estimates of alpha and beta for controls>>>\n")
    print(est.MLE.co)

    cat("\n********************************************\n")
  }
  res=list(genoMat=genoMat, 
    theta.ca.hat=theta.ca.hat, 
    theta.co.hat=theta.co.hat, 
    est.MLE.ca=est.MLE.ca,
    est.MLE.co=est.MLE.co
  )
  invisible(res)
}

# simulate data in - group
simFunc.n2=function(
  nCases=100,
  nControls=100,
  alpha.n.ca=2, beta.n.ca=8, 
  alpha.n.co=2, beta.n.co=3, nSNPs.n=100,
  low=0.02, upp=0.5, verbose=FALSE

)
{
  
  nTotal=nCases+nControls
  # generate
  theta.pVec.ca=rbeta(n=nSNPs.n, shape1=alpha.n.ca, shape2=beta.n.ca)
  theta.pVec.co=rbeta(n=nSNPs.n, shape1=alpha.n.co, shape2=beta.n.co)
  pos.t=which(theta.pVec.ca > theta.pVec.co)
  if(length(pos.t))
  {
    ca.tt=theta.pVec.ca[pos.t]
    co.tt=theta.pVec.co[pos.t]
    theta.pVec.ca[pos.t]=co.tt
    theta.pVec.co[pos.t]=ca.tt
  }  

  pos.del.ca=which(theta.pVec.ca<low | theta.pVec.ca>upp)
  pos.del.co=which(theta.pVec.co<low | theta.pVec.co>upp)
 
  pos.del2=unique(c(pos.del.ca, pos.del.co))
  if(length(pos.del2))
  {
    theta.pVec.ca2=theta.pVec.ca[-pos.del2]
    theta.pVec.co2=theta.pVec.co[-pos.del2]
  } else {
    theta.pVec.ca2=theta.pVec.ca
    theta.pVec.co2=theta.pVec.co
  }

  nSNPs.n2=length(theta.pVec.ca2)
  
  genoMat=matrix(NA, nrow=nSNPs.n2, ncol=nTotal)
  theta.ca.hat=rep(NA, nSNPs.n2)
  theta.co.hat=rep(NA, nSNPs.n2)
  
  for(g in 1:nSNPs.n2)
  {
    theta.n.ca=theta.pVec.ca2[g]
    theta.n.co=theta.pVec.co2[g]

    # mutation homozygote genotype
    p2.ca=theta.n.ca^2
    p2.co=theta.n.co^2
    
    # heterozygote genotype
    p1.ca=2*theta.n.ca*(1-theta.n.ca)
    p1.co=2*theta.n.co*(1-theta.n.co)
    
    # wildtype homozygote genotype
    p0.ca=(1-theta.n.ca)^2
    p0.co=(1-theta.n.co)^2
    
    x.ca=sample(x=c(0,1,2), size=nCases, replace=TRUE, 
      prob=c(p0.ca, p1.ca, p2.ca))
    x.co=sample(x=c(0,1,2), size=nControls, replace=TRUE, 
      prob=c(p0.co, p1.co, p2.co))
    x=c(x.ca, x.co)

    theta.ca.hat[g]=(2*sum(x.ca==2)+sum(x.ca==1))/(2*nTotal)
    theta.co.hat[g]=(2*sum(x.co==2)+sum(x.co==1))/(2*nTotal)
    genoMat[g,]=x
  }
  
  est.MLE.ca=est.alpha.beta.MLE(x=theta.ca.hat)

  est.MLE.co=est.alpha.beta.MLE(x=theta.co.hat)

  #####
  if(verbose)
  {
    cat("\n********************************************\n")
    cat("Estimates of alpha and beta for cases>>>\n")
    print(est.MLE.ca)

    cat("\n**********************\n")

    cat("Estimates of alpha and beta for controls>>>\n")
    print(est.MLE.co)

    cat("\n********************************************\n")
  }
  res=list(genoMat=genoMat, 
    theta.ca.hat=theta.ca.hat, 
    theta.co.hat=theta.co.hat, 
    est.MLE.ca=est.MLE.ca,
    est.MLE.co=est.MLE.co
  )
  invisible(res)
}

# simulate data (case & control having different prior parameters 
# in SNP cluster + and -)
simGenoFuncDiffPriors=function(
  nCases=100,
  nControls=100,
  nSNPs=1000,
  alpha.p.ca=2, beta.p.ca=3, 
  alpha.p.co=2, beta.p.co=8, pi.p=0.1,
  alpha0=2, beta0=5, pi0=0.8,
  alpha.n.ca=2, beta.n.ca=8, 
  alpha.n.co=2, beta.n.co=3, pi.n=0.1,
  low=0.02, upp=0.5, verbose=FALSE
)
{
  nSNPs.p=ceiling(nSNPs*pi.p)
  nSNPs.n=ceiling(nSNPs*pi.n)
  nSNPs0=nSNPs-nSNPs.p-nSNPs.n

  genoMat.p=simFunc.p2(
    nCases=nCases,
    nControls=nControls,
    alpha.p.ca=alpha.p.ca, beta.p.ca=beta.p.ca, 
    alpha.p.co=alpha.p.co, beta.p.co=beta.p.co, 
    nSNPs.p=nSNPs.p,
    low=low, upp=upp, verbose=FALSE
  )$genoMat


  genoMat0=simFunc0(
    nCases=nCases,
    nControls=nControls,
    alpha0=alpha0, beta0=beta0, nSNPs0=nSNPs0,
    low=low, upp=upp, verbose=FALSE
  )$genoMat

  genoMat.n=simFunc.n2(
    nCases=nCases,
    nControls=nControls,
    alpha.n.ca=alpha.n.ca, beta.n.ca=beta.n.ca, 
    alpha.n.co=alpha.n.co, beta.n.co=beta.n.co, nSNPs.n=nSNPs.n,
    low=low, upp=upp, verbose=FALSE
  )$genoMat

  genoMat=rbind(genoMat.p, genoMat0)
  genoMat=rbind(genoMat, genoMat.n)
 
  nSNPs.p2=nrow(genoMat.p)
  nSNPs02=nrow(genoMat0)
  nSNPs.n2=nrow(genoMat.n)
  nSNPs2=nSNPs.p2+nSNPs02+nSNPs.n2

  memGenes=c(rep(1, nSNPs.p2), rep(0, nSNPs02), rep(-1, nSNPs.n2))
  memGenes2=as.numeric(memGenes != 0)
  memSubjs=c(rep(1, nCases), rep(0, nControls))

  name.snps=paste("snp", 1:nSNPs2, sep="")
  nTotal=nCases+nControls
  name.subjs=paste("subj", 1:nTotal, sep="")

  rownames(genoMat)=name.snps
  colnames(genoMat)=name.subjs

  pDat=data.frame(sID=name.subjs, memSubjs=memSubjs)
  rownames(pDat)=name.subjs

  fDat=data.frame(snp=name.snps, 
    gene=paste("gene", 1:nSNPs2, sep=""),
    chr=rep(1, nSNPs2), memGenes=memGenes, memGenes2=memGenes2)
  rownames(fDat)=name.snps

  es=genExprSet2(ex=genoMat, pDat=pDat, fDat=fDat)
  
  invisible(es)
}
