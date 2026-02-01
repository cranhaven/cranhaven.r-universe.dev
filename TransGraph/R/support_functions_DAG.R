Compute.precision.trans = function(t.data, A.data, comp.hardth=0.3,
                                   precision.method="glasso", cov.method="opt",
                                   cn.lam2=0.5, theta.algm="cd", adjust.BIC=F,
                                   precision.refit = F, ini.prec=T, correlation=T,
                                   preselect.aux=0, sel.type="L2"){

  n = nrow(t.data)
  p = ncol(t.data)

  trans.precision = trans_precision(t.data, A.data, precision.method=precision.method,
                                    cov.method=cov.method, cn.lam2=cn.lam2, correlation=correlation,
                                    theta.algm=theta.algm, adjust.BIC=adjust.BIC,
                                    preselect.aux=preselect.aux, sel.type=sel.type)
  Theta.hat = trans.precision$Theta.hat
  k.check = trans.precision$k.check
  N = trans.precision$N

  PreciMat = Theta.hat
  stdPreciMat = t(PreciMat/diag(PreciMat))
  stdPreciMat.trans = stdPreciMat

  if(ini.prec){
    Theta.hat0 = trans.precision$Theta.hat0
    PreciMat = Theta.hat0
    stdPreciMat = t(PreciMat/diag(PreciMat))
    if(precision.refit){
      stdPreciMat = Compute.stdPreciMat.refit.high(t.data,stdPreciMat,comp.hardth)
      stdPreciMat[abs(stdPreciMat)<comp.hardth] = 0
    }
    stdPreciMat.int = stdPreciMat
  } else {
    stdPreciMat.int = NULL
  }

  PreciMat.list = list(stdPreciMat.trans=stdPreciMat.trans,
                       stdPreciMat.int=stdPreciMat.int,
                       Theta.hat = Theta.hat,
                       Theta.hat0 = Theta.hat0,
                       k.check = k.check, N=N)
  return(PreciMat.list)
}

Compute.dcor.est.trans = function(index.Y, preci.coeff, comp.X){

  p=ncol(comp.X)
  comp.resid=comp.X%*%preci.coeff
  other.X=comp.X[,-index.Y]
  if (p>2){
    comp.dcov=sapply(1:(p-1),function(l) dcov::dcov2d(comp.resid, other.X[,l]))
  } else{
    comp.dcov=dcov::dcov2d(comp.resid, other.X)
  }

  return(dcov.max=max(comp.dcov))
}

Compute.dcor.test.trans = function(index.Y, preci.coeff, comp.X){

  p=ncol(comp.X)
  comp.resid=comp.X%*%preci.coeff
  other.X=comp.X[,-index.Y]
  if (p>2){
    comp.dcor.index = 1:ncol(other.X)
    comp.dcor.test = dcov::dcor.test(comp.resid,other.X[,comp.dcor.index],R=100)$p.values
  } else{
    comp.dcor.test=dcov::dcor.test(comp.resid,other.X,R=100)$p.values
  }

  return(comp.dcor.test)
}

Compute.precision = function(comp.X, comp.hardth, precision.refit = T, precision.method="glasso"){

  n=nrow(comp.X)
  p=ncol(comp.X)

  if(precision.method=="glasso"){
    PreciMat = huge::huge(comp.X, lambda = 0.5*sqrt(log(max(p,n))/n), method = "glasso", verbose = FALSE)$icov[[1]]
  }
  if(precision.method=="CLIME"){
    PreciMat = clime::clime(comp.X,lambda=sqrt(log(max(p,n))/n))$Omegalist[[1]]
    # PreciMat = huge::huge(comp.X, lambda = 0.5*sqrt(log(max(p,n))/n), method = "glasso", verbose = FALSE)$icov[[1]]
    # warning("Note: CLIME cannot be used for initialization due to the removal of R package 'fastclime' from the CRAN repository. Initialization is still based on glasso.")
  }

  stdPreciMat=t(PreciMat/diag(PreciMat))
  if(precision.refit){
    stdPreciMat=Compute.stdPreciMat.refit.high(comp.X,stdPreciMat,comp.hardth)
    stdPreciMat[abs(stdPreciMat)<comp.hardth]=0
  }

  return(stdPreciMat)
}

Compute.stdPreciMat.refit.high = function(comp.X, pre_stdPreciMat, hardth=0.3){

  p=ncol(comp.X)
  stdPreciMat=matrix(0,p,p)
  diag(stdPreciMat)=1

  diag(pre_stdPreciMat)=0

  for (i in 1:p){
    index_regres=which(pre_stdPreciMat[,i]!=0)
    if(length(index_regres)<nrow(comp.X)){
      if (length(index_regres)!=0){
        pre.coeff=MASS::ginv(t(comp.X[,index_regres])%*%comp.X[,index_regres])%*%t(comp.X[,index_regres])%*%comp.X[,i]
        index_select=which(abs(pre.coeff)>hardth)
        if (length(index_select)!=0){
          X_regres=comp.X[,index_regres]
          if (is.matrix(X_regres)==T){
            select.coeff=MASS::ginv(t(X_regres[,index_select])%*%X_regres[,index_select])%*%t(X_regres[,index_select])%*%comp.X[,i]
            comp.coeff=rep(0,length(index_regres))
            comp.coeff[index_select]=select.coeff
            stdPreciMat[index_regres,i]=-comp.coeff
          } else {
            select.coeff=MASS::ginv(t(X_regres)%*%X_regres)%*%t(X_regres)%*%comp.X[,i]
            stdPreciMat[index_regres,i]=-select.coeff
          }

        }

      }
    }
  }
  return(stdPreciMat)
}

Compute.dcor = function(index.Y, preci.coeff, comp.X){

  p=ncol(comp.X)
  comp.resid=comp.X%*%preci.coeff
  other.X=comp.X[,-index.Y]
  if (p>2){
    comp.dcor.index = 1:ncol(other.X)
    comp.dcor.test = dcov::dcor.test(comp.resid,other.X[,comp.dcor.index],R=100)$p.values
  } else{
    comp.dcor.test=dcov::dcor.test(comp.resid,other.X,R=100)$p.values
  }
  return(comp.dcor.test)
}

Parent.set.refit = function(comp.X,pre_B){

  p=ncol(comp.X)
  B=matrix(0,p,p)

  for (i in 1:p){
    index=which(pre_B[,i]!=0)
    if(length(index)>0){
      comp.coeff=MASS::ginv(t(comp.X[,index])%*%comp.X[,index])%*%t(comp.X[,index])%*%comp.X[,i]
      B[index,i]=comp.coeff
    }

  }
  return(B)
}

hammingDistance=function(G1, G2){
  allMistakeOne=FALSE
  if(allMistakeOne){
    Gtmp=(G1+G2)%%2
    Gtmp=Gtmp+t(Gtmp)
    nrRevesals=sum(Gtmp==2)/2
    nrIncDel=sum(Gtmp==1)/2
    hammingDis=nrRevesals+nrIncDel

  }else{
    hammingDis=sum(abs(G1-G2))
    hammingDis=hammingDis-0.5*sum(G1*t(G1)*(1-G2)*t(1-G2)+G2*t(G2)*(1-G1)*t(1-G1))
  }
  return(hammingDis)
}


