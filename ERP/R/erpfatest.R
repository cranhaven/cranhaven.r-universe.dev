erpfatest <-
function (dta, design, design0 = NULL, 
    method = c("BH", "holm", "hochberg","hommel", "bonferroni", "BY", "fdr", "none"), 
    nbf = NULL,nsamples=200,significance=c("Satterthwaite","none"),
    nbfmax = min(c(nsamples,nrow(design)))-ncol(design)-1,
    alpha = 0.05, pi0 = 1, wantplot = ifelse(is.null(nbf),TRUE,FALSE), s0 = NULL, 
    min.err = 1e-02, maxiter = 5, verbose = FALSE,svd.method=c("fast.svd","irlba")) {
    
fastiSB = function(mPsi, lB) {
  nbdta = nrow(mPsi)
  m = ncol(mPsi)
  nbf = ncol(lB[[1]])
  lbeta = lapply(1:nbdta,function(i,mPsi,lB,nbf) lB[[i]]/(sqrt(as.vector(mPsi[i,]))%*%t(rep(1,nbf))),mPsi=mPsi,lB=lB,nbf=nbf)
  lsvdBeta = lapply(lbeta,fast.svd)
  ltheta = lapply(lsvdBeta,function(s,m) (s$u*(rep(1,m)%*%t(s$d/(1+s$d^2))))%*%t(s$v),m=m)
  liSB = lapply(1:nbdta,function(i,lt,mPsi,nbf) lt[[i]]/(sqrt(as.vector(mPsi[i,]))%*%t(rep(1,nbf))),lt=ltheta,mPsi=mPsi,nbf=nbf)
  return(list(iSB=liSB,svdBeta=lsvdBeta))
}

fastfa = function(ldta, nbf, min.err = 1e-02, verbose = FALSE,svd.method=c("fast.svd","irlba")) {  # data are centered, their type is matrix, nbf cannot be zero
   m = ncol(ldta[[1]])
   n = nrow(ldta[[1]])
   nbdta = length(ldta)
   vdta = matrix(unlist(lapply(ldta,function(dta,n) (crossprod(rep(1, n),dta^2)/n),n=n)),nrow=nbdta,byrow=TRUE)
   sddta = sqrt(n/(n - 1)) * sqrt(vdta)
   ltdta = lapply(ldta,t)
   if (svd.method=="fast.svd") lsvddta = lapply(ltdta,function(x,n) fast.svd(x/sqrt(n-1)),n=n)
   if (svd.method=="irlba") lsvddta = lapply(ltdta,function(x,n,nbf) irlba(x/sqrt(n-1),nu=nbf),n=n,nbf=nbf)
   lB = lapply(lsvddta,function(s,nbf,m) s$u[, 1:nbf, drop=FALSE]*tcrossprod(rep(1,m),s$d[1:nbf]),nbf=nbf,m=m)
   lB2 = lapply(lB,function(b,nbf) (b^2 %*% rep(1, nbf))[, 1],nbf=nbf)
   mB2 = matrix(unlist(lB2),nrow=length(ldta),byrow=TRUE)
   mPsi = sddta^2 - mB2
   crit = rep(1,length(ldta))
   mPsi[mPsi<=1e-16] = 1e-16
   while (max(crit) > min.err) {
      liSB = fastiSB(mPsi,lB)
      lxiSB = Map(crossprod,ltdta,liSB$iSB)
      lCyz = Map(function(x,y,n) crossprod(y,x)/(n-1),y=ldta,x=lxiSB,n=n)
      lCzz1 = Map(crossprod,liSB$iSB,lCyz) 
      lCzz2 = Map(crossprod,lB,liSB$iSB)
      lCzz2 = lapply(lCzz2,function(M,nbf) diag(nbf)-M,nbf=nbf) 
      lCzz = Map("+",lCzz1,lCzz2)
      liCzz = lapply(lCzz,solve)
      lBnew = Map(tcrossprod,lCyz,liCzz) 
      lB2 = lapply(lBnew,function(b,nbf) (b^2 %*% rep(1, nbf))[, 1],nbf=nbf)
      mB2 = matrix(unlist(lB2),nrow=length(ldta),byrow=TRUE)
      mPsinew = sddta^2 - mB2
      crit = ((mPsi - mPsinew)^2)%*%rep(1,m)/m
      lB = lBnew
      mPsi = mPsinew
      mPsi[mPsi<=1e-16] = 1e-16
      if (verbose) print(paste("Convergence criterion: ",signif(max(crit),digits=ceiling(-log10(min.err))),sep=""))
   }
   liSB = fastiSB(mPsi,lB)
   res = list(B = lB, Psi = mPsi, svdbeta = liSB$svdBeta)
   return(res)
}

fstat = function(erpdta,design,design0) {
  n = nrow(erpdta)
  idsignal = NULL
  for (j in 1:ncol(design)) {
    cj = apply(design0, 2, function(x, y) all(x == y), y = design[,j])
    if (all(!cj)) 
      idsignal = c(idsignal, j)
  }
  svd.design = fast.svd(design)
  u = svd.design$u
  iD = matrix(1/svd.design$d,nrow=1)
  viD = svd.design$v*(rep(1,nrow(svd.design$v))%*%iD)
  viDtu = tcrossprod(u,viD)
  beta = crossprod(viDtu,erpdta)
  beta = beta[idsignal,,drop=FALSE]
  svd.design0 = fast.svd(design0)
  u0 = svd.design0$u
  tuy = crossprod(u,erpdta)
  tu0y = crossprod(u0,erpdta)
  fit = u%*%tuy
  fit0 = u0%*%tu0y
  res = erpdta-fit
  res0 = erpdta-fit0
  rdf1 = nrow(design) - length(svd.design$d)
  rdf0 = nrow(design0) - length(svd.design0$d)
  rss1 = (t(rep(1, n)) %*% res^2)[1,]
  rss0 = (t(rep(1, n)) %*% res0^2)[1,]
  F = ((rss0 - rss1)/(rdf0 - rdf1))/(rss1/rdf1)
  return(list(F=F,beta=beta,residuals=res,rdf0=rdf0,rdf1=rdf1))
}

pval.fstat = function(F,erpdta,design,design0,nsamples) {
  n = nrow(erpdta)
  svd.design = fast.svd(design)
  svd.design0 = fast.svd(design0)
  u = svd.design$u
  u0 = svd.design0$u
  rdf1 = nrow(design) - length(svd.design$d)
  rdf0 = nrow(design0) - length(svd.design0$d)
  lsamples = lapply(1:nsamples,function(i,n) sample(1:n),n=n)
  lu = lapply(lsamples,function(s,d) d[s,],d=u)
  ltu = lapply(lu,t)
  ltuy = lapply(lu,function(u,y) crossprod(u,y),y=erpdta)
  lfit = Map(crossprod,ltu,ltuy)
  lres = lapply(lfit,function(fit,y) y-fit,y=erpdta)
  lu0 = lapply(lsamples,function(s,d) d[s,],d=u0)
  ltu0 = lapply(lu0,t)
  ltu0y = lapply(lu0,function(u,y) crossprod(u,y),y=erpdta)
  lfit0 = Map(crossprod,ltu0,ltu0y)
  lres0 = lapply(lfit0,function(fit,y) y-fit,y=erpdta)
  lrss1 = lapply(lres,function(res,n) as.vector(t(rep(1, n)) %*% res^2),n=n) 
  lrss0 = lapply(lres0,function(res,n) as.vector(t(rep(1, n)) %*% res^2),n=n) 
  lf0 = Map(function(rss1,rss0,rdf1,rdf0) {
    ((rss0 - rss1)/(rdf0 - rdf1))/(rss1/rdf1)
  },lrss1,lrss0,rdf1=rdf1,rdf0=rdf0)
  mf0 = matrix(unlist(lf0),nrow=nsamples,byrow=TRUE)
  varf0 = apply(mf0,2,var)
  meanf0 = apply(mf0,2,mean)
  const = varf0/(2*meanf0)
  nu = 2*meanf0^2/varf0
  pval = pchisq(F/const,df=nu,lower.tail=FALSE)
  return(pval)
}

update.beta = function(erpdta,design,design0,nbf,fs0,min.err,verbose) {
  n = nrow(erpdta)
  idsignal = NULL
  for (j in 1:ncol(design)) {
    cj = apply(design0, 2, function(x, y) all(x == y), y = design[,j])
    if (all(!cj)) 
      idsignal = c(idsignal, j)
  }
  svd.design = fast.svd(design)
  u = svd.design$u
  R = diag(ncol(design))
  R = R[idsignal, ,drop=FALSE]
  iD = matrix(1/svd.design$d,nrow=1)
  viD = svd.design$v*(rep(1,nrow(svd.design$v))%*%iD)
  viDtu = tcrossprod(u,viD)
  coef1 = crossprod(viDtu,erpdta)
  beta = coef1[idsignal,,drop=FALSE]
  itxx = tcrossprod(viD) 
  svd.designR = fast.svd(R%*%viD)
  RiD = matrix(1/svd.designR$d,nrow=1)
  RuiD = svd.designR$u*(rep(1,nrow(svd.designR$u))%*%RiD)
  iRitxxtR = tcrossprod(RuiD)
  fit = design%*%coef1
  if (length(fs0)>0) {
     res = erpdta-fit   
     meanres = (crossprod(rep(1,n),res)/n)[1,]
     cres = res-rep(1,n)%*%t(meanres)
     fa = emfa(cres,nbf=nbf,min.err=min.err,verbose=verbose,svd.method=svd.method)
     Psi = fa$Psi
     B = fa$B
     B0 = B[fs0, ,drop=FALSE]
     iSxx = ifa(Psi[fs0], B0)$iS
     Sxy = tcrossprod(B0,B[-fs0, , drop=FALSE])
     betaSigma0 = iSxx %*% Sxy
     beta0 = beta
     if (length(fs0)<ncol(erpdta)) beta0[, -fs0] = beta[, fs0,drop=FALSE] %*% betaSigma0
     beta = beta - beta0
     Rcoef = R%*%coef1
     epsilon = beta-Rcoef
     M = itxx %*% t(R) %*% iRitxxtR
     Mepsilon = M%*%epsilon
     coef1 = coef1+Mepsilon
     fit = design%*%coef1
  }
  res = erpdta-fit   
  meanres = (crossprod(rep(1,n),res)/n)[1,]
  cres = res-rep(1,n)%*%t(meanres)
  sdres = sqrt(crossprod(rep(1,n),cres^2)/(n-1))[1,] 
  scres = cres/tcrossprod(rep(1,n),sdres)
  fa = emfa(scres,nbf=nbf,min.err=min.err,verbose=verbose,svd.method=svd.method)
  Psi = fa$Psi
  B = fa$B
  sB = t(B)/tcrossprod(rep(1,nbf),sqrt(Psi))
  G = solve(diag(nbf)+tcrossprod(sB))
  sB = t(B)/tcrossprod(rep(1,nbf),Psi)
  GsB = crossprod(G,sB)
  Factors = tcrossprod(scres,GsB)
  designz0 = cbind(design0,Factors)
  designz1 = cbind(designz0,design[,idsignal,drop=FALSE])
  svd.designz0 = fast.svd(designz0)
  svd.designz1 = fast.svd(designz1)
  uz0 = svd.designz0$u
  uz1 = svd.designz1$u
  vz0 = svd.designz0$v
  vz1 = svd.designz1$v
  dz0 = svd.designz0$d
  dz1 = svd.designz1$d
  rdfz0 = nrow(designz0) - length(dz0)
  rdfz1 = nrow(designz1) - length(dz1)
  vz1id = vz1/(rep(1,ncol(designz1))%*%t(dz1))
  pdesignz1 = tcrossprod(vz1id,uz1)
  coefz1 = pdesignz1%*%erpdta
  vz0id = vz0/(rep(1,ncol(designz0))%*%t(dz0))
  pdesignz0 = tcrossprod(vz0id,uz0)
  coefz0 = pdesignz0%*%erpdta
  idsignalz1 = (ncol(designz1)-length(idsignal)+1):ncol(designz1)
  fitz1 = designz1%*%coefz1
  resz1 = erpdta-fitz1
  rssz1 = (t(rep(1,n))%*%resz1^2)[1,]
  fitz0 = designz0%*%coefz0
  resz0 = erpdta-fitz0
  rssz0 = (t(rep(1,n))%*%resz0^2)[1,]
  F = ((rssz0 - rssz1)/(rdfz0 - rdfz1))/(rssz1/rdfz1)
  F = ksmooth(1:T,F,bandwidth = 0.01 * diff(range(1:T)),x.points = 1:T)$y
  return(list(F=F,residuals=resz1,rdf0=rdfz0,rdf1=rdfz1,beta=coefz1[idsignalz1,,drop=FALSE]))
}

pval.fstatz = function(F,erpdta,design,design0,nbf,fs0,nsamples,min.err,verbose) { 
  n = nrow(erpdta)
  idsignal = NULL
  for (j in 1:ncol(design)) {
    cj = apply(design0, 2, function(x, y) all(x == y), y = design[,j])
    if (all(!cj)) 
      idsignal = c(idsignal, j)
  }
  R = diag(ncol(design))
  R = R[idsignal, ,drop=FALSE]
  svd.design = fast.svd(design)
  u = svd.design$u
  lsamples = lapply(1:nsamples,function(i,n) sample(1:n),n=n)
  lu = lapply(lsamples,function(s,u) u[s,],u=u)
  iD = matrix(1/svd.design$d,nrow=1)
  viD = svd.design$v*(rep(1,nrow(svd.design$v))%*%iD)
  itxx = tcrossprod(viD) 
  svd.designR = fast.svd(R%*%viD)
  RiD = matrix(1/svd.designR$d,nrow=1)
  RuiD = svd.designR$u*(rep(1,nrow(svd.designR$u))%*%RiD)
  iRitxxtR = tcrossprod(RuiD)
  lviDtu = lapply(lu,function(u,m) tcrossprod(u,m),m=viD)
  lcoef1 = lapply(lviDtu,function(u,y) crossprod(u,y),y=erpdta)
  lbeta = lapply(lcoef1,function(beta,id) beta[id,,drop=FALSE],id=idsignal)
  ldesign = lapply(lsamples,function(s,design) design[s,],design=design)
  ldesign0 = lapply(lsamples,function(s,design) design[s,,drop=FALSE],
                    design=design[,-idsignal,drop=FALSE])
  lfit = Map("%*%",ldesign,lcoef1)
  if (length(fs0)>0) {
     lres = lapply(lfit,function(fit,y) y-fit,y=erpdta)   
     lmeanres = lapply(lres,function(res,n) (crossprod(rep(1,n),res)/n)[1,],n=n)
     lcres = Map(function(res,meanres,n) res-rep(1,n)%*%t(meanres),lres,lmeanres,n=n)
     lfa = fastfa(lcres,nbf=nbf,min.err=min.err,verbose=FALSE,svd.method=svd.method)
     lPsi = as.list(data.frame(t(lfa$Psi)))
     lB = lfa$B
     lB0 = lapply(lB,function(B,fs0) B[fs0, ,drop=FALSE],fs0=fs0)
     lB1 = lapply(lB,function(B,fs0) B[-fs0, ,drop=FALSE],fs0=fs0)
     lPsi0 = lapply(lPsi,function(Psi,fs0) Psi[fs0],fs0=fs0)
     liSxx = Map(function(Psi,B) ifa(Psi,B)$iS,lPsi0,lB0)
     lSxy = Map(tcrossprod,lB0,lB1)
     lbetaSigma0 = Map(crossprod,liSxx,lSxy)
     lbeta0 = lbeta
     lbeta0c = lapply(lbeta0,function(beta0,fs0) beta0[, fs0,drop=FALSE],fs0=fs0)
     lbeta0 = lapply(lbeta0,function(beta0,fs0) {
        res = beta0
        res[,-fs0] = 0
        return(res)
     },fs0=fs0)
     lbeta0c = Map("%*%",lbeta0c,lbetaSigma0)
     lbeta0c = lapply(lbeta0c,function(beta0c,fs0,T,p) {
        res = matrix(0,nrow=p,ncol=T)
        res[,-fs0] = beta0c
        return(res)
     },fs0=fs0,T=T,p=length(idsignal))
     lbeta0 = Map("+",lbeta0,lbeta0c)
     lbeta = Map("-",lbeta,lbeta0)
     lRcoef1 = lapply(lcoef1,function(b,R) R%*%b,R=R)
     lepsilon = Map("-",lbeta,lRcoef1)
     M = itxx %*% t(R) %*% iRitxxtR
     lMepsilon = lapply(lepsilon,function(epsilon,M) M%*%epsilon,M=M)
     lcoef1 = Map("+",lcoef1,lMepsilon)
     lfit = Map("%*%",ldesign,lcoef1)
  }   
  lres = lapply(lfit,function(fit,y) y-fit,y=erpdta)   
  lmeanres = lapply(lres,function(res,n) (crossprod(rep(1,n),res)/n)[1,],n=n)
  lcres = Map(function(res,meanres,n) res-rep(1,n)%*%t(meanres),lres,lmeanres,n=n)
  lsdres = lapply(lcres,function(res,n) sqrt(crossprod(rep(1,n),res^2)/(n-1))[1,],n=n) 
  lscres = Map(function(cres,sdres,n) cres/tcrossprod(rep(1,n),sdres),lcres,lsdres,n=n)
  lfa = fastfa(lscres,nbf=nbf,min.err=min.err,verbose=FALSE,svd.method=svd.method)
  lPsi = as.list(data.frame(t(lfa$Psi)))
  lB = lfa$B
  lsB = Map(function(B,Psi) t(B)/tcrossprod(rep(1,ncol(B)),sqrt(Psi)),lB,lPsi)
  lG = lapply(lsB,function(sb,nbf) solve(diag(nbf)+tcrossprod(sb)),nbf=nbf)
  lsB = Map(function(B,Psi) t(B)/tcrossprod(rep(1,ncol(B)),Psi),lB,lPsi)
  lGsB = Map(crossprod,lG,lsB)
  lFactors = Map(tcrossprod,lscres,lGsB)
  ldesignz0 = Map(function(design0,Factors) cbind(design0,Factors),ldesign0,lFactors)
  ldesign1 = lapply(lsamples,function(s,design) design[s,,drop=FALSE],design=design[,idsignal,drop=FALSE])
  ldesignz1 = Map(function(designz0,design1) cbind(designz0,design1),ldesignz0,ldesign1)
  lsvd.designz0 = lapply(ldesignz0,fast.svd)
  lsvd.designz1 = lapply(ldesignz1,fast.svd)
  luz0 = lapply(lsvd.designz0,function(x) x$u)
  luz1 = lapply(lsvd.designz1,function(x) x$u)
  lvz0 = lapply(lsvd.designz0,function(x) x$v)
  lvz1 = lapply(lsvd.designz1,function(x) x$v)
  ldz0 = lapply(lsvd.designz0,function(x) x$d)
  ldz1 = lapply(lsvd.designz1,function(x) x$d)
  rdfz0 = nrow(ldesignz0[[1]]) - length(ldz0[[1]])
  rdfz1 = nrow(ldesignz1[[1]]) - length(ldz1[[1]])
  lvz1id = Map(function(v,d,p) v/(rep(1,p)%*%t(d)),lvz1,ldz1,p=nbf+ncol(design))
  lpdesignz1 = Map(tcrossprod,lvz1id,luz1)
  lcoefz1 = lapply(lpdesignz1,function(pdesign,y) pdesign%*%y,y=erpdta)
  lvz0id = Map(function(v,d,p) v/(rep(1,p)%*%t(d)),lvz0,ldz0,p=nbf+ncol(design0))
  lpdesignz0 = Map(tcrossprod,lvz0id,luz0)
  lcoefz0 = lapply(lpdesignz0,function(pdesign,y) pdesign%*%y,y=erpdta)
  idsignalz1 = (ncol(ldesignz1[[1]])-length(idsignal)+1):ncol(ldesignz1[[1]])
  lfitz1 = Map("%*%",ldesignz1,lcoefz1)
  lresz1 = lapply(lfitz1,function(fit,y) y-fit,y=erpdta)
  lrssz1 = lapply(lresz1,function(res,n) (t(rep(1,n))%*%res^2)[1,],n=n)
  lfitz0 = Map("%*%",ldesignz0,lcoefz0)
  lresz0 = lapply(lfitz0,function(fit,y) y-fit,y=erpdta)
  lrssz0 = lapply(lresz0,function(res,n) (t(rep(1,n))%*%res^2)[1,],n=n)
  lfz0 = Map(function(rss1,rss0,rdf1,rdf0) {
    ((rss0 - rss1)/(rdf0 - rdf1))/(rss1/rdf1)
  },lrssz1,lrssz0,rdf1=rdfz1,rdf0=rdfz0)
  mfz0 = matrix(unlist(lfz0),nrow=nsamples,byrow=TRUE)
  varfz0 = apply(mfz0,2,var) 
  meanfz0 = apply(mfz0,2,mean)
  constz = varfz0/(2*meanfz0)
  nuz = 2*meanfz0^2/varfz0
  pvalz = pchisq(F/constz,df=nuz,lower.tail=FALSE)
  return(pvalz)
}

    method = match.arg(method, choices = c("BH", "holm", "hochberg", 
        "hommel", "bonferroni", "BY", "fdr", "none"))
    significance = match.arg(significance,c("Satterthwaite","none"))
    svd.method = match.arg(svd.method,choices=c("fast.svd","irlba"))
    if (typeof(nsamples) != "double") 
      stop("nsamples sould be an integer, usually larger than 200.")
    if (is.null(design0)) 
        design0 = matrix(1, nrow = nrow(dta), ncol = 1)
    erpdta = as.matrix(dta)
    design = as.matrix(design)
    design0 = as.matrix(design0)
    if (typeof(erpdta) != "double") 
        stop("ERPs should be of type double")
    if (nrow(erpdta) != nrow(design)) 
        stop("dta and design should have the same number of rows")
    if (nrow(erpdta) != nrow(design0)) 
        stop("dta and design0 should have the same number of rows")
    if (ncol(design) <= ncol(design0)) 
        stop("design0 should have fewer columns than design")
    idsignal = NULL
    for (j in 1:ncol(design)) {
        cj = apply(design0, 2, function(x, y) all(x == y), y = design[, 
            j])
        if (all(!cj)) 
            idsignal = c(idsignal, j)
    }
    if (length(idsignal) < (ncol(design) - ncol(design0))) 
        stop("the null model design0 should be nested into the non-null model design")
    if (typeof(alpha) != "double") 
        stop("alpha should be of type double")
    if ((alpha <= 0) | (alpha >= 1)) 
        stop("alpha should be in ]0,1[, typically 0.05")
    if (typeof(pi0) != "double") 
        stop("pi0 should be of type double")
    if ((pi0 <= 0) | (pi0 > 1)) 
        stop("pi0 should be in ]0,1]")
    if (length(s0) == 1) 
        stop("s0 should be either NULL, or of length larger than 2")
    frames = 1:ncol(erpdta)
    if (is.null(s0)) 
        fs0i = integer(0)
    if (length(s0) > 2) 
        fs0i = s0
    if (length(s0) == 2) 
        fs0i = which((frames <= s0[1] * diff(range(frames))) | 
            (frames >= s0[2] * diff(range(frames))))
    nbfmaxtheo = min(c(nrow(design),nsamples))-ncol(design)-1
    if (sum(is.element(nbfmax, 0:nbfmaxtheo)) != 1) {
      warning(paste("nbfmax should be an integer in [0,", nbfmaxtheo,"]", sep = ""))
      nbfmax = nbfmaxtheo
    }
    
    n = nrow(erpdta)
    T = ncol(erpdta)
    pval = NULL
    qval = NULL
    correctedpval=NULL
    significant=integer(0)
    test = NULL
    r2 = NULL
    p0 = 1
    rdf1 = NULL
    rdf0 = NULL
    beta = NULL
    test = NULL

    if (is.null(nbf)) {
      svd.design = fast.svd(design)
      svd.design0 = fast.svd(design0)
      P0 = diag(n)-svd.design0$u%*%t(svd.design0$u)
      Z = design[,idsignal,drop=FALSE]
      cZ = P0%*%Z
      Szz = t(cZ)%*%cZ/n   
      svdcz = fast.svd(cZ)
      if (length(idsignal)>1) sqrtcz = svdcz$v%*%diag(svdcz$d)%*%t(svdcz$v)
      if (length(idsignal)==1) sqrtcz = svdcz$v%*%t(svdcz$v)*svdcz$d
      vid = svd.design$v%*%diag(1/svd.design$d)
      lsamples = lapply(1:nsamples,function(i,n) sample(1:n),n=n)
      lu = lapply(lsamples,function(s,d) d[s,],d=svd.design$u)
      ltu = lapply(lu,t)
      ltuy = lapply(lu,function(u,y) crossprod(u,y),y=erpdta)
      lfit = Map(crossprod,ltu,ltuy)
      lres = lapply(lfit,function(fit,y) y-fit,y=erpdta)
      lsdres = lapply(lres,function(res,n) sqrt(crossprod(rep(1,n),res^2)/(n-1))[1,],n=n) 
      lmsdres = lapply(lsdres,function(sdres,p) tcrossprod(rep(1,p),sdres),p=length(idsignal))
      lbeta.ols = lapply(ltuy,function(tuy,m,select) crossprod(m,tuy)[select,,drop=FALSE],m=t(vid),select=idsignal)
      lb.ols = lapply(lbeta.ols,function(beta,m) crossprod(m,beta),m=t(sqrtcz))
      lb.ols = Map("/",lb.ols,lmsdres)
      mb.ols = lapply(lb.ols,function(b,p) crossprod(rep(1,p),b)/p,p=length(idsignal))
      mb.ols = matrix(unlist(mb.ols),ncol=T,byrow=TRUE)
      meanmb.ols = (t(rep(1,nsamples))%*%mb.ols)/nsamples
      cmb.ols = mb.ols-rep(1,nsamples)%*%meanmb.ols
      sdmb.ols = sqrt(t(rep(1,nsamples))%*%cmb.ols^2/(nsamples-1))
      scmb.ols = cmb.ols/(rep(1,nsamples)%*%sdmb.ols)
      nbf = nbfactors(scmb.ols,maxnbfactors=nbfmax,diagnostic.plot=wantplot,verbose=verbose,min.err=min.err,svd.method="irlba")$optimalnbfactors
    }
    
    if (significance=="Satterthwaite") {
      F = fstat(erpdta,design=design,design0=design0)
      res = F$residuals
      sdres = sqrt((t(rep(1,n))%*%res^2)[1,]/(n-1))
      scres = res/(rep(1,n)%*%t(sdres))
      beta = F$beta
      F = F$F
      pval = pval.fstat(F,erpdta,design,design0,nsamples)
      if (is.null(pi0)) 
        p0 = pval.estimate.eta0(pval, diagnostic.plot = FALSE)
      qval = p0 * p.adjust(pval, method = method)
      fs0 = sort(unique(c(fs0i, which(pval > 0.2))))
      if (verbose) 
        print(paste("AFA with", nbf, "factors"))
      if ((nbf > 0) & (maxiter > 0)) {
         diff.fs0 = length(setdiff(fs0,integer(0)))/length(union(fs0,integer(0)))
         fs1 = fs0
         iter = 0
         while ((diff.fs0>0.05) & (iter < maxiter)) {
           iter = iter + 1
           if (verbose) 
           print(paste(iter, "/", maxiter, " iterations",sep = ""))
           upd = update.beta(erpdta,design,design0,nbf=nbf,fs0=fs0,min.err=min.err,verbose=FALSE)  
           F = upd$F
           rdf0 = upd$rdf0
           rdf1 = upd$rdf1
           if (length(fs0)<T) 
             pval = pval.fstatz(F,erpdta,design,design0,nbf,fs0,nsamples,min.err,verbose=FALSE)
           if (is.null(pi0)) 
             p0 = pval.estimate.eta0(pval, diagnostic.plot = FALSE)
           qval = p0 * p.adjust(pval, method = method) 
           fs0 = which(pval > 0.2)
           diff.fs0 = length(setdiff(fs0,fs1))/length(union(fs0,fs1))
           fs1 = fs0
           if (verbose) print(paste("Convergence criterion: ",diff.fs0,". Tolerance: 0.05",sep=""))
           beta = upd$beta
         }
       }
       significant = which(qval <= alpha)
       test = F
       r2 = (1 - 1/(1 + F * ((rdf0 - rdf1)/rdf1)))
       if (length(idsignal)==1) test = sign(beta[1,])*sqrt(F)
    }
    list(pval = pval, correctedpval = qval, significant = significant, 
      pi0 = p0, test = test, df1 = rdf1, df0 = rdf0, 
      nbf = nbf,signal = beta, r2=r2)
}
