erpFtest <-
function(dta,design,design0=NULL,nbf=NULL,pvalue=c("Satterthwaite","MC","none"),nsamples=200,min.err=1e-02,verbose=FALSE,
   nbfmax=min(c(nsamples,nrow(design)))-ncol(design)-1,wantplot = ifelse(is.null(nbf),TRUE,FALSE),svd.method=c("fast.svd","irlba")) {

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
   
   if (is.null(design0)) 
        design0 = matrix(1, nrow = nrow(dta), ncol = 1)
   if (!is.logical(wantplot)) stop("wantplot should be logical")
   if (!is.logical(verbose)) stop("verbose should be logical")

   erpdta = as.matrix(dta)
   design = as.matrix(design)
   design0 = as.matrix(design0)

   pvalue = match.arg(pvalue,choices=c("Satterthwaite","MC","none"))  
   
   if (typeof(nsamples) != "double") 
     stop("nsamples sould be an integer, usually larger than 200.")
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
      cj = apply(design0, 2, function(x, y) all(x == y), y = design[,j])
      if (all(!cj)) idsignal = c(idsignal, j)
   }
   if (length(idsignal) < (ncol(design) - ncol(design0))) 
      stop("the null model design0 should be nested into the non-null model design")

   svd.method = match.arg(svd.method,choices=c("fast.svd","irlba"))  

   if ((pvalue=="Satterthwaite")&(nsamples<200)) stop("Since pvalue=Satterthwaite, the number of MC samples should be at least 200.") 
   
   n = nrow(erpdta)
   T = ncol(erpdta)

   nbfmaxtheo = min(c(nrow(design),nsamples))-ncol(design)-1
   if (sum(is.element(nbfmax, 0:nbfmaxtheo)) != 1) {
     warning(paste("nbfmax should be an integer in [0,", nbfmaxtheo,"]", sep = ""))
     nbfmax = nbfmaxtheo
   }
   
   svd.design = fast.svd(design)
   svd.design0 = fast.svd(design0)
   rdf1 = nrow(design) - length(svd.design$d)
   rdf0 = nrow(design0) - length(svd.design0$d)
   P0 = diag(n)-svd.design0$u%*%t(svd.design0$u)
   if (ncol(design)==1) pdesign = (svd.design$v%*%t(svd.design$u))/(svd.design$d[1])
   if (ncol(design)>1) pdesign = svd.design$v%*%diag(1/svd.design$d)%*%t(svd.design$u)    

   Z = design[,idsignal]
   if (length(idsignal)==1) Z = matrix(Z,ncol=1)
   cZ = P0%*%Z
   Szz = t(cZ)%*%cZ/n   
   svdcz = fast.svd(cZ)
   if (length(idsignal)>1) sqrtcz = svdcz$v%*%diag(svdcz$d)%*%t(svdcz$v)
   if (length(idsignal)==1) sqrtcz = svdcz$v%*%t(svdcz$v)*svdcz$d

   vid = svd.design$v%*%diag(1/svd.design$d)
   tuy = crossprod(svd.design$u,erpdta)
   beta.ols = vid%*%tuy
   fit = svd.design$u%*%tuy 
   beta.ols = beta.ols[idsignal,]
   if (length(idsignal)==1) beta.ols = matrix(beta.ols,nrow=1)
   res = erpdta - fit
   sdres = sqrt(t(rep(1,n))%*%res^2/(n-length(svd.design$d)))[1,]
   sigma = sqrt(mean(sdres^2))
   Phi = rep(1/sigma,T)
   Phibeta = beta.ols*(rep(1,nrow(beta.ols))%*%t(Phi))
   Phibetatcz = crossprod(Phibeta,t(cZ)) 
   Fols = sum(Phibetatcz^2)/(T*length(idsignal))
   b.ols = (sqrtcz%*%beta.ols)*(rep(1,length(idsignal))%*%t(1/sdres))
   pval.Fols = NULL
   scres = res/(rep(1,n)%*%t(sdres))*sqrt((n-1)/(n-length(svd.design$d)))

   utu = tcrossprod(svd.design$u)
   
   bool=FALSE
   if (is.null(nbf)) bool=TRUE
   if ((!is.null(nbf))) if (nbf>0) bool=TRUE
   if (pvalue!="none") bool=TRUE
   if (bool) {
    lsamples = lapply(1:nsamples,function(i,n) sample(1:n),n=n)
    lu = lapply(lsamples,function(s,d) d[s,],d=svd.design$u)
    ltu = lapply(lu,t)
    ltuy = lapply(lu,function(u,y) crossprod(u,y),y=erpdta)
    lfit = Map(crossprod,ltu,ltuy)
    lres = lapply(lfit,function(fit,y) y-fit,y=erpdta)
    lsdres = lapply(lres,function(res,n,ddlr) sqrt(crossprod(rep(1,n),res^2)/ddlr)[1,],n=n,ddlr=n-length(svd.design$d)) 
    lsigma = lapply(lsdres,function(sdres) rep(sqrt(mean(sdres^2)),length(sdres)))
    lmsdres = lapply(lsdres,function(sdres,p) tcrossprod(rep(1,p),sdres),p=length(idsignal))
    lmsigma = lapply(lsigma,function(sdres,p) tcrossprod(rep(1,p),sdres),p=length(idsignal))
    lbeta.ols = lapply(ltuy,function(tuy,m,select) crossprod(m,tuy)[select,,drop=FALSE],m=t(vid),select=idsignal)
    lb.ols = lapply(lbeta.ols,function(beta,m) crossprod(m,beta),m=t(sqrtcz))
    lbh.ols = Map("/",lb.ols,lmsigma)
    f0.ols = unlist(lapply(lbh.ols,function(b.ols) mean(b.ols^2)))
    if (pvalue=="MC") pval.Fols = mean(f0.ols>=Fols)
    if (pvalue=="Satterthwaite") {
      const = var(f0.ols)/(2*mean(f0.ols))
      nu = 2*mean(f0.ols)^2/var(f0.ols)
      pval.Fols = pchisq(Fols/const,df=nu,lower.tail=FALSE)
    }
    lb.ols = Map("/",lb.ols,lmsdres)
    f0 = NULL
   }
   
   if (is.null(nbf)) {
      mb.ols = lapply(lb.ols,function(b,p) crossprod(rep(1,p),b)/p,p=length(idsignal))
      mb.ols = matrix(unlist(mb.ols),ncol=T,byrow=TRUE)
      meanmb.ols = (t(rep(1,nsamples))%*%mb.ols)/nsamples
      cmb.ols = mb.ols-rep(1,nsamples)%*%meanmb.ols
      sdmb.ols = sqrt(t(rep(1,nsamples))%*%cmb.ols^2/(nsamples-1))
      scmb.ols = cmb.ols/(rep(1,nsamples)%*%sdmb.ols)
      nbf = nbfactors(scmb.ols,maxnbfactors=nbfmax,diagnostic.plot=wantplot,verbose=verbose,min.err=min.err,svd.method="irlba")$optimalnbfactors
   }

   if (nbf>0) {
      fa = emfa(scres,nbf=nbf,min.err=min.err,verbose=verbose,svd.method=svd.method)            
      Lambda = fa$B
      Psi = fa$Psi
      Phi = 1/sqrt(Psi)
      beta = Lambda*(Phi%*%t(rep(1,nbf))) 
      svdbeta = fast.svd(beta)
      theta = svdbeta$u*(rep(1,T)%*%t(svdbeta$d/sqrt(1+svdbeta$d^2)))
      Phibeta = b.ols*(rep(1,length(idsignal))%*%t(Phi))
      tthetaPhibeta = crossprod(theta,t(Phibeta))
      Fgls = (sum(Phibeta^2)-sum(tthetaPhibeta^2))*(length(idsignal))
      pval = NULL
   }

   if (nbf==0) {
      Lambda = NULL
      Psi = sdres^2
      Phi = rep(1,T)
      theta = matrix(0,ncol=1,nrow=T)
      Phibeta = b.ols*(rep(1,length(idsignal))%*%t(Phi))
      tthetaPhibeta = crossprod(theta,t(Phibeta))
      Fgls = (sum(Phibeta^2)-sum(tthetaPhibeta^2))*(length(idsignal))
      pval = NULL
   }

   if ((nbf>0)&(pvalue=="MC")) {
      if (verbose) print("Starting Monte-Carlo estimation of the p-value")
      lmsdres = lapply(lsdres,function(sdres,n) tcrossprod(rep(1,n),sdres),n=n) 
      lscres = Map("/",lres,lmsdres) 
      lfa = fastfa(lscres,nbf=nbf,min.err=min.err,verbose=FALSE,svd.method=svd.method)
      lPhi = as.data.frame(t(1/sqrt(lfa$Psi)))
      lmPhi = lapply(lPhi,function(x,nbf) tcrossprod(x,rep(1,nbf)),nbf=nbf)
      lbeta = Map("*",lfa$B,lmPhi)
      lsvdbeta = lapply(lbeta,fast.svd)
      ltheta = lapply(lsvdbeta,function(svdbeta,T) svdbeta$u*(rep(1,T)%*%t(svdbeta$d/sqrt(1+svdbeta$d^2))),T=T)
      lmPhi = lapply(lPhi,function(x,p) tcrossprod(rep(1,p),x),p=length(idsignal))
      lphibeta = Map("*",lb.ols,lmPhi)
      ltphibeta = lapply(lphibeta,t)
      ltthetaPhibeta = Map(crossprod,ltheta,ltphibeta)
      f0 = unlist(lapply(lphibeta,function(x) sum(x^2)))-unlist(lapply(ltthetaPhibeta,function(x) sum(x^2)))
      f0 = f0*(length(idsignal))
      pval = mean(f0>=Fgls)
   }

   if ((nbf>0)&(pvalue=="Satterthwaite")) {
      if (verbose) print("Starting Monte-Carlo estimation of the p-value")
      subsample = sample(1:nsamples,200)
      lmsdres = lapply(lsdres[subsample],function(sdres,n) tcrossprod(rep(1,n),sdres),n=n) 
      lscres = Map("/",lres[subsample],lmsdres) 
      lfa = fastfa(lscres,nbf=nbf,min.err=min.err,verbose=FALSE,svd.method=svd.method)
      lPhi = as.data.frame(t(1/sqrt(lfa$Psi)))
      lmPhi = lapply(lPhi,function(x,nbf) tcrossprod(x,rep(1,nbf)),nbf=nbf)
      lbeta = Map("*",lfa$B,lmPhi)
      lsvdbeta = lapply(lbeta,fast.svd)
      ltheta = lapply(lsvdbeta,function(svdbeta,T) svdbeta$u*(rep(1,T)%*%t(svdbeta$d/sqrt(1+svdbeta$d^2))),T=T)
      lmPhi = lapply(lPhi,function(x,p) tcrossprod(rep(1,p),x),p=length(idsignal))
      lphibeta = Map("*",lb.ols[subsample],lmPhi)
      ltphibeta = lapply(lphibeta,t)
      ltthetaPhibeta = Map(crossprod,ltheta,ltphibeta)
      f0 = unlist(lapply(lphibeta,function(x) sum(x^2)))-unlist(lapply(ltthetaPhibeta,function(x) sum(x^2)))
      f0 = f0*(length(idsignal))
      const = var(f0)/(2*mean(f0))
      nu = 2*mean(f0)^2/var(f0)
      pval = pchisq(Fgls/const,df=nu,lower.tail=FALSE)
   }

   if ((nbf==0)&(pvalue!="none")) {
      if (verbose) print("Starting Monte-Carlo estimation of the p-value")
      lPhi = lapply(1:nsamples,function(i,T) rep(1,T),T=T)
      lmPhi = lapply(lPhi,function(x,p) tcrossprod(rep(1,p),x),p=length(idsignal))
      lphibeta = Map("*",lb.ols,lmPhi)
      f0 = unlist(lapply(lphibeta,function(x) sum(x^2)))
      f0 = f0*(length(idsignal))
      pval = mean(f0>=Fgls)
   }
   return(list(Fgls=Fgls/T,Fols=Fols,pval=pval,pval.Fols=pval.Fols,nbf=nbf))
}
