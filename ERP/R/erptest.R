erptest <-
function (dta, design, design0 = NULL, 
  method = c("BH", "holm", "hochberg","hommel", "bonferroni", "BY", "fdr", "none"), 
  alpha = 0.05, pi0 = 1,nbs=NULL) {
  
  method = match.arg(method,choices=c("BH", "holm", "hochberg","hommel", "bonferroni", "BY", "fdr", "none"))
  if (is.null(design0)) 
    design0 = matrix(1, nrow = nrow(design), ncol = 1)
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
    cj = apply(design0, 2, function(x, y) all(x == y), y = design[,j])
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

  n = nrow(erpdta)
  T = ncol(erpdta)
  if (!is.null(nbs))
    if ((nbs <= 2) | (nbs > T)) 
      stop("The number nbs of B-splines should be an integer value in [3,T[")
  if (is.null(nbs)) nbs=0
  
  svd.design = fast.svd(design)
  if (ncol(design)==1) itxx = (svd.design$v%*%t(svd.design$v))/(svd.design$d[1]^2)
  if (ncol(design)>1) itxx = svd.design$v%*%diag(1/svd.design$d^2)%*%t(svd.design$v)    
  svd.design0 = fast.svd(design0)
  Proj = svd.design$u%*%t(svd.design$u)
  Proj0 = svd.design0$u%*%t(svd.design0$u)
  if (ncol(design)==1) pdesign = (svd.design$v%*%t(svd.design$u))/(svd.design$d[1])
  if (ncol(design)>1) pdesign = svd.design$v%*%diag(1/svd.design$d)%*%t(svd.design$u)    
  rdf1 = nrow(design) - length(svd.design$d)
  rdf0 = nrow(design0) - length(svd.design0$d)
  coeff = pdesign %*% erpdta
  rownames(coeff) = colnames(design)
  if (nbs==0) {
     beta = coeff[idsignal, ,drop=FALSE]
     rownames(beta) = colnames(design)[idsignal]
     res1 = erpdta - Proj %*% erpdta
     rss1 = as.vector(t(rep(1, n)) %*% res1^2)
     res0 = erpdta - Proj0 %*% erpdta
     rss0 = as.vector(t(rep(1, n)) %*% res0^2)
     fstat = ((rss0 - rss1)/(rdf0 - rdf1))/(rss1/rdf1)
     pval = pf(fstat, df1 = rdf0 - rdf1, df2 = rdf1, lower.tail = FALSE)
     sigma2 = mean((rss1/rdf1))
     if (length(idsignal)>1)
        sdsignal = sqrt(sigma2)*sqrt(diag(itxx[idsignal,idsignal]))%*%t(rep(1,T))
     if (length(idsignal)==1)
       sdsignal = sqrt(sigma2)*sqrt(itxx[idsignal,idsignal])*t(rep(1,T))
     
  }
  if (nbs>0) {
    if (ncol(design0)==1) pdesign0 = (svd.design0$v%*%t(svd.design0$u))/(svd.design0$d[1])
    if (ncol(design0)>1) pdesign0 = svd.design0$v%*%diag(1/svd.design0$d)%*%t(svd.design0$u)    
    phi = bs(1:T,df=nbs)
    beta = coeff
    svd.phi = fast.svd(phi)
    pphi = svd.phi$v%*%diag(1/svd.phi$d)%*%t(svd.phi$u)    
    b = beta%*%t(pphi) 
    fit = design%*%b%*%t(phi) 
    rdf1 = n-length(svd.design$d)
    res1 = erpdta-fit
    beta0 = (pdesign0 %*% erpdta)
    b0 = beta0%*%t(pphi) 
    fit0 = design0%*%b0%*%t(phi) 
    rdf0 = n-length(svd.design0$d)
    res0 = erpdta-fit0
    rss1 = as.vector(t(rep(1,n))%*%res1^2)
    rss0 = as.vector(t(rep(1,n))%*%res0^2)
    fstat = ((rss0 - rss1)/(rdf0 - rdf1))/(rss1/rdf1)
    pval = pf(fstat, df1 = rdf0 - rdf1, df2 = rdf1, lower.tail = FALSE)
    trSm = nbs*length(svd.design$d)
    trSm2 = nbs*length(svd.design$d)
    rdf = (T*n-2*trSm+trSm2)/T
    sigma2 = mean((rss1/rdf))
    beta = (b%*%t(phi))[idsignal,,drop=FALSE]
    itphiphi = svd.phi$v%*%diag(1/svd.phi$d^2)%*%t(svd.phi$v)
    vbeta = sigma2*kronecker(itphiphi,itxx)
    lvb = lapply(1:ncol(design),function(i,phi,vbeta,nbs,d) 
      phi%*%vbeta[seq(i,d*nbs,d),seq(i,d*nbs,d)]%*%t(phi),
      phi=phi,vbeta=vbeta,nbs=nbs,d=ncol(design))
    lsdsignal = lapply(lvb,function(vb) sqrt(diag(vb)))
    sdsignal = matrix(unlist(lsdsignal),nrow=ncol(design),byrow=TRUE)
    sdsignal = sdsignal[idsignal,,drop=FALSE]
  }
  rownames(beta) = colnames(design)[idsignal]
  rownames(sdsignal) = colnames(design)[idsignal]
  
  if (is.null(pi0)) 
    pi0 = pval.estimate.eta0(pval, diagnostic.plot = FALSE)
  correctedpval = pi0 * p.adjust(pval, method = method)
  significant = which(correctedpval <= alpha)
  test = fstat
  r2 = (1 - 1/(1 + fstat * ((rdf0 - rdf1)/rdf1)))
  if (length(idsignal)==1) test = sign(beta[1,])*sqrt(fstat)
  return(list(pval = pval, correctedpval = correctedpval, significant = significant, 
              pi0 = pi0, test = test, df1 = rdf1, df0 = rdf0, signal = beta,sd=sqrt(rss1/rdf1),
              r2 = r2,sdsignal=sdsignal,residuals=res1,coef=coeff))
}
