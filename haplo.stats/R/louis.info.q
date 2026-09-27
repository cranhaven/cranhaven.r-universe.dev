#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/louis.info.q,v 1.9 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: louis.info.q,v $
#Revision 1.9  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.8  2007/02/21 21:19:40  schaid
#fixed call of Ginv by passing a smaller value of eps (see Machine tol for eps) .
#
#Revision 1.7  2004/03/19 15:02:14  sinnwell
#have PACKAGE in all .C calls, part of '...' for Splus
#
#Revision 1.6  2004/03/17 20:54:32  sinnwell
#two different calls for .C("louis_info" for R and Splus
#
#Revision 1.5  2004/03/02 16:53:48  sinnwell
#fix typo
#
#Revision 1.4  2004/03/02 16:45:54  sinnwell
#change call to resid.scaled.glm.fit
#
#Revision 1.3  2003/12/10 16:44:02  sinnwell
#fix error caused by previous change of T to 1, F to 0
#
#Revision 1.2  2003/12/08 19:59:50  sinnwell
#changed T,F to TRUE,FALSE
#
#Revision 1.1  2003/09/16 16:02:21  schaid
#Initial revision
#
louis.info <- function(fit, epsilon=1e-8){

 # Compute Louis' Information Matrix for glm regression coefficients (which
 # include effects for haplotypes and non-genetic covariates) and
 # haplotype frequency estimates. Because information is not defined when
 # haplotype frequencies approach zero, the rows and columns of the 
 # information matrix are eliminated for any haplotype frequencies
 # less than the threshold haplo.min.info

 haplo.min.info <-  fit$control$haplo.min.info

 # base haplotype
  hap.base <- fit$haplo.base
  hap.freq <- fit$haplo.freq

  # subject-specific haplotypes
  h1 <- fit$haplo.post.info$hap1
  h2 <- fit$haplo.post.info$hap2

  # determine which haplotypes to remove from info matrix,
  # due to small frequencies, and eliminate observations
  # that are composed of at least one rare haplotype, and
  # fix up base haplotype index, in case its index is >
  # index of any eliminated haplotpes

  hap.id <- 1:length(hap.freq)

  hap.elim <- hap.id[hap.freq < haplo.min.info]

  if(length(hap.elim)>0){
    # check if haplo.base is among those eliminated
    if(any(hap.elim == hap.base)) 
     stop("Error - base haplotype frequency is too small - choose a different base")
     hap.keep <- hap.id[-hap.elim]
     hap.base <- (1:length(hap.keep))[hap.keep==hap.base]
     hap.freq <- hap.freq[hap.keep]
     h1 <- as.numeric(factor(h1,levels=hap.keep))
     h2 <- as.numeric(factor(h2,levels=hap.keep))

     keep <- ifelse(is.na(h1) | is.na(h2), FALSE, TRUE)

     h1 <- h1[keep]
     h2 <- h2[keep]
  
  } else {
    keep <- rep(TRUE,length(h1))
  }


 # standardized residuals =  {y - fit}/a(phi)
  resid.lst <- residScaledGlmFit(fit)   
  resid <- resid.lst$resid[keep]
  a.phi <- resid.lst$a.phi

  # variance function =  b''(theta)
  vfunc <- varfunc.glm.fit(fit) /a.phi
  vfunc <- vfunc[keep]
 
  ## From the glm fit, prior.weights are what go into glm, expanded to the size
  ## of the haplo.mf model matrix;  weights are what 
  ## come out after IRWLS (scaled and multiplied by post).
  ## We want the expanded prior weights times the last updated posterior
  
  wt <- fit$prior.weights * fit$haplo.post.info$post

  wt <- wt[keep]

  xvec <- as.vector(fit$x[keep,])

  ncov <- ncol(fit$x)

  indx.subj <- fit$haplo.post.info$indx[keep]

  nhap <- length(hap.freq)

  len.tot <- length(indx.subj)

  info11 <- numeric(ncov^2)
  info12 <- numeric(ncov*(nhap-1))
  info22 <- numeric((nhap-1)^2)

  size.max <- max(c(ncov, (nhap-1)))
 
  # zero-offset hap.base, h1, h2 for C

  hap.base <- hap.base - 1
  h1 <-  h1 - 1
  h2 <-  h2 - 1

 tmp <- .C("louis_info",
           len.tot=as.integer(len.tot),
           indx.subj=as.integer(indx.subj),
           resid=as.double(resid),
           vfunc=as.double(vfunc),
           wt=as.double(wt),
           xvec=as.double(xvec),
           ncov=as.integer(ncov),
           h1=as.integer(h1),
           h2=as.integer(h2),
           hap.base=as.integer(hap.base),
           nhap=as.integer(nhap),
           hap.freq=as.double(hap.freq),
           info11=as.double(info11),
           info12=as.double(info12),
           info22=as.double(info22),
           PACKAGE='haplo.stats')

 nh <- nhap-1
 info11 <- matrix(tmp$info11, ncol=ncov)
 info12 <- matrix(tmp$info12, ncol=nh)
 info22 <- matrix(tmp$info22, ncol=nh)

 info <- rbind( cbind(info11,   info12),
               cbind(t(info12), info22) )

# v <-  Ginv(info, eps=sqrt(.Machine$double.eps))
 v <- Ginv(info, epsilon)
 var.mat <-v$Ginv
 rank   <- v$rank

 if(length(hap.elim)==0) hap.elim <- NA

 return(list(info=info, var.mat=var.mat, rank = rank,
             haplo.base = hap.base +1, haplo.elim=hap.elim))

}


