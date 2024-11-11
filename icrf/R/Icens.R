# S-plus/R functions to determine the NPMLE of the distribution for interval-
# censored event time.
# Some of the unused functions are dropped (VEM, ...) by the icrf package author.
# Copyright 1998-2000 Alain Vandal and Robert Gentleman
# University of Auckland
# These functions should work, but their intent is to illustrate the
# concepts involved.  Functions are provided as is, with no guarantee.
# Redistribute freely, without undocumented modification & without charge.
# Queries to vandal@stat.auckland.ac.nz or rgentlem@stat.auckland.ac.nz.

# Returns a list of maximal antichains from a list of real valued intervals
# Arguments:  intvls:  n x 2 matrix;first column contains left endpoints
#				second column contains right endpoints
# Returned value:   list of length m (magnitude of underlying interval order)
#		      - each entry corresponds to one maximal antichain
#		      - each entry contains the row numbers of all intervals
#					  belonging to the maximal antichains
#		      - maximal antichains occur in the list in their natural
#					  linear ordering
# Known bugs: In R, will issue some ignorable warnings if there is
#right-censored data (with an "Inf" right endpoint).
Maclist <- function(intvls, Lopen=TRUE, Ropen=FALSE)
{
  m <- dim(intvls)[1]
  id <- 1:m
  or <- order(intvls[,1])
  maclist <- NULL
  curmac <- id[or[1]]
  minend <- intvls[curmac,2]
  for (i in 2:m) {
    curintvl <- id[or[i]]
    if( intvls[curintvl,1]>minend ||
        ((Lopen || Ropen) &&  intvls[curintvl,1]==minend ) ) {
      # New maximal antichain
      maclist <- c(maclist,list(curmac))
      oldmac <- curmac
      curmac <- NULL
      for (j in 1:length(oldmac))
        if ( intvls[curintvl,1]<intvls[oldmac[j],2] ||
             (!Lopen && !Ropen &&
              intvls[curintvl,1]==intvls[oldmac[j],2]) )
          curmac <- c(curmac,oldmac[j])
      curmac <- c(curmac,curintvl)
      minend <- min(intvls[curmac,2])
    } else {
      curmac <- c(curmac,curintvl)
      minend <- min(minend,intvls[curintvl,2]) }
  }
  c(maclist,list(curmac))
}

# Returns the clique matrix and Petrie pairs of an interval order
# given its list of maximal antichains Arguments: ml: list of maximal
# antichains as returned by Maclist Returned value: object containing
# # - pmat: clique matrix of the underlying interval order, # rows are
# ordered according to the linear ordering of # the maximal antichains
# # - ppairs: Petrie pairs indicate the first and last # maximal
# antichains to which each elements belongs

Macmat <- function(ml)
{
  temp <- NULL
  m <- length(ml)
  for (i in 1:m)
    temp <- c(temp,ml[[i]])
  temp <- sort(unique(temp))
  n <- length(temp)
  ppairs <- matrix(0,2,n)
  retmat <- matrix(0,m,n)
  for (i in 1:m) {
    for (j in ml[[i]]) {
      if (ppairs[1, j]==0)
        ppairs[1, j] <- i
      ppairs[2, j] <- i
    }
    retmat[i, ml[[i]]] <- 1
  }
  dimnames(ppairs) <- list(c("Start","End"),temp)
  dimnames(retmat) <- list(NULL,temp)
  ret <- list(pmat = retmat, ppairs = ppairs)
  class(ret) <- "petrie"
  return(ret)
}

# Produce the mapping of the maximal antichains to their real interval
# representation for an interval order given by real-valued intervals.
# Arguments:	intvls:	see Maclist
#		ml:	list of maximal antichains for the intervals as
#			returned by Maclist
# Returned values:  matrix m x 2 containing the mapping row-wise
#		(1rst row corresponds to 1rst maximal antichains, etc.)
#		     m is the number of maximal antichains,
#		     1rst column contains left endpoints of the mapping
#		     2nd column contains right endpoints of the mapping

MLEintvl <- function(intvls, ml=Maclist(intvls))
{
  if( ncol(intvls) != 2 || any(intvls[,2] < intvls[,1]) )
    stop("only one dimensional intervals can be handled")
  m <- length(ml)
  ret <- matrix(0, m, 2)
  for(i in 1:m) {
    LL <- min(intvls)
    RR <- max(intvls)
    for(j in 1:length(ml[[i]])) {
      LL <- max(LL, intvls[ml[[i]][j], 1])
      RR <- min(RR, intvls[ml[[i]][j], 2])
    }
    ret[i,  ] <- c(LL, RR)
  }
  ret
}

# Pool monotone groups algorithm
#  Adapted from Y.L. Zhang & M.A. Newton (1997)
# (http://www.stat.wisc.edu/~newton/newton.html)
# Isotonizes a weighted and ordered set of values
# Arguments:	est:	the list of values
#				ww:		their weights
# Returned values: object containing
#		- est:  isotonized estimates
#		- ww:	weights of the isotonized estimates
#		- poolnum:  number of values pooled in the current
#					  estimate
#		- passes:  number of passes which were required to
#					  isotonize the list

PMGA<-function(est,ww=rep(1,length(est)))
{
  curm<-length(est)
  poolnum<-rep(1,curm)
  passes<-0
  iso<-FALSE
  while (!iso) {
    iso<-TRUE
    poolind<-1
    curind<-1
    while (curind<=curm) {
      groupstart<-curind
      while (curind<curm && est[curind+1]<est[curind])
        curind<-curind+1
      iso<-poolind == curind
      est[poolind]<-sum(ww[groupstart:curind]*est[groupstart:curind])/sum(ww[groupstart:curind])
      ww[poolind]<-sum(ww[groupstart:curind])
      poolnum[poolind]<-sum(poolnum[groupstart:curind])
      poolind<-poolind+1
      curind<-curind+1
    }
    curm<-poolind-1
    passes<-passes+1
  }
  return(list(est = est[1:curm], ww = ww[1:curm], poolnum = poolnum[1:curm],
              passes = passes))
}

# Returns the (unsmoothed) NPMLE of the distribution function on the maximal
# antichains of interval censored survival data.
# The algorithm is adapted from Wellner & Zhan (1997).
# Arguments:
#	- A:  clique matrix of the data (only necessary argument)
#	- EMstep:  boolean determining whether an EM-step will be taken
#	  at each iteration
#	- ICMstep: boolean determining whether an ICM step will be taken
#	  at each iteration
#	- checkbnds: make sure that isotonization step does not wash out
#	  essential maximal antichains by using self-consistent bounds
#	- keepiter: boolean determining whether to keep the iteration
#	  states
#	- eps: maximal L1 distance between successive estimates before
#	  stopping iteration
#	- maxiter:  maximal number of iterations to perform before
#	  stopping
# Returned values:  object containing
#	- sigma:  NPMLE of the survival function on the maximal
#	  antichains
#	- weights:  diagonal of the likelihood function's second
#	  derivative
#	- lastchange:  vector of differences between the last two
#	  iterations
#	- numiter:  total number of iterations performed
#	- iter: (only present if keepiter is true) states of sigma during
#	  the iteration

EMICMmac <- function(A, EMstep=TRUE, ICMstep=TRUE, keepiter=FALSE, tol=1e-7,
                     tolbis=1e-7,maxiter=1000)
{
  if (!EMstep && !ICMstep) {
    print("One of EMstep or ICMstep must be true.")
    return(NULL)
  }
  Meps<-.Machine$double.eps
  m<-dim(A)[1]
  n<-dim(A)[2]
  tA<-t(A)
  if (m==1) {
    ret<-NULL
    ret$sigma<-1
    ret$weights<-n
    ret$lastchange<-0
    ret$numiter<-0
    return(ret)
  }

  WW<-matrix(0,m,n)
  for (i in 1:(m-1))
    WW[i,]<-A[i,]-A[i+1,]
  WW[m,]<-A[m,]
  sigma<-cumsum(apply(A,1,sum)/sum(A))
  numiter<-0
  oldsigma<-rep(-1,m)
  if (keepiter) iter<-sigma
  while (max(abs(oldsigma-sigma))>tol && numiter<=maxiter) {
    oldsigma<-sigma
    if (EMstep) {
      pvec<-diff(c(0,sigma))
      temp<-sweep(A,1,pvec,FUN="*")
      if (sum(apply(temp,2,sum)==0)==0) {
        pvec<-apply(sweep(temp,2,apply(temp,2,sum),
                          FUN="/"),1,sum)
        sigma<-cumsum(pvec)/sum(pvec)
      }
      if (keepiter) iter<-rbind(iter,sigma)
    }
    if (ICMstep) {
      Wps<-1/(t(WW)%*%sigma)
      weights<-(abs(WW)%*%Wps^2)
      increment<-as.vector((WW%*%Wps)/weights)
      sigma<-sigma+increment
      sigma[m]<-1
      if (keepiter) iter<-rbind(iter,sigma)
      temp<-PMGA(sigma[-m],weights[-m])
      poolnum<-c(0,cumsum(temp$poolnum))
      for (i in 2:length(poolnum))
        for (j in (poolnum[i-1]+1):poolnum[i])
          sigma[j]<-temp$est[i-1]
      if (keepiter) iter<-rbind(iter,sigma)
      # Implementing Jongbloed's correction through bisection
      temp<-c(0,sigma)
      pvec<-diff(c(0,oldsigma))
      ndir<-diff(c(0,temp[2:(m+1)]))-pvec
      pvec<-Bisect(tA,pvec,ndir,Meps,tolbis=1e-7)
      sigma<-cumsum(pvec)
      if (keepiter) iter<-rbind(iter,sigma)
    }
    numiter<-numiter+1
  }
  if (numiter == maxiter)
    warning("EM/ICM may have failed to converge.")
  pf<-diff(c(0,sigma))
  ret<-list(sigma=sigma,pf=pf,llk=sum(log(t(A)%*%pf)),
            weights=as.vector(weights),lastchange=sigma-oldsigma,
            numiter=numiter,eps=tol)
  if (keepiter) {
    if (EMstep && ICMstep)
      dimnames(iter)<-list(c("Seed",rep(c("EM","Fisher","PMGA","Bisect"),
                                        numiter)),NULL)
    else if (EMstep)
      dimnames(iter)<-list(rep("EM",numiter+1),NULL)
    else
      dimnames(iter)<-list(c("Seed",rep(c("Fisher","PMGA"),
                                        numiter)),NULL)
    ret$iter<-iter
  }
  ret
}


# Returns the distribution function NPMLE for interval censored data, with
# information regarding its real-line support
# Arguments:  - intvls: list of intervals as per Maclist
#			  - all other arguments:  see EMICMmac
# Returned values: object containing
#	      - all information as per returned value of EMICMmac
#	      - pf:  probability function on the maximal antichains
#	      - intmap:  real interval mapping for the mass of the
#		    NPMLE;  the Groeneboom-Wellner estimate is derived
#		    by assigning all the mass of the NPMLE on the maximal
#	            antichain to the right endpoint of the corresponding
#                   interval.
#	      - class:  value "icsurv"

EMICM <- function(A, EMstep=TRUE, ICMstep=TRUE, keepiter=FALSE, tol=1e-7,
                  maxiter=1000)
{
  if( ncol(A) == 2 && all(A[,2]>=A[,1]) ) {
    ml<-Maclist(A)
    intmap <- t(MLEintvl(A, ml))
    A <- Macmat(ml)$pmat
  }
  else
    intmap <- NULL
  temp<-EMICMmac(A, EMstep=EMstep, ICMstep=ICMstep,
                 keepiter=keepiter,tol=tol,maxiter=maxiter)
  if (is.null(temp)) return(NULL)
  class(temp) <- "icsurv"
  temp$intmap <- intmap
  temp
}

####################################
# MIXTURE METHODS for interval censored data survival estimation
# VEM, ISDM, PGM
# All require argument A, the clique matrix of the data, so that a typical call would be
# VEM(Macmat(Maclist(brcm))$pmat)
##################################
Bisect <- function(tA, pvec, ndir, Meps, tolbis=1e-7)
{
  etainv<-1/(tA%*%pvec)
  bot<-0
  top<-1
  mult<-tA%*%ndir
  dbot<-sum(etainv*mult)
  ptop<-rescaleP(pvec+top*ndir, Meps)
  pbot<-pvec
  dtop<-sum(mult/(tA%*%ptop))
  done<-FALSE
  while( !done ) {
    if( sign(dbot)*sign(dtop) > 0 || top-bot<tolbis ) {
      ltop<-sum(log(tA%*%ptop))
      lbot<-sum(log(tA%*%pbot))
      if( lbot > ltop )
        pnew<-rescaleP(pvec+bot*ndir, Meps)
      else
        pnew<-rescaleP(pvec+top*ndir, Meps)
      done<-TRUE
    }
    else {
      mid<-(bot+top)/2
      pmid<-rescaleP(pvec+mid*ndir, Meps)
      dmid<-sum(mult/(tA%*%pmid))
      if( dmid*dtop < 0 ) {
        bot<-mid
        dbot<-dmid
        pbot<-pmid
      }
      else {
        top<-mid
        dtop<-dmid
        ptop<-pmid
      }
    }
  }
  pnew
}

#############
#rescaleP is a function that rescales a prob vector so that elements
# that are negative or less than machine epsilon are set to zero.
###########
rescaleP <- function(pvec, tiny)
{
  pvec<-ifelse(pvec<tiny,0,pvec)
  pvec<-pvec/sum(pvec)
  return(pvec)
}
