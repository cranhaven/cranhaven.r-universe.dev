\name{mixreg-internal}
\alias{aux1}
\alias{aux2}
\alias{aux3}
\alias{dirSum}
\alias{dist2d}
\alias{distMat}
\alias{cint}
\alias{gfun}
\alias{hatfun}
\alias{info1}
\alias{info2}
\alias{initRand}
\alias{mixregEngine}
\alias{parUin}

\title{Internal mixreg functions}
\description{
  Internal mixreg functions.
}
\usage{
aux1(gma,k,l)
aux2(t11,t12,t13,t21,t22,t23,t31,t32,t33)
aux3(m,ind)
dirSum(...)
dist2d(a,b,c)
distMat(x,y,theta)
cint(object,alpha=0.05,MC=FALSE,plot=FALSE,...)
gfun(xm,y,theta)
hatfun(x,g)
info1(x,y,theta,gma)
info2(x,y,theta,gma)
initRand(fmla,data,K,seed=NULL)
mixregEngine(fmla,data,ncomp,eqVar,thetaStart,itmax,
             eps,verb,digits,maxTry,seed,covMat,MC,cawl,
             warn,...)
parUin()
}

\details{
  These are usually not to be called by the user.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\keyword{internal}
