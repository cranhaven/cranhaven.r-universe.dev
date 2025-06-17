\name{Runtwalk}

\alias{Runtwalk}

\title{Run the 't-walk'}

\description{
Runs the 't-walk' and returns a list including the samples.
}

\usage{
Runtwalk(Tr, dim = length(x0), Obj, Supp, x0, xp0
  , PlotObj=FALSE, PlotLogPost=TRUE, dynty="b", pathcol="grey"
  , add=FALSE, at=6, aw=1.5, pphi=min( dim, 4)/dim
	, F1=0.4918, F2=F1+0.4918, F3=F2+0.0082, ...)
}

\arguments{

    \item{Tr}{
        number of iterations.
        }
    \item{dim}{
        dimension of the objective function.
        } 
    \item{Obj}{
        a function that takes a vector of length \code{dim} and returns -log of the objective function, up to a constant.
        }
    \item{Supp}{
        a function that takes a vector of length=\code{dim} and returns TRUE if the vector is within the support of the objective
        and FALSE otherwise.  Supp is *always* called right before Obj.
        }
    \item{x0}{First of a pair of initial points, within the support of the objective function. \code{x0} and \code{xp} must be different.}
    \item{xp0}{Second of a pair of initial points, within the support of the objective function. \code{x0} and \code{xp} must be different.}
    \item{PlotObj}{Some parameters for plotting the path of the twalk when \code{dim}=2.  Only used for demonstration purposes, commonly
         PlotObj=FALSE and the rest is ignored.  Set PlotLogPost=FALSE to also avoid plotting the LogPosterior time series
        as the talk progresses. This will force the twalk (if dim=2, add PlotObj=FALSE) to run with no graphics (eg. server or batch mode).
        }
    \item{PlotLogPost}{See description for \code{PlotObj}.}
    \item{dynty}{See description for \code{PlotObj}.}
    \item{pathcol}{See description for \code{PlotObj}.}
    \item{add}{See description for \code{PlotObj}.}
    \item{at}{The remaining parameters are the traverse and walk kernel parameters, the parameter choosing probability and the cumulative probabilities
        of choosing each kernel.  These are not intended to be modified in standard calculations.}
    \item{aw}{See description for \code{at}.}
    \item{pphi}{See description for \code{at}.}
    \item{F1}{See description for \code{at}.}
    \item{F2}{See description for \code{at}.}
    \item{F3}{See description for \code{at}.}
    \item{...}{Other parameters passed to \code{Obj}.}
}

\value{
A list with the following items:
	\item{n}{dimension of the objective.}
	\item{Tr}{number of iterations.}
	\item{Us}{value of -log of Obj for x at each iteration.}
	\item{Ups}{value of -log of Obj for xp at each iteration.}
	\item{output}{a \code{Tr}X\code{n} matrix with the iterations for x.}
	\item{outputp}{a \code{Tr}X\code{n} matrix with the iterations for xp.}
}


\references{
Christen JA and Fox C (2010). A general purpose sampling algorithm for continuous distributions (the t-walk)., Bayesian Analysis, 5 (2), 263-282. URL: http://ba.stat.cmu.edu/journal/2010/vol05/issue02/christen.pdf
}


\author{
  J Andres Christen (CIMAT, Guanajuato, MEXICO).
}

\seealso{\code{\link{OneMove}} for performing a stand alone iteration of the t-walk kernel, to be inserted within a more
complex MCMC with other transition kernels.}

\examples{  

#### We first load the twalk package:
library(Rtwalk)



#### A ver simple inline example, 4 independent normals N(0,1):
######        dimension,  num of it, -log of objective function besides a const, support,
info <- Runtwalk( dim=4,  Tr=1000,  Obj=function(x) { sum(x^2)/2 }, Supp=function(x) { TRUE },
	x0=runif(4, min=20, max=21), xp0=runif(4, min=20, max=21)) 
####  and two (intentionally bad) initial points

### One can plot some histograms:
PlotHist(info, par=3)
### Or time series of the parameters
TS(info)
### or plot the log of the objective
PlotLogObj(info)
### and remove the burn-in
PlotLogObj(info, from=500)
PlotHist(info, par=3, from=500)
TS( info, from=500)
### And do some basic autocorrelation analysis
Ana( info, from=500)

### And save the output as columns in a table
SaveOutput( info, file="Tsttwalk.dat")
### SaveOutput is simply a wraper to the write.table function



########### A more complex Objective,
########### the posterior of alpha (shape) and beta (rate) in gamma sampling
########### The prior for alpha is U( 1, 4) and for beta is Exp(1)

### a initialization function
GaSamInit <- function(sample.size=100) {
	
	### Set the dimension as the global variable npars
	npars <<- 2 ## alpha and beta 	
	
	### sample 100 gammas with the true parameters 2.5 and 3
	m <<- sample.size ### sample size, now global variable m
	smpl <- rgamma( sample.size, shape=2.5, rate=3)
	
	### calculate the suff. statistics 
	r1 <<- sum(smpl)
	r2 <<- sum(log(smpl))
}

### This is the -log of the posterior, -log of the objective
GaSamU <- function(x) {

	al <- x[1]
	be <- x[2]
	
	### It is VERY advisable to try to do the calculations inside -log post:
	-1*m*al*log(be) + m*lgamma(al) + (1-al)*r2 + be*(1+r1) 
}

### This is the support:
GaSamSupp <- function(x) {

	(((0 < x[1]) & (x[1] < 4)) & (0 < x[2]))	
}

### Is also very advisable to have a function that generates initial (random?) points
### anything "within the same galaxy of the objective" most probabbly work
### for example, sample from the prior
GaSamX0 <- function(x) { c( runif(1, min=1, max=4), rexp(1,rate=1)) }

### The twalk is run with
### Don't forget to initialize the problem:
GaSamInit()
info <- Runtwalk( dim=npars,  Tr=1000,  Obj=GaSamU, Supp=GaSamSupp, x0=GaSamX0(), xp0=GaSamX0()) 

### This no longer works!!!
### Value of dim taken from the global var n
#n <- npars
#info <- Runtwalk( Tr=1000,  Obj=GaSamU, Supp=GaSamSupp, x0=GaSamX0(), xp0=GaSamX0()) 


###  See this and many more examples in:  \url{http://www.cimat.mx/~jac/twalk/examples.R}
}

\keyword{Automatic}
\keyword{self adjusting}
\keyword{MCMC}
