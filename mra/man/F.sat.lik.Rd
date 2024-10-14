\name{F.sat.lik}
\alias{F.sat.lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ F.sat.lik }
\description{
  Calculate the log likelihood of a fully saturated time varying CJS
  model. Use to convert the relative deviance output by \code{F.cjs.estim} to
  actual deviance.
}
\usage{
F.sat.lik(ch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch}{ A capture history matrix consisting of 0's, 1's, and 2's. }
}
\value{
A scalar equal to the value of the saturated CJS log-likelihood.  The 
saturated log-likelihood is the theoretical best predictive model possible, 
and actual deviance is calculated relative to this.  See Examples.
}
\details{
The number reported as \code{deviance} by \code{F.cjs.estim} is relative 
deviance, calculated as -2*log(likelihood).  IF THERE ARE NO INDIVIDUAL-VARYING 
COVARIATES in the model, it is possible to compute the theoretical log-likelihood
for a set of data assuming perfect prediction.  This is the saturated log-likelihood. 
The actual deviance of a model is the deviance of the model relative to this 
theoretical maximum, computed as -2*((saturated log-likelihood) - 
2*(model log-likelihood)).  

In the parameterization of \code{F.cjs.estim}, all covariates are potentially individual and 
time varying, and in this case the saturated log-likelihood is unknown. Consequently, 
the saturated likelihood is not often needed in MRA.  This routine was included 
as a utility function because the saturated likelihood is handy in some cases, including
parametric bootstrapping to estimate C-hat. 

Assuming \code{cjs.fit} is an estimated CJS model with time varying
covariates only fit to histories in \code{cjs.hists}, compute deviance as 

\code{-F.sat.lik(cjs.hists) - 2*cjs.fit\$loglik} = 
\code{cjs.fit\$deviance - F.sat.lik(cjs.hists)}



}


\note{ 
CAUTION: This routine works for time varying models only. If 
individual-varying or individual-and-time-varying covariates are fitted 
in the model, 
the routine cannot sense it and will run but yield an incorrect answer. 
Use relative deviance reported by \code{F.cjs.estim} in this case.

Also, this routine will not run if animals have been removed (censored). I.e., the 
capture history matrix cannot have any 2's in it. Use relative deviance reported 
by \code{F.cjs.estim} when animals have been removed. 
}

\references{ Look up "saturated model" in the program MARK help file
	     for the equations implemented by this function. 
 }

\author{ Eric V. Regehr (USGS, eregehr@usgs.gov) and Trent McDonald (WEST Inc., tmcdonald@west-inc.com) }

\seealso{ \code{\link{F.cjs.estim}} }
\examples{ 
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
deviance <- -F.sat.lik( dipper.histories ) - 2*dipper.cjs$loglik
 }
\keyword{ models }
