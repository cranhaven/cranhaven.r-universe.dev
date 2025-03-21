\name{dic}
\alias{dicStats}
\alias{dicSet}
\alias{dicClear}
\title{DIC}
\description{These functions are used to evaluate the Deviance Information Criterion.}
\usage{
dicSet()
dicStats()
dicClear()
}
\details{
These functions are used to evaluate the Deviance Information Criterion (DIC; Spiegelhalter et al., 2002) 
and related statistics - these can be used to assess model complexity and compare different models. 
Most of the examples packaged with OpenBUGS contain an example of their usage.

It is important to note that DIC assumes the posterior mean to be a good estimate of the stochastic parameters. 
If this is not so, say because of extreme skewness or even bimodality, 
then DIC may not be appropriate. 
There are also circumstances, such as with mixture models, 
in which OpenBUGS will not permit the calculation of DIC and so the menu option is greyed out. 
Please see \code{\link{help.WinBUGS}} for restrictions.
}
\value{
\code{dicStats} returns a data frame with columns:
\item{Dbar}{The posterior mean of the deviance, which is exactly the same as if the node 
    \sQuote{deviance} had been monitored. This deviance is defined as -2 * log(likelihood): 
    \sQuote{likelihood} is defined as p(y | theta), where y comprises all stochastic nodes given values (i.e. data), 
    and theta comprises the stochastic parents of y - \sQuote{stochastic parents} are the stochastic nodes 
    upon which the distribution of y depends, when collapsing over all logical relationships.}
\item{Dhat}{A point estimate of the deviance (-2 * log(likelihood)) obtained by substituting in the 
    posterior means theta.bar of theta: thus Dhat = -2 * log(p(y | theta.bar)).}
\item{pD}{The effective number of parameters is given by pD = Dbar - Dhat. 
    Thus pD is the posterior mean of the deviance minus the deviance of the posterior means.}
\item{DIC}{The Deviance Information Criterion is given by DIC = Dbar + pD = Dhat + 2 * pD. 
    The model with the smallest DIC is estimated to be the model that would best predict a replicate 
    dataset of the same structure as that currently observed.}
 }
\references{  
Spiegelhalter, D.J., Best, N.G., Carlin B.P., and van der Linde, A. (2002):
Bayesian measures of model complexity and fit (with discussion). \emph{J. Roy. Statist. Soc. B.} 64, 583-640.
}
\note{Users should ensure their simulation has converged before using these functions.
If the MCMC simulation has an adaptive phase it will not be possible to make inference 
using values sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

