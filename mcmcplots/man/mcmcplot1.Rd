\name{mcmcplot1}
\alias{mcmcplot1}

\title{ MCMC Diagnostics Plots for one Model Parameter}

\description{
Creates a graph window containing three different plots---a trace plot, a density plot, and an autocorrelation plot---for one parameter in an MCMC run.  This function is used by \code{mcmcplot} to construct an html file of MCMC diagnostics.  This function is intended for internal use only.
}

\usage{
mcmcplot1(x, col = mcmcplotsPalette(n), lty = 1, xlim = NULL, ylim =
NULL, style = c("gray", "plain"), greek = FALSE)
}

\arguments{
  \item{x}{an \code{mcmc} object with a single variable.}
  \item{col}{ colors for plotting each parallel chain.  The default is \code{seq(nchains)+1}    where \code{nchains} is the number of parallel chains in \code{mcmcout}.  If there is only one parallel chain, then the default is 1.}
  \item{lty}{ line types for plotting each parallel chain.  The default is 1 for all parallel chains.}
  \item{xlim}{ limits for the x axis of the density plot.}
  \item{ylim}{ limits for the y axis of the density plot.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{greek}{if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
}

\value{
  Creates a plot.
}

\references{ No references. }

\author{ S. McKay Curtis }

\note{
  Only the first parallel chain is used to create the autocorrelation plot.  This function is used by \code{mcmcplot} to create html output for all the parameters of an MCMC simulation.
}

\seealso{ \code{\link{mcmcplot}} }

\examples{
## Create fake MCMC output
fakemcmc <- coda::as.mcmc.list(coda::mcmc(sapply(1:5, function(dum) rnorm(1000))))
coda::varnames(fakemcmc) <- c("gamma[1,1]", "gamma[1,2]", "gamma[1,3]", "sigma[1]", "sigma[2]")

mcmcplot1(fakemcmc[, "sigma[1]", drop=FALSE])
mcmcplot1(fakemcmc[, "gamma[1,3]", drop=FALSE], style="plain")
}

\keyword{hplot}
