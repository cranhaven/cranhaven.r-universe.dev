\name{traplot1}
\alias{traplot1}

\title{
  Trace Plot for a Single Parameter in MCMC Output
}

\description{
  Creates a trace plot for one parameter in an MCMC simulation.  This function is called by the \code{mcmcplot} function, and is intended for internal use only.
}

\usage{
traplot1(x, col = NULL, lty = 1, style = c("gray", "plain"), \dots)
}

\arguments{
  \item{x}{an \code{mcmc.list} object with a single variable.}
  \item{col}{ one or more colors for the trace lines.  Default is \code{mcmcplotsPalette(nchains)}.}
  \item{lty}{ one or more line types for the trace lines.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{\dots}{ further arguments passed to the plotting function.}
}

\value{
  Creates a plot.
}

\author{
  S. McKay Curtis
}

\seealso{
  \code{\link{mcmcplot1}}, \code{\link{denoverplot1}}, \code{\link{autplot1}}
}

\examples{
## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means,each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))

traplot(fakemcmc[, "alpha[5]", drop=FALSE])
traplot(fakemcmc[, "alpha[5]", drop=FALSE], style="plain")
}

\keyword{ hplot }
