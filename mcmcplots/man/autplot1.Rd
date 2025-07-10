\name{autplot1}
\alias{autplot1}
\title{
  Autocorrelation Plot of MCMC Output
}
\description{
  Creates an autocorrelation or partial autocorrelation plot of MCMC output.
}
\usage{
autplot1(x, chain = 1, lag.max = NULL, partial = FALSE,
col = mcmcplotsPalette(1), style = c("gray", "plain"),
ylim = NULL, \dots)
}
\arguments{
  \item{x}{an \code{mcmc.list} object with a single variable.}
  \item{chain}{the number of the parallel chain for plotting.  The default is to use the first parallel chain.}
  \item{lag.max}{passed as an argument to the autocorrelation function \code{acf}.}
  \item{partial}{logical indicating whether paritial autocorrelation should be plotted.}
  \item{col}{color of the bars in the plot.}
  \item{style}{if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{ylim}{limits for the y-axis.}
  \item{\dots}{ further arguments passed to the plotting function.}
}
\details{
  None.
}
\value{
  Creates a plot.
}
\references{
  None.
}
\author{
  S. McKay Curtis (adapted from Martyn Plummer's \code{autcorr.plot} code in the \pkg{coda} package)
}

\seealso{
  \code{\link[stats]{acf}}, \code{\link[coda]{autocorr.plot}}
}

\examples{
## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <-
    coda::as.mcmc.list(
        lapply(1:3,
               function(i)
                   coda::mcmc(matrix(rnorm(nc*nr, rep(means,each=nr)),
                                     nrow=nr, dimnames=list(NULL,pnames)))))

autplot1(fakemcmc[, "alpha[1]", drop=FALSE])
autplot1(fakemcmc[, "alpha[1]", drop=FALSE], chain=2, style="plain")
autplot1(fakemcmc[, "alpha[1]", drop=FALSE], partial=TRUE)

}

\keyword{ hplot }
