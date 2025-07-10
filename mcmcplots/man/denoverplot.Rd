\name{denoverplot}
\alias{denoverplot}
\title{Overlaying Densities for Parameters from two MCMC Simulations.}

\description{
  Determines which parameters are in common from two different MCMC simulations and plots overlaying density estimates of the parameters in common.
}

\usage{
denoverplot(mcmc1, mcmc2, parms = NULL, regex = NULL, random = NULL,
ci = NULL, auto.layout = TRUE, legend = TRUE,
mar = c(2.0, 2.0, 1.5, 0.25) + 0.1, col = mcmcplotsPalette(2),
lty = 1, plot.title = NULL, main = NULL, greek =FALSE,
style = c("gray", "plain"), \dots)
}

\arguments{
  \item{mcmc1}{ object that can be coerced to an \code{mcmc} object. }
  \item{mcmc2}{ object that can be coerced to an \code{mcmc} object. }
  \item{parms}{ character vector specifying which subsets of parameters to plot.  If \code{NULL} and \code{regex=NULL}, \code{denoverplot} will plot all common parameters \code{mcmc1} and \code{mcmc2}. See documentation for \code{\link{parms2plot}} for more information. }
  \item{regex}{ character vector of regular expressions denoting groups of parameters to plot. }
  \item{random}{ integer specifying how many parameters from each group will be randomly selected for plotting.  This argument is useful when \code{mcmcout} has a large number of parameters (e.g., from a hierarchical model).  If \code{NULL}, \code{mcmcplot} will plot all parameters.}
  \item{ci}{ if non \code{NULL}, plots \code{(100*ci)}\% credible interval limits on the density plots.  The default (\code{ci=NULL}) is not to plot the intervals.}
  \item{auto.layout}{ logical specifying whether the \code{mult.fig} function from the \pkg{sfsmisc} package should be used to construct the plotting region.}
  \item{legend}{ if \code{TRUE} an extra plot in the plotting region is used as a legend.}
  \item{mar}{ argument passed to \code{multi.fig} if \code{auto.layout=TRUE}}
  \item{col}{ colors for plotting the densites. }
  \item{lty}{ line types for plotting densities.  Argument is recylced to be of length 2.}
  \item{plot.title}{ title to put in the outer margin.  Default is no title.}
  \item{main}{ character vector of titles to put over each individual plot.  If \code{NULL}, then the names of the parameters are used.}
  \item{greek}{if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{\ldots}{ additional arguments passed to the \code{denoverplot} function.}
}

\details{
  This function can be used in debugging MCMC code by comparing distributions of parameters from the development MCMC code and a reference MCMC simulation.
}

\value{
  Creates a plot.
}

\author{S. McKay Curtis}

\seealso{
  \code{\link{denplot}}, \code{\link{parms2plot}}
}

\examples{
## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means, each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))
fakemcmc2 <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means, each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))

## Plot the fake MCMC output
denoverplot(fakemcmc, fakemcmc2)
denoverplot(fakemcmc, fakemcmc2, style="plain",
            col=mcmcplotsPalette(3, type="grayscale"),
            ci=0.95, greek=TRUE)
denoverplot(fakemcmc, fakemcmc2,
            plot.title="Comparison of densities of fake data")
denoverplot(fakemcmc, fakemcmc2,
            plot.title="Comparison of densities of fake data", greek=TRUE)

}

\keyword{hplot}
