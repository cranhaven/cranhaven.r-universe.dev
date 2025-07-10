\name{rmeanplot}
\alias{rmeanplot}

\title{
  Running Mean Plots of Multiple Parameters
}
\description{
  This function produces running mean plots from an MCMC simulation on a single plot for all parameters (by default) or those parameters indicated by the \code{parms} argument.
}
\usage{
rmeanplot(mcmcout, parms = NULL, regex = NULL, random = NULL,
leaf.marker = "[\\\\[_]", ylim = NULL, auto.layout = TRUE,
mar = c(2, 2, 1.5, 0.25) + 0.1, col = NULL, lty = 1,
plot.title = NULL, main = NULL, greek = FALSE,
style = c("gray", "plain"), ...)
}

\arguments{
  \item{mcmcout}{ an object that can be coerced to an \code{mcmc} or \code{mcmc.list} object  }
  \item{parms}{ character vector specifying which subsets of parameters to plot.  If \code{NULL}, \code{mcmcplot} will plot all parameters. Regular expressions are used to strip all numbers and punctuation out of the parameter names to find the parameters that match the character strings in \code{parms}.}
  \item{regex}{ character vector of regular expressions denoting groups of parameters to plot.}
  \item{random}{ an integer indicating the maximum number of parameters to randomly select for plotting from each group of parameters as specified by the \code{parms} argument. }
  \item{leaf.marker}{ a regular expression with a character class that marks the beginning of the ``leaf'' portion of a parameter name.  The default character class includes \code{[} and \code{_}}
  \item{ylim}{ limits for the y-axis.}
  \item{auto.layout}{automatically creates a plot layout using \code{mult.fig} if \code{TRUE}.}
  \item{mar}{ argument passed to \code{multi.fig} if \code{auto.layout=TRUE}}
  \item{col}{colors to be used in plotting the densities.  Default is \code{mcmcplotsPalette(nchains)}.}
  \item{lty}{line types to be used in plotting.}
  \item{plot.title}{ title to put in the outer margin.  Default is no title.}
  \item{main}{ main title for the plots. Default is to use parameter names.}
  \item{greek}{ if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{\ldots}{ further arguments passed to the plotting function. }
}

\value{
  Creates a plot.
}

\author{
  Evangelos Evangelou
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

## Plot traces of the fake MCMC output
rmeanplot(fakemcmc)
rmeanplot(fakemcmc, style="plain")
rmeanplot(fakemcmc, "gamma", greek=TRUE)
}

\keyword{ hplot }
