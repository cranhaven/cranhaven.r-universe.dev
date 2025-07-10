\name{denplot}
\alias{denplot}

\title{Density Plots for MCMC Parameters on a Single Plot}

\description{
  Creates a plot of densities for specified parameters from an MCMC simulation in a single plot or plots densities for those parameters as indicated by the \code{parms} and \code{regex}.
}

\usage{
denplot(mcmcout, parms = NULL, regex = NULL, random = NULL,
leaf.marker="[\\\\[_]", ci = NULL, xlim = NULL,
ylim = NULL, auto.layout = TRUE, mar=c(2.0, 2.0, 1.5, 0.25) + 0.1, col =
NULL, lty = 1, xlab = "", ylab = "", plot.title = NULL, main = NULL,
greek = FALSE, collapse = FALSE, style=c("gray", "plain"), \dots)
}

\arguments{
  \item{mcmcout}{ an object that can be coerced to an \code{mcmc} or \code{mcmc.list} object }
  \item{parms}{ a vector of character strings that identifies which variables in \code{mcmcout} should be plotted. If \code{parms} and \code{regex} are both \code{NULL}, all parameters will be plotted.}
  \item{regex}{ a vector of character strings with regular expressions that identify which variables in \code{mcmcout} should be plotted.}
  \item{random}{ an integer indicating the maximum number of parameters to randomly select for plotting from each group of parameters as specified by the \code{parms} argument.}
  \item{leaf.marker}{ a regular expression with a character class that marks the beginning of the ``leaf'' portion of a parameter name.  The default character class includes \code{[} and \code{_}}
  \item{ci}{ if non \code{NULL}, plots \code{(100*ci)}\% credible interval limits on the density plots.  The default (\code{NULL}) is not to plot the intervals.}
  \item{xlim}{ limits of the x-axis.}
  \item{ylim}{ limits of the y-axis.}
  \item{auto.layout}{ if \code{TRUE}, \code{denplot} automatically creates a plot layout using \code{mult.fig} from the \pkg{sfsmisc} package.}
  \item{mar}{ argument passed to \code{multi.fig} if \code{auto.layout=TRUE}}
  \item{col}{ colors to be used in plotting the densities.}
  \item{lty}{ line types to be used in plotting.}
  \item{xlab}{ label for the x-axis.}
  \item{ylab}{ label for the y-axis.}
  \item{plot.title}{ title to put in the outer margin.  Default is no title.}
  \item{main}{ character vector of titles for each individual plot.  If \code{NULL}, then the names of the parameters are used.}
  \item{greek}{ if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
  \item{collapse}{ if \code{TRUE}, all parallel chains are collapsed into one chain before plotting. If \code{FALSE}, parallel chains are plotted with colors as specified in \code{col}.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{\ldots}{ further arguments to be passed to the plotting function.}
}

\value{
  Creates a plot.
}

\author{
  S. McKay Curtis
}

\seealso{
  \code{\link{mcmcplot}}, \code{\link{parms2plot}}
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

## Plot densities of the fake MCMC output
denplot(fakemcmc)
denplot(fakemcmc, style="plain")
denplot(fakemcmc, collapse=TRUE, greek=TRUE, ci=0.95)
denplot(fakemcmc, xlim=range(unlist(fakemcmc)),
        plot.title="Density plots of fake data. Yawn.")
denplot(fakemcmc, "gamma")
denplot(fakemcmc, "gamma", "alpha\\\\[[12]]$") # all gamma and alpha[1] and alpha[2]

## What happens with NULL varnames?
coda::varnames(fakemcmc) <- NULL
denplot(fakemcmc)

\dontrun{
## denplot works on bugs objects too
library(R2WinBUGS)
example("openbugs", "R2WinBUGS")
## from the help file for openbugs:
schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains = 3, n.iter = 5000,
                    program = "openbugs", working.directory = NULL)
denplot(schools.sim, "theta")
}
}

\keyword{ hplot }
