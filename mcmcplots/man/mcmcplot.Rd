\name{mcmcplot}
\alias{mcmcplot}

\title{ Diagnostics Plots for MCMC in HTML format }

\description{
  Creates an HTML file that displays diagnostic plots (trace, density, autocorrelation) from an MCMC simulation.  When the number of parameters in an MCMC simulation is large, viewing all plots in a web browser is much easier than clicking through \R graph windows.
}

\usage{
mcmcplot(mcmcout, parms = NULL, regex = NULL, random = NULL,
leaf.marker = "[\\\\[_]", dir = tempdir(), filename = "MCMCoutput",
extension = "html", title = NULL,
heading = title, col = NULL, lty = 1,
xlim = NULL, ylim = NULL,
style=c("gray", "plain"), greek = FALSE)
}

\arguments{
  \item{mcmcout}{ posterior draws. This argument will be coerced to an \code{mcmc} object.}
  \item{parms}{ character vector specifying subsets of parameters to plot.  If \code{parms=NULL} and \code{regex=NULL}, \code{mcmcplot} will plot all parameters.}
  \item{regex}{ character vector of regular expressions denoting groups of parameters to plot.}
  \item{random}{ integer specifying how many parameters from each group will be randomly selected for plotting.  This argument is useful when \code{mcmcout} has a large number of parameters (e.g., from a hierarchical model).  If \code{NULL}, \code{mcmcplot} will plot all parameters.}
  \item{leaf.marker}{ a regular expression with a character class that marks the beginning of the ``leaf'' portion of a parameter name.  The default character class includes \code{[} and \code{_}}
  \item{dir}{ string containing the directory where the plots and the main html file will be stored. }
  \item{filename}{ string containing the name of the main html file which will contain code to display each plot produced by \code{mcmcplot}.}
  \item{extension}{ string containing the extension to be used for the html file.}
  \item{title}{ string containing the title to be included in the html file.  Default is to use the name of object passed as the \code{mcmcout} argument prepended with the string "MCMC Output: ".}
  \item{heading}{ string containing the heading to be used for the html file.  Default is to use the \code{title}.}
  \item{col}{ vector of colors.  This will determine the colors that will be used to plot each chain in the traceplots and density plots.  Default is \code{mcmcplotsPalette(nchains)}.}
  \item{lty}{ vector of line types.  This will determine the line types that will be used to plot each chain in the traceplots and density plots.  If missing, \code{mcmcplot} will use 1 for all line types.}
  \item{xlim}{ limits for the x axis of the density plot.}
  \item{ylim}{ limits for the y axis of the density plot.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{greek}{if \code{TRUE}, the names of greek letters in the \code{labels} will be displayed as greek characters on the plot.}
}

\details{
  The \code{mcmcplot} function generates an html file that contains diagnostics plots -- trace plots, autocorrelation plots, and density plots -- for parameters from an MCMC simulation.  When an MCMC simulation contains a large number of parameters, it is no longer convenient to view plots in a small graph window.  Viewing the plots in a web browser gives the user the ability to scroll through and examine a large number of plots in a more convenient manner.

See documentation for \code{\link{parms2plot}} for more information on how to ``smartly'' select parameters to plot using the \code{parms}, \code{regex}, and \code{random} arguments.

}

\value{
  Invisibly returns a string containing the path to \code{filename}.
}

\author{
  S. McKay Curtis and Ilya Goldin
}

\seealso{
  \code{\link{parms2plot}}, plotting functions in the \pkg{coda} package.
}

\examples{
\dontrun{
## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means,each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))

## Use mcmcplot to plot
## the fake MCMC output
mcmcplot(fakemcmc)
mcmcplot(fakemcmc, greek=TRUE)
mcmcplot(fakemcmc, xlim=range(fakemcmc)) # put the densities on the same scale
mcmcplot(fakemcmc, "gamma")
mcmcplot(fakemcmc, regex="alpha\\\\[[12]", style="plain")
mcmcplot(fakemcmc, "gamma", regex="alpha\\\\[[12]")
mcmcplot(fakemcmc, random=2)
mcmcplot(fakemcmc, random=c(2, 3))

## What happens with NULL varnames?
coda::varnames(fakemcmc) <- NULL
mcmcplot(fakemcmc)

## mcmcplot works on bugs objects too
library(R2WinBUGS)
example("openbugs", "R2WinBUGS")
## from the help file for openbugs:
schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains = 3, n.iter = 5000,
                    program = "openbugs", working.directory = NULL)
mcmcplot(schools.sim)
}
}

\keyword{ hplot }
