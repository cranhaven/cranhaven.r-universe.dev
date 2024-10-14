\name{plot.nhat}
\alias{plot.nhat}

\title{ Plot size estimates }

\description{
  Plot the population size estimates for a Cormack-Jolly-Seber 
  model estimated by F.cjs.estim
}

\usage{
\method{plot}{nhat}(x, ci = TRUE, smooth = TRUE, occasions = -1, 
	smubass = 5, ...)
}

\arguments{
  \item{x}{ An object of class 'nhat', which inherits from 'cjs'.  Objects of this 
		class are estimated open population Cormack-Jolly-Seber models produced by F.cjs.estim.}
  \item{ci}{Plot confidence intervals? If ci=TRUE, confidence intervals 
		around population size or survival estimates are plotted (depending on 'type='), otherwise, only 
		confidence intervals are not plotted.}
  \item{smooth}{Smooth estimates of population size? If type="n", smooth=TRUE will produce 
		a smoothed (supsmu) line through plotted size estimates. Ignored for type="s". }
  \item{occasions}{Vector of occasion numbers to use in plot. If any(occasions <= 0), 
		all occasions are plotted.  Otherwise, plot the occasions specified. For example, 
		if occasions = c(1,3,5), only estimates from the 1st, 3rd, and 5th capture 
		occasion are plotted.  If type = "n", occasion = 1 (1st occasion) cannot 
		be plotted because it can't be estimated.  If type = "s", occasion = ncol(y) 
		(last occasion) cannot 
		be plotted because no survival interval exist beyond the end of the study.}
  \item{smubass}{Bass parameter for super-smoothed line, if called for by smooth=TRUE. 
		Must be between 0
		and 10.  Larger numbers produce smoother lines.}
  \item{\dots}{Additional arguments to \code{plot} (for size estimates) or \code{matplot} (for 
		survival estimates). Options such as ylim=, col=, cex.axis=, etc. may be useful. }
}

\value{
    The smooth fit is invisibly returned if called for 
    by smooth = TRUE, otherwise NA is invisibly returned.
}
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{ \code{\link{F.cjs.estim}}, \code{\link{matplot}}, \code{\link{lines}}, \code{\link{plot}}, \code{\link{plot.cjs}} }
\examples{ 

data(dipper.histories)
dipper.cjs <- F.cjs.estim( ~1, ~1, dipper.histories )
plot(dipper.cjs,type="n")

# See examples for F.cr.model.avg for a way to plot model averaged population size estimates.

}
 
\keyword{ models }
