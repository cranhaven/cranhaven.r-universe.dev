\name{plot.cjs}
\alias{plot.cjs}

\title{ Plot CJS Model }

\description{
  Plot the population size or survival estimates for a Cormack-Jolly-Seber 
  model estimated by F.cjs.estim
}

\usage{
\method{plot}{cjs}(x, type="n", ci = TRUE, smooth = TRUE, occasions = -1, 
	animals = -1, smubass = 5, ...)
}

\arguments{
  \item{x}{ An object of class 'cjs'.  Objects of this 
		class are estimated open population Cormack-Jolly-Seber models produced by F.cjs.estim.}
  \item{type}{ Type of estimates to plot.  type = "n" (the default) produces 
		a plot of population size estimates versus capture occasion. 
		type = "s" produces
		a plot of survival estimates versus capture occasion.  }
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
  \item{animals}{Vector of individuals to plot.  This parameter is used only when plotting
		survival estimates.  For example, animals = c(1,2,10) plots the 
		survival estimates of the 1st, 2nd, and 10th animals (rows 1, 2, and 10 of the survival 
		estimate matrix).}
  \item{smubass}{Bass parameter for super-smoothed line, if called for by smooth=TRUE. 
		Must be between 0
		and 10.  Larger numbers produce smoother lines.}
  \item{\dots}{Additional arguments to \code{plot} (for size estimates) or \code{matplot} (for 
		survival estimates). Options such as ylim=, col=, cex.axis=, etc. may be useful. }
}
\details{
Confidence intervals on survival estimates cannot be plotted with this routine. To plot confidence
intervals surrounding survival estimates, call this routine with type="s", then compute 
confidence intervals for survival estimates, and use \code{lines} to add lines to the plot.
}
\value{
  For type="s", the survival estimate matrix that was plotted is 
  is invisibly returned.  For type = "n", the smooth fit is invisibly returned if called for 
  by smooth = TRUE, otherwise NA is invisibly returned if smooth = FALSE.
}
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{ \code{\link{F.cjs.estim}}, \code{\link{matplot}}, \code{\link{lines}}, \code{\link{plot}} }
\examples{ 
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) }
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
plot(dipper.cjs)
print(dipper.cjs$s.hat)
plot(dipper.cjs, type="s", animals=1)
 }
\keyword{ models }
