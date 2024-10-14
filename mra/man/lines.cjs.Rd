\name{lines.cjs}
\alias{lines.cjs}

\title{ lines.cjs }
\description{
  Add a line to an existing CJS capture-recapture plot showing either N or survival estimates.
}

\usage{ 
\method{lines}{cjs}( x, what="n", animals=-1, occasions=-1, \dots )
}

\arguments{
  \item{x}{ CJS object from \code{F.cr.estim} }
  \item{what}{ Indicator for what to plot.  what = "n" plots estimates of size (i.e,. \$n.hat). 
		what = "s" plots estimates of survival. }
  \item{animals}{ Index of animals to plot.  This is the row number for 
		animals to include.  E.g., if \code{animals = c(1,4,10)}, the 1st, 4th, and 10th animals
		represented in the 1st, 4th, and 10th rows of the capture history matrix are plotted. 
		Applies to survival estimates only. }
  \item{occasions}{ Sampling occasions to plot.  This must match the occasions argument to 
		the last \code{plot.cjs}.  If the first element of \code{occasions} is <= 0, 
		all occasions are plotted.  Otherwise, only occasions listed in \code{occasions}
		are plotted.}
  \item{\dots}{Additional arguments to \code{lines} (for N estimates) or \code{matlines} (for survival 
		estimates).  Arguments like \code{col=} and \code{lty=} may prove useful.}
}
\details{
	This is a utility function for plotting.  Lines are added to the current plot.  
	A current plot must be displayed.  
}
\value{
	Nothing.  A value of 1 is invisibly returned. 
}
\author{ Trent McDonald, WEST Inc., tmcdonald@west-inc.com }
\seealso{ \code{\link{plot.cjs}}, \code{\link{lines}}, \code{\link{matlines}} }
\examples{

data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 

# Fit constant capture probability, period (i.e., flood) effects on survival
dipper.cjs <- F.cjs.estim( ~1, ~x2+x3, dipper.histories )  

plot(dipper.cjs, type="s", animals=1)
lines(dipper.cjs, what="s", animals=c(4, 10))

}
\keyword{ models }
