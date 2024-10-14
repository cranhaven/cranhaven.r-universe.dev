\name{predict.cjs}
\alias{predict.cjs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ predict.cjs }
\description{ 
  Predictor method for CJS capture-recapture objects.  Return expected 
  values for all active cells in the model.
}
\usage{
\method{predict}{cjs}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{CJS capture-recapture model as output from F.cjs.estim }
  \item{\dots}{ Additional arguments to other functions.  Not used, but must be here 
		for compatibility with the generic \code{predict} function.
	    }
}
\value{A nan X ns matrix of fitted values (=cell expected value), where 
	nan=number of animals and ns=number of samples.  Fitted values in the 
	non-active cells are set to NA.  Non-active 
	cells are those prior to and including the initial capture, and after 
	the occasion on which an animal is known to have died.  Computation of 
	expected values is described in the \code{details} section of the help file 
	for \code{F.cjs.gof}.
	
}
\details{
	The only components of \code{cjsobj} needed are \code{$histories}, \code{$p.hat}, \code{$s.hat}
}
\author{ Trent McDonald, WEST Inc., tmcdonald@west-inc.com }
\seealso{ \code{\link{F.cjs.estim}}, \code{\link{F.cjs.gof}} }
\examples{

# Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
dipper.expected <- predict(dipper.cjs)

}
\keyword{ models }
