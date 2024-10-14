\name{residuals.cjs}
\alias{residuals.cjs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Residuals for CJS Model }
\description{
  Residual extraction routine for a CJS object. Returns Pearson or deviance residuals 
  of a CJS capture-recapture model.
}
\usage{
\method{residuals}{cjs}(object, type="pearson", ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ a CJS (Cormack-Jolly-Seber capture-recapture) object, which is usually 
	the result of calling F.cjs.estim  }
  \item{type}{ string indicating type of residual to return.  Either "pearson" for Pearson
               residuals (i.e., (o - e)/sqrt(e*(1-e))) or "deviance" for deviance 
               residuals (i.e., 2*sign(o-e)*sqrt(o*log(o/e) + (1-o)*log((1-o)/(1-e))) ) }
 \item{\dots}{ Additional arguments to other functions. Not used, but must be here 
		for compatibility with the generic \code{residuals} function.
	    }
}

\value{
  A NAN X NS matrix of residuals, where NAN = number of animals and NS = number of
  capture occasions.  Residuals in the non-active cells are set to NA.  Non-active 
  cells are those prior to and including the initial capture, and after 
  the occasion on which an animal is known to have died.

  If type = "pearson", the residual for active cell (i,j) is 
	(o(ij) - e(ij)) / sqrt(e(ij) * (1 - e(ij))). 

  If type = "deviance", the residual for active cell (i,j) is 
	2 * sign(o(ij) - e(ij)) * sqrt(o(ij)*log(o(ij) / e(ij)) + (1 - o(ij)) * log((1 - o(ij)) / (1 - e(ij)))). 

  Observed (o(ij)) and expected (e(ij)) are defined in Details.
}

\details{
  In almost all cases, a CJS model fitted by F.cjs.estim already has a \code{$residuals} component.  This 
  routine either extracts this component, or computes residuals of the component if not found.

  Observed component (o(ij)) in formulas above is the capture indicator for animal i during occasion j.  If animal i 
  was seen during occasion j, o(ij) = 1.  Otherwise, o(ij) = 0.

  Expected component (e(ij)) in formula above is the expected value of the capture indicator for animal 
  i during occasion j.  In other words, o(ij) is a binomial random variable with expected 
  value e(ij).  Under the assumptions of a CJS model, e(ij) is computed as 
  phi(i(1)) * phi(i(2)) * ... * phi(i(j-1)) * p(ij), where 
  p(ij) is the estimated capture probability of animal i during occasion j, and phi(i(1)) is estimated survival during the first interval 
  following initial capture of the animal, phi(i(2)) is survival during the second interval 
  after initial capture, 
  and phi(i(j-1)) is survival during the interval just prior to occasion j.      
}
\author{ Trent McDonald }
\seealso{ \code{\link{F.cjs.estim}}, \code{\link{predict.cjs}} }
\examples{

# Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
residuals(dipper.cjs)

}
\keyword{ survival }% at least one, from doc/KEYWORDS
\keyword{ models }
