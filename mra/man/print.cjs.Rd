\name{print.cjs}
\alias{print.cjs}

\title{ Print Cormack-Jolly-Seber (CJS) Models }

\description{
  Print method for Cormack-Jolly-Seber (CJS) models estimated by F.cjs.estim().
}
\usage{
\method{print}{cjs}(x, alpha=c(0.05,0.01), ...)
}
\arguments{
  \item{x}{ An object of class "cjs" produced by F.cjs.estim()}
  \item{alpha}{ A vector with length either 2 or 3 containing alpha levels 
		used to put "*" or "**" beside the GOF results. 
		One * is printed if significance is between alpha[1] and alpha[2] 
		(i.e., if alpha[2] < p < alpha[1]).  Two ** are printed if significance 
		is less than alpha[2] (p < alpha[2]).
	      }
  \item{\dots}{ Arguments to other functions called by this one. Currently no other 
		functions are called, so this is not used, but must be here 
		for compatibility with the generic \code{print} function.
	    }
}
\value{
  Nothing is returned.  This function is used exclusively for its side effects.  It prints
  an object of class "cjs" in a nice human-readable format.  If goodness-of-fit tests are present, 
  they are printed.  If population size estimates are present, they are printed. 
}
\author{ Trent McDonald, Ph.D., WEST-INC, tmcdonald@west-inc.com  }
\seealso{ \code{\link{F.cjs.estim}}, \code{\link{plot.cjs}} }
\examples{ 
# Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
print(dipper.cjs)
 }
\keyword{ models }

