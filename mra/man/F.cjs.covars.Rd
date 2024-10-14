\name{F.cjs.covars}
\alias{F.cjs.covars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ F.cjs.covars }
\description{
  Return an x and y 3-D array for estimation of 
  a traditional time-variant Cormack-Jolly-Seber capture-recapture model.
}

\usage{
F.cjs.covars(nan, ns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nan}{Number of individuals/animals. }
  \item{ns}{Number of trap/mark occasions. }
}

\value{
A list containing a single component, \code{\$x}, that can be used to 
estimate a classical CJS model when included in a subsequent call to 
\code{F.cjs.estim}. The returned component, \code{\$x},
is a 3-D array containing 0's everywhere, except for 1's in certain columns.
\code{\$x} has dimension \code{nan} X \code{ns} X \code{ns}.
Element [i,j,k] of \code{\$x} is 1 if j == k, and 0 otherwise.  I.e., the k-th "page" of 
the 3-D array has 1's in the k-th column, 0's elsewhere.
}

\details{Pages from \code{\$x} are designed to useful 
for fitting classical CJS models with time-variant, but individual-invariant effects. 
To fit a CJS model using this function, the commands would be 
something like: 

\code{tmp<-F.cjs.covars(nan,ns);F.cjs.estim(capture=~tmp\$x[,,2]+tmp\$x[,,3]+ ..., survival=
~tmp\$x[,,1]+tmp\$x[,,2]+ ..., histories=my.histories)}

}
\author{ Trent McDonald, WEST Inc., tmcdonald@west-inc.com }
\seealso{ \code{\link{F.cjs.estim}} }
\examples{
##  Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
dipper.cjs <- F.cjs.estim( capture=~xy$x[,,2]+xy$x[,,3]+xy$x[,,4]+xy$x[,,5]+xy$x[,,6], 
	survival=~xy$x[,,1]+xy$x[,,2]+xy$x[,,3]+xy$x[,,4]+xy$x[,,5], dipper.histories )
print(dipper.cjs)

}
\keyword{ models }
