\name{print.nhat}
\alias{print.nhat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ print.nhat }
\description{
  Print the estimates of N from a CJS object in a nice format.
}
\usage{ 
\method{print}{nhat}(x, width=10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class "nhat", which inherits from class "cr".  This class of object is 
              output from \code{F.cr.model.avg}. }
  \item{width}{The minimum width of columns in the table printed by this routine.} 
  \item{...}{Arguments to other functions called by this one. Currently no other 
		functions are called, so this is not used, but must be here 
		for compatibility with the generic \code{print} function.
	    }
}
\details{
Horvitz-Thompson estimates of N, along with 
standard errors, are printed.  \code{print.cjs} also prints N estimates, if present, 
but as a brief, one-row summary.  This routine prints a more complete table.  Numerical 
values of the \code{supsmu} smooth of N estimates (\code{bass} = 0.5) associated with 
plots produced by \code{plot.cjs} are printed. 
}
\value{ Nothing.  Run for side effects }
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }
\seealso{ \code{\link{plot.cjs}}, \code{\link{print.cjs}}, \code{\link{F.cjs.estim}} }

\examples{
# Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)

# Method 1: explicit matricies
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
print(dipper.cjs)

# Method 2: indicator vectors
x <- factor(1:ncol(dipper.histories), labels=paste("t",1:ncol(dipper.histories),sep=""))
nd <- nrow(dipper.histories)
dipper.cjs <- F.cjs.estim( ~tvar(x,nan=nd,drop=c(1,7)), ~tvar(x,nan=nd,drop=c(6,7)), 
    dipper.histories)
print(dipper.cjs)

}
\keyword{ models }
