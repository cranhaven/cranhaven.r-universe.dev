\name{print.hug}
\alias{print.hug}

\title{ Print Huggin's Model objects }

\description{
  Print method for Huggin's closed population model objects 
  estimated by F.huggins.estim().
}
\usage{
\method{print}{hug}(x, ...)
}
\arguments{
  \item{x}{ An object of class "hug" produced by F.huggins.estim()}
  \item{\dots}{ Arguments to other functions called by this one. Currently no other 
		functions are called, so this is not used, but must be here 
		for compatibility with the generic \code{print} function.
	    }
}
\details{
If there are no free covariates in the recapture model (i.e., \code{recapture=NULL}), 
only the combined capture and recapture model is printed. If the recapture 
model has coefficients, coefficients in both are printed in separate columns. 

When a recapture model has free coefficients, 
a "C" (for "capture") appears next to coefficients
in the recapture model that also appear in the capture model.  These 
coefficients are fixed in the recapture model because they 
are not free.  These values were estimated from initial capture information.  
A "B" (for "behavioral")
appears next to free coefficients in the recapture model that \emph{do not}
appear in the initial capture model.  

}

\value{
  Nothing is returned.  This function is used exclusively for its side effects.  It prints
  an object of class "hug" in a nice human-readable format.  
}
\author{ Trent McDonald, Ph.D., WEST-INC, tmcdonald@west-inc.com  }
\seealso{ \code{\link{F.huggins.estim}} }
\examples{ 

data(dipper.histories)
dipper.fit <- F.huggins.estim( ~1, histories= dipper.histories )
print(dipper.fit)

 }
\keyword{ models }

