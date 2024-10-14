\name{F.update.df}
\alias{F.update.df}

\title{ F.update.df - Update degrees of freedom in a Cormack-Jolly-Seber fitted object }
\description{
Updates the degrees of freedom in a fitted object to either the value estimated from the rank of the variance-covariance matrix, the
number of coefficients, or a user-specified value.
}
\usage{
F.update.df(fit, df=NA)
}

\arguments{
  \item{fit}{ An MRA fitted CJS model.  Class must be c("cjs", "cr"). These are produced by \code{F.cjs.estim}. }
  \item{df}{ The new value for degrees of freedom.
	If \code{df} == NA, the number of parameters estimated from the rank of the
	matrix of 2nd derivatives or Hessian, depending on \code{cov.meth} parameter.
	If \code{df} <= 0, the number of parameters will be
	set to NX+NY = the number of estimated coefficients. Otherwise, if \code{df} > 0,
	the supplied value is used.  The penalty terms of AIC, QAIC, AICc, and QAICc are recomputed using this value.  }
}


\value{An object (list) of class c("cjs","cr") with degrees of freedom, AIC, QAIC, AICc, and QAICc updated. }



\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{ \code{\link{F.cjs.estim}} }

\examples{


## Fit CJS model to dipper data, time-varying capture and survivals.
data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipper.cjs <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

## Update the degrees of freedom
dipper.cjs <- F.update.df( dipper.cjs, -1 )

}

\keyword{ survival }% at least one, from doc/KEYWORDS

\keyword{ models }% __ONLY ONE__ keyword per line
