\name{roots.estimatedModels}
\alias{roots.estimatedModels}
\alias{roots.forecastCovEstimatorsWRTtrue}

\title{Roots Specific Methods}
\description{See the generic function description.}
\usage{
    \method{roots}{estimatedModels}(obj, digits=options()$digits, mod =FALSE, ...)
    \method{roots}{forecastCovEstimatorsWRTtrue}(obj, digits=options()$digits,
       mod=FALSE, ...)
}
\arguments{
    \item{obj}{an object from which roots are to be extracted or calculated and
       printed.}
    \item{digits}{an integer indicating the number of significant digits to be
       printed (passed to the print method).}
    \item{mod}{if TRUE the modulus of the roots is calculated. Otherwise, a
       complex value may result.}
    \item{...}{arguments to be passed to other methods.}
}
\details{
    The methods ***.ee are intended mainly to be called from EstEval 
    in the \pkg{EvalEst} as criterion for evaluating an estimation method.
}
\seealso{\code{\link{roots}}
         \code{\link{stability}}
         \code{\link[EvalEst]{EstEval}}
	}

\concept{DSE}
\keyword{ts}

