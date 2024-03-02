\name{toSSOform}
\alias{toSSOform}
\alias{toSSOform.TSmodel}
\alias{toSSOform.TSestModel}

\title{Convert to Oform}
\description{Convert a state space model to (observability?) form.}
\usage{
    toSSOform(model)
    \method{toSSOform}{TSmodel}(model)
    \method{toSSOform}{TSestModel}(model)
    }
\arguments{
    \item{model}{An object of class TSmodel.}
}
\value{
  An object of class 'SS' 'TSmodel' containing a state space 
  model in observability form (more or less).
  }
\details{
  WARNING: This function does not work properly.
 
  Convert to a SS innovations representation with a minimum number 
  of parameters by converting as much of H as possible to I matrix.
  Any remaining reductions are done by converting part of ?? to I.
  It seems there should remain n(m+2p) free parameters in F,G,H,K, and Om is 
  determined implicitly by the residual.
  }
\seealso{
    \code{\link{toSSinnov}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
%    model <- toSSOform(model)
}
\concept{DSE}
\keyword{ts}

