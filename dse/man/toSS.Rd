\name{toSS}
\alias{toSS}
\alias{toSS.ARMA}
\alias{toSS.SS}
\alias{toSS.TSestModel}
\alias{toSSaugment}
\alias{toSSaugment.ARMA}
\alias{toSSaugment.TSestModel}
\alias{toSSnested}
\alias{toSSnested.SS}
\alias{toSSnested.ARMA}
\alias{toSSnested.TSestModel}

\title{Convert to State Space Model}
\description{
Convert a model to state space form.
}
\usage{
    toSS(model, ...)
    \method{toSS}{ARMA}(model, ...)
    \method{toSS}{SS}(model, ...)
    \method{toSS}{TSestModel}(model, ...)
    
    toSSaugment(model, ...)
    \method{toSSaugment}{ARMA}(model, fuzz=1e-14, ...)
    \method{toSSaugment}{TSestModel}(model, ...)
    
    toSSnested(model, ...)
    \method{toSSnested}{ARMA}(model, n=NULL, Aoki=FALSE, ...)
    \method{toSSnested}{SS}(model, n=NULL, Aoki=FALSE, ...)
    \method{toSSnested}{TSestModel}(model, ...)
    
    }
\arguments{
    \item{model}{An object of class TSmodel.}
    \item{n}{If n is specified then it is used as the state dimension when the 
      markov parameter conversion technique is required.}
    \item{Aoki}{logical indicating if Aoki's method (which does not work in
       general) should be tried.}
    \item{fuzz}{if the zero lag term of polynomials A and B are within fuzz of
       the identitity matrix then they are not inverted. (i.e. they are assumed
       to be identity.)}
    \item{...}{arguments to be passed to other methods.}
}
\value{
A state space model in an object of class 'SS' 'TSmodel'.
}
\details{

If the order of the AR polynomial equals or exceeds the MA 
polynomial (and the input polynomial) then the model is converted 
by state augmentation. Otherwise, it is converted by approximating 
the markov coefficients a la Mittnik.  (This may not always work
very well. Compare the results to check.)
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
model <- toSS(model)
}
\concept{DSE}
\keyword{ts}

