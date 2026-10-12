\name{getLvr}
\alias{getLvr}
\title{
Gets the final value of loglikelihood.}
\description{
Gets the final value of loglikelihood from the function \code{EstimBradley()}.
}
\usage{
getLvr(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
Numeric value of the loglikelihood.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Lvr<-getLvr(ResCock)
}