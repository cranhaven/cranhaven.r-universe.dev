\name{getPi}
\alias{getPi}
\title{
Gets the Bradley's scores.}
\description{
Gets the Bradley's scores from the function \code{EstimBradley()}.
}
\usage{
getPi(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
A list of the Bradley's scores for the different criteria .
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Pi<-getPi(ResCock)
}