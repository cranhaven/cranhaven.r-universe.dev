\name{getVarcov}
\alias{getVarcov}
\title{
Gets the Bradley'scores covariance matrices.}
\description{
Gets the Bradley'scores covariance matrices from the function \code{EstimBradley()}.}
\usage{
getVarcov(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
\code{list} of \code{Tcla} elements containing Bradley'scores covariance matrices for the different criteria.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Varcov<-getVarcov(ResCock)
}