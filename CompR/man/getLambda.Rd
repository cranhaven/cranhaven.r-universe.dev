\name{getLambda}
\alias{getLambda}
\title{
Gets the weight of the different classes.}
\description{
Gets the weight of the different classes from the function \code{EstimBradley()}.
}
\usage{
getLambda(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
A vector of the weights of the different classes.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Lambda<-getLambda(ResCock)
}