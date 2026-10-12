\name{getZh}
\alias{getZh}
\title{
Gets the result of the function \code{EstimBradley()} 
}
\description{
Gets the posterior probabilities for each individual to belong to the different classes and the class with the higher probability.
}
\usage{
getZh(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}

\value{
Object of class \code{matrix} with the posterior probabilities for each individual to belong to the different classes and the class with the higher probability.
}
\examples{
data(Cocktail)
ResCock2<-EstimBradley(Cocktail,Constraint=0,Tcla=2,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock2_Zh<-getZh(ResCock2)
}