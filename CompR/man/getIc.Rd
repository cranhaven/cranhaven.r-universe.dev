\name{getIc}
\alias{getIc}
\title{
Gets the Information criteria's labels.
}
\description{
Gets the Information criteria's labels (AIC, BIC, CAIC).
}
\usage{
getIc(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}
}
}
\value{
vector of Information criteria.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Ic<-getIc(ResCock)
}