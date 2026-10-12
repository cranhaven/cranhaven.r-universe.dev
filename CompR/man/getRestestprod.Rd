\name{getRestestprod}
\alias{getRestestprod}
\title{
Gets the result of the Bradley's scores multiple comparison tests.}
\description{
Gets the result of the Bradley's scores multiple comparison tests from the\cr function \code{EstimBradley()}.
}
\usage{
getRestestprod(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
\code{list} of \code{Tcla} elements of type \code{matrix} of size (number of paired comparison * 7),   
    each column corresponding to:
    
class identification,
              
criterion identification,
              
product identification i,
              
product identification j,
              
value for the statistic corresponding to H0: equality of the Bradley's scores of products i and j,
              
P value of this test,
              
Rejection or acceptation of H0 for a level of 5\%.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Restestprod<-getRestestprod(ResCock)
}