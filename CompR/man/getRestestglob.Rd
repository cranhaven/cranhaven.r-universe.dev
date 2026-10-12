\name{getRestestglob}
\alias{getRestestglob}
\title{
Gets the result of the test of Bradley's scores equality.}
\description{
Gets the result of the test of Bradley's scores equality from the function \code{EstimBradley()}.
}
\usage{
getRestestglob(object)
}
\arguments{
  \item{object}{
An object of class \code{BradleyEstim}}
}
\value{
\code{list} of five elements:
        
\code{lvrH0} { \code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood under the hypothesis of equality of Bradley's scores}
              
\code{lvrH1} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood under the hypothesis of non equality of Bradley's scores}
              
\code{lRatio} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood Ratio statistic}
              
\code{Pvalue} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the P value of the log likelihood Ratio test}
              
\code{H1} {\code{matrix} of size (\code{Tcla} * number of criteria) giving the result of rejection of equality of Bradley's scores}

}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Restestglob<-getRestestglob(ResCock)
}