\name{getLvriter}
\alias{getLvriter}
\title{
Gets the iteration done until convergence of the loglikelihood estimation of Bradley's scores.}
\description{
Gets the iteration done until convergence from the function \code{EstimBradley()}
}
\usage{
getLvriter(object)
}
\arguments{
  \item{object}{
      An object of class \code{BradleyEstim}
}
}

\value{
A matrix with numbers of iteration rows and 4 columns giving the iteration, the previous value of loglikelihood, the current value of the loglikelihood, and the difference between these loglikelihoods.
}
\examples{
data(Cocktail)
ResCock<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
ResCock_Lvriter<-getLvriter(ResCock)
}