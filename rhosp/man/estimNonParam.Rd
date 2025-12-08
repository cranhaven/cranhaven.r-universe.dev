\name{estimNonParam}
\alias{estimNonParam}
\title{ compute the parametric estimation}
\description{
   estimParam compute the parametric estimation of the first model on the input file
   containing the stay duration Xi and the side effect reporting Zi. this estimator of R (and not mu)
   is the smallest root of the equation \eqn{(*) : P(Zi=0)E[exp(-RXi)/Zi=0]=1}
}
\usage{
estimNonParam(fileName, toplot = TRUE, header = TRUE, ks = FALSE, DV = FALSE)
}
\arguments{
  \item{fileName}{ the file in which the simulated data will be stored }
  \item{toplot}{ a logical variable to plot the result of this estimation }

  \item{header}{ a logical for : has the input file an header }
  \item{ks}{ a logical for : do you want the Kolmogorv Smirnov test }
  \item{DV}{ a logical for : do you want to calculate the De Vielder estimation }
}
\value{
  a list of the following components
  \item{CR }{the CR risk constant calculated with the value of R}
  \item{R }{the risk constant estimated by this estimation}
  \item{T}{the vector of observations of the random variable T}
  \item{lambdaHat}{the best estimation of lambda : the ESBVM of lambda}
  \item{muHat}{not available with this estimation}
  \item{lambdaEmp}{the estimation of the lambda of the De Vielder method (only available if \code{DV=TRUE} and if there is no problem with De Vielder method)}
  \item{muEmp}{the estimation of the mu of the De Vielder method (only available if \code{DV=TRUE})}
}
\author{ Christophe Dutang and Julie Barthes }
\examples{

#res<-estimDV("data.rda",TRUE,TRUE)

}
\keyword{ htest }
