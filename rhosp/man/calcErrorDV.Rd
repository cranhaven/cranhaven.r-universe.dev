\name{calcErrorDV}
\alias{calcErrorDV}
\title{ compute the bias, variance of the De Vielder approximation  }
\description{
	calcErrorDV computes the risk constant R with the \emph{De Vielder} estimator
	and its bias and variance.
}
\usage{
calcErrorDV(file, nb = 10, disXi, disP, plot = TRUE)
}
\arguments{
  \item{file}{ the file in which the simulated data will be stored }
  \item{nb}{ the number of simulation }
\item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disP}{ the side effect probability (success  probability of Zi) p :
disP is a 3 elements list : disfun stands for a distribution function
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{plot}{ a logical variable to plot the variable Zi }
}
\value{
  a list of the following components
  \item{bias }{the bias of this estimator}
  \item{var }{the variance of this estimator}
  \item{R}{the risk constant estimated}
  \item{CR}{the CR risk constant calculated with R}
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
\dontrun{
arg1Exp<-list(rangen=rexp,nbparam=1,param=list(1/3));
arg2Exp<-list(disfun=pexp,nbparam=1,param=list(1/5));
res<-calcErrorDV("data.rda",25,arg1Exp,arg2Exp,TRUE)
}
}
\keyword{ utilities }

