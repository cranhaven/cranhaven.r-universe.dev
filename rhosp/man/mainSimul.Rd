\name{mainSimul}
\alias{mainSimul}
\title{ simulate many times the first model and calculate the risk constant }
\description{
  main simulates nbBed times the first model with the function simul
and calculates the risk constant R and CR by solving the renewal equation (star).
this renewal equation is only valid if the Xi forms a poisson process.
R and CR are defined such that the equivalent survival function is \eqn{CR *exp(-R*x)}.
}
\usage{
mainSimul(nbBed, nbPatient, disXi, disP, toplot = FALSE, calc = TRUE)
}
\arguments{
  \item{nbBed}{ the number of beds }
  \item{nbPatient}{ the number of patient in each bed  }
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disP}{ the distribution of the success probability of Zi : p :
disP is a 3 elements list : disfun stands for a distribution function
       ; nbparam for number of parameter of this distribution and param for a list of parameters
}
  \item{toplot}{ a logical variable to plot the variable Zi}
  \item{calc}{ should the risk constants calculate? }
}
\details{
make simulation and estimation on the sample
}
\value{
  Describe the value returned
  If it is a LIST, use
  \item{CR }{CR constant used in the exponential bound}
  \item{R }{the risk constant}
  \item{T}{the vector of durations between two declared side effects}
  \item{lambdaEmp}{estimate of lambda}
  \item{muEmp}{estimate of mu}
}
\author{ Christophe Dutang and Julie Barthes }

\examples{
arg1Exp <- list(rangen=rexp,nbparam=1,param=list(1/3));
arg1Bin <- list(rangen=rbinom,nbparam=2,param=list(1,1/20));
arg1Unif <- list(rangen=runif,nbparam=2,param=list(0,20));
arg1Lnorm <- list(rangen=rlnorm,nbparam=2,param=list(1/4,1));

arg2Exp <- list(disfun=pexp,nbparam=1,param=list(1/5));
arg2Cst <- list(disfun=pcst <- function(x,p) p ,nbparam=1,param=list(1/3));
arg2Comp <- list(disfun=pcomp <- function(x,mu1,mu2,mu3)
{1-1/3*exp(-mu1* x)-1/2*exp(-mu2 *x)-1/6*exp(-mu3 *x)}
,nbparam=3,param=list(1/3,1/5,1/10));
arg2Gamma <- list(disfun=pgamma,nbparam=2,param=list(3,1/3));
arg2Lnorm <- list(disfun=plnorm,nbparam=2,param=list(1/20,2));

T <- mainSimul(100,100,arg1Exp,arg2Exp)

}
\keyword{ survey }
