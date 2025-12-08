\name{mainSimul2}
\alias{mainSimul2}
\title{ simulate many times the second model }
\description{
 main simulates nbBed times the second model with the function simul
}
\usage{
mainSimul2(nbBed, nbPatient, disXi, disYi, toplot = FALSE)
}
\arguments{
  \item{nbBed}{ the number of beds }
  \item{nbPatient}{ the number of patient in each bed }
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disYi}{ the distribution of the variable Yi :
disYi is a 3 elements list : rangen for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{toplot}{ a logical variable to plot the variable Zi }
}
\details{
   If necessary, more details than the description  above 
}
\value{
	return a list of the following components
  \item{T }{T the vector of durations between two declared side effects}
  \item{R }{R a risk constant}
  \item{CR}{CR a risk constant}
}
\author{ Christophe Dutang and Julie Barthes }

\examples{

arg1ExpMod2 <- list(rangen=rexp,nbparam=1,param=list(1/2));
arg1BinMod2 <- list(rangen=rbinom,nbparam=2,param=list(1,1/20));
arg1UnifMod2 <- list(rangen=runif,nbparam=2,param=list(0,20));
arg1LnormMod2 <- list(rangen=rlnorm,nbparam=2,param=list(1/4,1));

arg2ExpMod2 <- list(rangen=rexp,nbparam=1,param=list(1/5));
# arg2-cst <- list(rangen=pcst <- function(x,p) p ,nbparam=1,param=list(1/10));
arg2GammaMod2 <- list(rangen=rgamma,nbparam=2,param=list(3,1/5));

T <- mainSimul2(100,100,arg1ExpMod2,arg2ExpMod2)


}
\keyword{ survey }
