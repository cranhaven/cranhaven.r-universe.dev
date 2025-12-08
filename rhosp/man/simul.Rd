\name{simul}
\alias{simul}
\title{ simulate the first model of the hospital risk }
\description{
  simul simulate the first model of the hospital that is to say 
simulating the sequence of variables Xi (which follow the disxi distribution
passed as an argument) and the sequence of Zi (where the probability of
succes p is passed as an argument)
    simul returns T the vector of durations between two declared side effects 
}
\usage{
simul(nbPatient, disXi, disP, toplot = TRUE)
}
\arguments{
  \item{nbPatient}{the number of patient of the simulation }
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disP}{ the side effect probability (success  probability of Zi) p :
disP is a 3 elements list : disfun stands for a distribution function
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{toplot}{ a logical variable to plot the variable Zi }
}
\value{
  T the vector of durations between two declared side effects

}
\author{ Christophe Dutang and Julie Barthes }
\note{ further notes }


\seealso{ simul2  }
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

T <- simul(100,arg1Exp,arg2Exp)
T <- simul(100,arg1Bin,arg2Cst)
T <- simul(100,arg1Unif,arg2Comp)
T <- simul(100,arg1Lnorm,arg2Gamma)


}
\keyword{ utilities }
