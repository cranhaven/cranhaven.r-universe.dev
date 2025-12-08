\name{simul2}
\alias{simul2}
\title{ simulate the second model of the hospital risk }
\description{
  simul simulate the first model of the hospital that is to say 
simulating the sequence of variables Xi (which follow the disxi distribution
passed as an argument) and the sequence of Zi (where the probability of
succes p is passed as an argument)
    simul returns T the vector of durations between two declared side effects 
}
\usage{
simul2(nbPatient, disXi, disYi, toplot = TRUE)
}
\arguments{
  \item{nbPatient}{ the number of patient of the simulation}
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disYi}{ the distribution of the variable Yi :
disYi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{toplot}{ a logical variable to plot the variable Zi}
}
\value{
  T the vector of durations between two declared side effects
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
arg1Exp<-list(rangen=rexp,nbparam=1,param=list(1/2));
arg2Exp<-list(rangen=rexp,nbparam=1,param=list(1/20));
T<-simul2(100,arg1Exp,arg2Exp)

}
\keyword{ utilities }
