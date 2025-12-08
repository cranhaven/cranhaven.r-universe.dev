\name{makeSample}
\alias{makeSample}
\title{ create a sample of the first model stored in a file  }
\description{
   make a sample of the first model, that is to say simulate 
   the sequence of the random variables Xi (stay duration) and the sequence of Zi (side effect reporting)
}
\usage{
makeSample(file, nbPatient, disXi, disP)
}
\arguments{
  \item{file}{ the filename in which the simulation will be stored }
  \item{nbPatient}{ the number of patients for the simulation }
  \item{disXi}{ the distribution of the variable Xi :
disXi is a 3 elements list : rangen stands for  a random positive variable generator
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
  \item{disP}{ the side effect probability (success  probability of Zi) p :
disP is a 3 elements list : disfun stands for a distribution function
       ; nbparam for number of parameter of this distribution and param for a list of parameters
 }
}
\value{
a NULL object.
}
\author{ Christophe Dutang and Julie Barthes }
\examples{
arg1Exp<-list(rangen=rexp,nbparam=1,param=list(1/3));

arg2Exp<-list(disfun=pexp,nbparam=1,param=list(1/5));

makeSample("mydata.rda",200,arg1Exp,arg2Exp)
}
\keyword{ file }

