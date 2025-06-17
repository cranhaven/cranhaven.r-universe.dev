\name{doseChoice}
\alias{doseChoice}

\title{
Choice of the next given dose level.
}

\description{
Algorithm giving the next dose which is the safe most successful dose (sMSD).
}

\usage{
doseChoice(probaTox, probaEff, p, targetTox, givenDose)
}

\arguments{
  \item{probaTox}{ 
	The probability of toxicity estimated with STAN model. 
}
  \item{probaEff}{
	The probability of efficacy estimated with STAN model.    
} 
  \item{p}{
	The probability of success.      
}
  \item{targetTox}{
	The target of toxicity.         
}
  \item{givenDose}{ 
	The vector of doses given to patients so far.
}   
} 

\references{
	Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
}

\author{
	Artemis Toumazi \email{artemis.toumazi@gmail.com},
    Caroline Petit \email{caroline.petit@crc.jussieu.fr}, 
    Sarah Zohar \email{sarah.zohar@inserm.fr}      
}    


\examples{
r <- 0.10
q <- 0.17
p <- 0.9
targetTox <- 0.6 
givenDose <- 2
newDose <- doseChoice(r, q, p, targetTox, givenDose)   
newDose
}
