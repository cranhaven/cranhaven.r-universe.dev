\name{GermCalc}
\alias{GermCalc}
\title{GermCalc Function}

\usage{
GermCalc(germdata, NSeeds)
}

\arguments{
  \item{germdata}{ A \code{data.frame} object. The first column is the time for germination, and the others are the total number of seeds germinated until each time}
  \item{NSeeds}{ The total amount of seeds used for the germination or emergence test. }
}

\description{
Calculates all indices for seed germination or seedling emergence.
}

\author{
Laercio Junio da Silva \email{laerciojdsilva@gmail.com.br}
Andre Dantas de Medeiros \email{medeiros.seeds@gmail.com }
Ariadne Morbeck Santos Oliveira  \email{ariadneoliveira86@gmail.com}
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
rep1 <- c(0,2,4,15,25,38,45,50,50,50)
rep2 <- c(0,4,6,18,22,39,40,48,50,50)
germdata <- data.frame(time,rep1,rep2)
GermCalc(germdata, 50)
}


\keyword{ indexes }
