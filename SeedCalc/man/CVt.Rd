\name{CVt}
\alias{CVt}
\title{Germination Time Coefficient of variation}
\usage{
CVt(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Germination Time Coefficient of variation (Carvalho et al., 2005).
}

\references{
CARVALHO, M. P.; SANTANA, D. G.; RANAL, M. A. Emergência de plântulas de Anacardium humile A. St.-Hil. (Anacardiaceae) avaliada por meio de amostras pequenas. Revista Brasileira de Botânica, v. 28, n. 3, p. 627–633, 2005.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
CVt(time,nger)
}
