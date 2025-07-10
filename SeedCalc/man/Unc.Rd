\name{Unc}
\alias{Unc}
\title{Germination Uncertainty}
\usage{
Unc(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the germination Uncertainty (Labouriau & Valadares, 1976).
}

\references{
LABOURIAU LG; VALADARES MB. 1976. On the germination of seeds of Calotropis procera. Anais da Academia Brasileira de Ciencias 48:174-186. LIFCHITZ A. 1981. Plantas medicinales. 5. ed. Buenos Aires: Kier. 139p.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
Unc(time,nger)
}
