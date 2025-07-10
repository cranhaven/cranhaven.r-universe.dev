\name{MGR}
\alias{MGR}
\title{Mean Germination Rate}
\usage{
MGR(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Mean Germination Rate (Labouriau, 1983). It is 1/MGT.
}

\references{
LABOURIAU, L. G. (1983). Uma nova linha de pesquisa na fisiologia da germina??o das sementes. Anais do XXXIV Congresso Nacional de Botanica. SBB, Porto Alegre, 11-50.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
MGR(time,nger)
}
