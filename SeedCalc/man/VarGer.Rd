\name{VarGer}
\alias{VarGer}
\title{Variance of Germination Time}
\usage{
VarGer(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Variance of Germination Time (Labouriau, 1983).
}

\references{
LABOURIAU, L. G. (1983). Uma nova linha de pesquisa na fisiologia da germinacao das sementes. Anais do XXXIV Congresso Nacional de Botanica. SBB, Porto Alegre, 11-50.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
VarGer(time,nger)
}
