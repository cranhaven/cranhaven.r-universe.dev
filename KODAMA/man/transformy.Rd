\name{transformy}

\alias{transformy}

\title{Conversion Classification Vector to Matrix}

\description{This function converts a classification vector into a classification matrix.}

\usage{
transformy(y)
}

\arguments{
  \item{y}{a vector or factor.}
}

\details{
	This function converts a classification vector into a classification matrix.
}


\value{
    A matrix.
  }


\author{Stefano Cacciatore and Leonardo Tenori}

\references{
Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}
  \examples{
y=rep(1:10,3)
print(y)
z=transformy(y)
print(z)
}

\keyword{transformation}
