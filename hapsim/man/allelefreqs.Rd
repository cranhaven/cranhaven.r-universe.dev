\name{allelefreqs}

\alias{allelefreqs}

\title{Estimates allele frequencies}

\description{Estimates allele frequencies from a binary matrix}

\usage{allelefreqs(dat)}


\arguments{
  \item{dat}{ A binary matrix, rows are haplotypes and columns are binary markers }
}

% \details{}


\value{
  A list containing:
  \item{freqs}{Vector of allele "0" frequencies}
  \item{all.polym}{If TRUE, all loci are polymorphic}
  \item{non.polym}{Vector of non-polymorphic loci, if any}
}

\references{
Montana, G. HapSim: a simulation tool for generating haplotype data with
pre-specified allele frequencies and LD coefficients. 2005.
}


\author{Giovanni Montana}

%\note{ ~~further notes~~ }

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{

data(ACEdata)
x <- allelefreqs(ACEdata)
hist(x$freqs)

}

\keyword{ utilities }
