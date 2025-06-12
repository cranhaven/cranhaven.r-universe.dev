\name{haplofreqs}

\alias{haplofreqs}


\title{ Haplotype frequencies }

\description{
 Compute haplotype frequencies
}

\usage{
haplofreqs(dat, firstl, lastl)
}

\arguments{
  \item{dat}{ A binary matrix, rows are haplotypes and columns are binary markers }
  \item{firstl}{ Position of the first locus }
  \item{lastl}{ Position of the last locus }
}

%\details{}

\value{A vector of haplotype frequencies}

\references{
Montana, G. HapSim: a simulation tool for generating haplotype data with
pre-specified allele frequencies and LD coefficients. 2005.
}

\author{Giovanni Montana}

%\note{ ~~further notes~~ }

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{
data(ACEdata)
freqs <- haplofreqs(ACEdata, 17, 22)
}

\keyword{ utilities }
