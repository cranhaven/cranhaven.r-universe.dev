\name{divlocus}

\alias{divlocus}

\title{Diversity score}

\description{
Compute a measure of genetic diversity at each locus
}

\usage{
divlocus(dat)
}

\arguments{
  \item{dat}{ A binary matrix, rows are haplotypes and columns are binary markers }
}
  
\details{This function implements a measure of diversity for a locus \eqn{j}
as in Clayton (2002). If \eqn{z_ij} represents the allele \eqn{j} of haplotype \eqn{i}, for
\eqn{i=1,...,N} and assuming that alleles are coded as 0 and 1, the
diversity measure can be written as
\deqn{D_j = 2*N( \sum_{i=1}^N z_{ij}^2 - (\sum_{i=1}^N z_{ij}) ^2 )}}

\value{
A vector containing the diversity measure for all markers
}

\references{D. Clayton. Choosing a set of haplotype tagging SNPs from a
larger set of diallelic loci. 2002. \url{www-gene.cimr.cam.ac.uk/clayton/software/stata/htSNP/htsnp.pdf}}

\author{Giovanni Montana}

%\note{ ~~further notes~~ }

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{
data(ACEdata)
divlocus(ACEdata)
}

\keyword{ utilities }
