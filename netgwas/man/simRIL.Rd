\name{simRIL}
\alias{simRIL}

\title{Generate genotype data of RIL}

\description{
Generating genotype data from a recombinant inbred line (RIL) population.
}

\usage{
simRIL( d = 25, n = 200, g = 5, cM = 100, selfing=2 )
}

\arguments{
	\item{d}{The number of markers per chromosome. The default value is 25.}
	\item{n}{The number of sample size (observations). The default value is 200.}
	\item{g}{The number of linkage groups (chromosomes). The default value is 5.}
	\item{cM}{The length of each chromosome based on centiMorgan.}
	\item{selfing}{ The number of selfing in RIL population.}
}

\value{
 \item{data}{The generated RIL genotype data as an \code{n} by \code{(d x g)} matrix.}
 \item{map}{The genetic map of the data.}
}

\author{ 
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi <pariya.behrouzi@gmail.com>
}

\seealso{
\code{\link{netmap}}, \code{\link{netsnp}}, and \code{\link{netgwas-package}}
}

\examples{
#genome-like graph structure
ril <- simRIL(g = 5, d = 25, cM = 100, n = 200, selfing = 2)
geno <- ril$data; image(geno, xlab= "individuals", ylab="markers")
map <- ril$map 
}