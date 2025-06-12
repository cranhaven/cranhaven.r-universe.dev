\name{haplodata}

\alias{haplodata}

\title{Haplotype object creator}

\description{Creates an haplotype data object needed for simulating haplotypes with \code{haplosim}. This object also contains some summary statistics about the real data.}

\usage{haplodata(dat)}

\arguments{
  \item{dat}{ A binary matrix, rows are haplotypes and columns are binary markers }
}

%\details{}

\value{
  A list containing:
  \item{freqs}{Allele frequencies}
  \item{cor}{Correlation matrix (LD coefficients) }
  \item{div}{Locus-specific diversity measure }
  \item{cov}{Covariance matrix for the normal distribution}
}

\references{
Montana, G. HapSim: a simulation tool for generating haplotype data with
pre-specified allele frequencies and LD coefficients. 2005.
}

\author{Giovanni Montana}

% \note{ ~~further notes~~ }

\seealso{See also \code{ \link{haplosim} } }

\examples{

data(ACEdata)

# creates the haplotype object
x <- haplodata(ACEdata) 

# simulates 100 random haplotypes
y <- haplosim(100, x) 

}

\keyword{ utilities }
