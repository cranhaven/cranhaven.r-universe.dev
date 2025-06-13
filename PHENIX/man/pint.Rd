\name{pint}
\alias{pint}
\title{Phenotypic integration index by Wagner}

\description{Estimates phenotypic integration indices based on the variance of the eigenvalues of the correlation matrix between phenotypic traits following Wagner (1984).}
\usage{pint(traits)}
\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
	}

\details{This function estimates the phenotypic integration index (PINT) using the correlation
matrix following Wagner (1984), as well as a phenotypic integration index (PINT.c) corrected by the number
of traits and individuals of each population. The uncorrected estimate is also expressed as percentage 
depending on the maximum possible integration levels.}

\value{ A list with five elements containing:
\item{PINT:}{The phenotypic integration index.}
\item{RelPINT:}{Percentage of maximum possible integration.}
\item{PINT.c:}{Corrected phenotypic integration index.}
\item{N:}{Number of observations used.}
}
\references{
Cheverud, J.M., Wagner, G.P. & Dow, M.M. 1989. Methods for the comparative analysis of variation patterns. Systematic Zoology 38:201–213

Pavlicev, M., Cheverud, J.M. & Wagner, G.P. 2009. Measuring morphological integration using eigenvaluev ariance. Evolutionary Biology 36:157–170

Wagner, G.P. 1984. On the eigenvalue distribution of genetic and phenotypic dispersion matrices: evidence for a nonrandom organization of quantitative character variation. Journal of Mathematical Biology 21:77–95
}
\author{R. Torices, A.J. Muñoz-Pajares}

\seealso{\code{\link{pint.p}}, \code{\link{pint.boot}}, \code{\link{pint.jack}}, \code{\link{pintsc}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grams) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
pint (tussilago[,1:5]) # the last column is not included since represents the total size
# NOTE that the number of observations used by the function was 18 instead 29 that were
# included in the 'tussilago' data set. Missing values were removed. 
}
