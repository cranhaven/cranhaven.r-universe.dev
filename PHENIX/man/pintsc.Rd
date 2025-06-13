\name{pintsc}
\alias{pintsc}
\title{size-controlled phenotypic integration}
\description{This function estimates the phenotypic integration indices controlling by a third variable.}
\usage{pintsc(traits,control=NA)}
\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
  \item{control}{a vector containing the variable to control by size.}
}
\details{This function estimates the same set of indices than \code{\link{pint}} but from partial correlation
matrix instead of the correlation matrix: PINTsc: The variance of the eigenvalues using the partial correlation
matrix instead of a correlation matrix. Torices & Méndez (2014) proposed this index for analysing phenotypic
integration of resource allocation to different components in an individual or a given organ when resource
allocation data come from observational studies in which resource availability is not controlled and therefore
could lead to components correlations only by the fact of different resource availability. Thus, PINT can be
estimated using the matrix of partial correlations in which size of the organ or individual is used as the third
control variable. PINTsc.c: The PINTsc corrected by the number of traits and individuals of each population.
RelPINTsc: The same as RelPINT but estimated with the partial correlation matrix.

Rows containing missing values are removed.}
\value{ A list with five elements containing:
\item{PINTsc:}{The phenotypic integration index.}
\item{RelPINTsc:}{Percentage of maximum possible integration.}
\item{PINTsc.c:}{Corrected phenotypic integration index.}
\item{N:}{Number of observations used.}
}
\references{Torices, R. & Méndez, M. 2014. Resource allocation to inflorescence components is highly integrated despite differences between allocation currencies and sites. International Journal of Plant Science 175(6):713–723.}
\author{R. Torices, A.J. Muñoz-Pajares}

\seealso{\code{\link{pint}}, \code{\link{pintsc.boot}}, \code{\link{pintsc.p}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grams) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
#'TOTALSIZE'

data(tussilago)

# To estimate the PINT indices controlling by a third variable
# it is mandatory to define the "control" variable. Otherwise, it gives an error:
#
# pintsc(traits=tussilago)
pintsc(traits=tussilago[,1:5],control=tussilago[,6])
}
