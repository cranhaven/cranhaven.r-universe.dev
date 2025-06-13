\name{pintsc.jack}
\alias{pintsc.jack}
\title{size-controlled phenotypic integration jackknife resampling}
\description{This function estimates the confidence intervals of the phenotypic integration index controlling by a third variable using jackknife resampling.}
\usage{
pintsc.jack(traits, control=NA, n.remove = 1, verbose=FALSE)
}

\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
  \item{control}{a vector containing the variable to control by size.}
  \item{n.remove}{a numeric, the number of individuals to remove on every jackknife iteration}
  \item{verbose}{a logic, if TRUE, values obtained during the jackknife resampling are also reported.}
}
\details{For every iteration, this function removes a given number of individuals before estimating phenotypic integration indices. Then, the process is repeated until all the possible combinations of this number of individuals have been removed. Rows containing missing values are removed.}

\value{A matrix with two columns and nine rows. Columns contain information for the uncorrected and corrected indices whereas rows contain mean, median, standard deviation (SD), standard error (SE), lower and higher confidence interval at 99\%, lower and higher confidence interval at 95\%, and the number of randomizations.}

\author{A.J. Muñoz-Pajares, R. Torices}

\seealso{\code{\link{pintsc}}, \code{\link{pint.boot}}, \code{\link{pint.jack}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grams) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)

# Iit is mandatory to define the "control" variable. Otherwise, it gives an error:
# pintsc.jack(tussilago, 1)
pintsc.jack(traits=tussilago[,1:5], control=tussilago[,6], 1)
}
