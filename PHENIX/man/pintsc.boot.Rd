\name{pintsc.boot}
\alias{pintsc.boot}
\title{size-controlled phenotypic integration bootstrap intervals}
\description{This function estimates the confidence intervals of the phenotypic integration index controlling by a third variable using bootstrap.}
\usage{
pintsc.boot(traits, control=NA, replicates = 1000, verbose=FALSE)
}

\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
  \item{control}{a vector containing the variable to control by size.}
  \item{replicates}{a numeric, the number of randomizations (1000 by default).}
  \item{verbose}{a logic, if TRUE, values obtained during the bootstrap resampling are also reported.}
}
\details{Rows containing missing values are removed.}

\value{A matrix with two columns and nine rows. Columns contain information for the uncorrected and corrected indices whereas rows contain mean, median, standard deviation (SD), standard error (SE), lower and higher confidence interval at 99\%, lower and higher confidence interval at 95\%, and the number of randomizations.}

\author{R. Torices, A.J. Muñoz-Pajares}

\seealso{\code{\link{pintsc}}, \code{\link{pint.boot}}, \code{\link{pintsc.jack}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grams) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)

# Iit is mandatory to define the "control" variable. Otherwise, it gives an error:
# pintsc.boot(tussilago, 5000)
pintsc.boot(traits=tussilago[,1:5], replicates=100, control=tussilago[,6])
# The phenotypic integration index intervals do not include 0.
# For faster execution, we used 100 replicates in this example.
# For real data set increase this number:
# pintsc.boot(traits=tussilago[,1:5], replicates=5000, control=tussilago[,6])
}
