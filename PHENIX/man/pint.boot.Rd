\name{pint.boot}
\alias{pint.boot}
\title{Phenotypic integration (by Wagner) bootstrap intervals}
\description{This function estimates the confidence intervals of the phenotypic integration index (Wagner, 1984) using bootstrap.}
\usage{pint.boot(traits, replicates = 1000, verbose=FALSE)}
\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
  \item{replicates}{a numeric, the number of randomizations (1000 by default)}
  \item{verbose}{a logic, if TRUE, values obtained during the bootstrap resampling are also reported.}
}
\details{Rows containing missing values are removed.}
\value{A matrix with two columns and nine rows. Columns contain information for the uncorrected and corrected indices whereas rows contain mean, median, standard deviation (SD), standard error (SE), lower and higher confidence interval at 99\%, lower and higher confidence interval at 95\%, and the number of randomizations.}

\references{
Cheverud, J.M., Wagner, G.P. & Dow, M.M. 1989. Methods for the comparative analysis of variation patterns. Systematic Zoology 38:201–213

Pavlicev, M., Cheverud, J.M. & Wagner, G.P. 2009. Measuring morphological integration using eigenvaluev ariance. Evolutionary Biology 36:157–170
}

\author{R. Torices, A.J. Muñoz-Pajares}

\seealso{\code{\link{pint}}, \code{\link{pint.p}}, \code{\link{pintsc.boot}}, \code{\link{pint.jack}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
pint.boot(tussilago[,1:5], 100)
# The phenotypic integration intervals do not include 0.
# For faster execution, we used 100 replicates in this example.
# For real data set increase this number:
# pint.boot(tussilago[,1:5], 5000)
}
