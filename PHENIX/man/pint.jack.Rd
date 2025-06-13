\name{pint.jack}
\alias{pint.jack}
\title{Phenotypic integration (by Wagner) jackknife resampling}
\description{This function estimates the confidence intervals of the phenotypic integration index (Wagner, 1984) using jackknife resampling.}
\usage{pint.jack(traits,n.remove=1, verbose=FALSE)}
\arguments{
  \item{traits}{a dataframe or a matrix object containing traits as columns and individuals as rows}
  \item{n.remove}{a numeric, the number of individuals to remove on every jackknife iteration}
  \item{verbose}{a logic, if TRUE, values obtained during the jackknife resampling are also reported.}
}
\details{For every iteration, this function removes a given number of individuals before estimating phenotypic integration indices. Then, the process is repeated until all the possible combinations of this number of individuals have been removed. Rows containing missing values are removed.}
\value{A matrix with two columns and nine rows. Columns contain information for the uncorrected and corrected indices whereas rows contain mean, median, standard deviation (SD), standard error (SE), lower and higher confidence interval at 99\%, lower and higher confidence interval at 95\%, and the number of iterations.}

\references{
Cheverud, J.M., Wagner, G.P. & Dow, M.M. 1989. Methods for the comparative analysis of variation patterns. Systematic Zoology 38:201–213

Pavlicev, M., Cheverud, J.M. & Wagner, G.P. 2009. Measuring morphological integration using eigenvaluev ariance. Evolutionary Biology 36:157–170
}

\author{A.J. Muñoz-Pajares, R. Torices}

\seealso{\code{\link{pint}}, \code{\link{pint.p}}, \code{\link{pintsc.jack}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
pint.jack(tussilago[,1:5], 1) # Replicates are obtained removing one individual
pint.jack(tussilago[,1:5], 3) # Replicates are obtained removing three individuals
}
