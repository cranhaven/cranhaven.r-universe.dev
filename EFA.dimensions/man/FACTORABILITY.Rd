\name{FACTORABILITY}
\alias{FACTORABILITY}
\title{Factorability of a correlation matrix}
\description{Three methods for assessing the factorability of a correlation matrix}
\usage{FACTORABILITY(data, corkind='pearson', Ncases=NULL, verbose=TRUE)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are 
  the variables, or a correlation matrix with ones on the diagonal. The function 
  internally determines whether the data are a correlation matrix.}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
  The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required 
  only if the entered data is not a correlation matrix.}

  \item{Ncases}{
  \code{}The number of cases for a correlation matrix. Required 
  only if the entered data is a correlation matrix.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
This function provides results from three methods of assessing whether a dataset or
correlation matrix is suitable for factor analysis:

1 -- whether the determinant of the correlation matrix is > 0.00001;

2 -- Bartlett's test of whether a correlation matrix is significantly different an identity matrix; and

3 -- the Kaiser-Meyer-Olkin measure of sampling adequacy.
}
\value{A list with the following elements:
 \item{chisq}{The chi-squared value for Bartlett,s test}
 \item{df}{The degrees of freedom for Bartlett,s test}
 \item{pvalue}{The significance level for Bartlett,s test}
 \item{Rimage}{The image correlation matrix}
 \item{KMO}{The overall KMO value}
 \item{KMOvars}{The KMO values for the variables}
}
\references{
{Bartlett, M. S. (1951). The effect of standardization on a chi square approximation in 
	factor analysis, \emph{Biometrika, 38,} 337-344.}
\cr\cr {Cerny, C. A., & Kaiser, H. F. (1977). A study of a measure of sampling adequacy 
	for factor-analytic correlation matrices. \emph{Multivariate Behavioral Research, 12(1),} 43-47.}
\cr\cr {Dziuban, C. D., & Shirkey, E. C. (1974). When is a correlation matrix appropriate for
	factor analysis? \emph{Psychological Bulletin, 81,} 358-361.}
\cr\cr {Kaiser, H. F., & Rice, J. (1974). Little Jiffy, Mark IV. \emph{Educational and
	Psychological Measurement, 34,} 111-117.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
FACTORABILITY(data_RSE, corkind='pearson')

FACTORABILITY(data_Field, corkind='pearson')
}
}
