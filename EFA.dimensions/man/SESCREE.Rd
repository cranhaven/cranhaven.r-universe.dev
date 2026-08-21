\name{SESCREE}
\alias{SESCREE}
\title{Standard Error Scree test}
\description{This is a linear regression operationalization of the 
	scree test for determining the number of components. The results are purportedly 
	identical to those from the visual scree test. The test is based on the standard 
	error of estimate values that are computed for the set of eigenvalues in a scree 
	plot. The number of components to retain is the point where the standard error 
	exceeds 1/m, where m is the numbers of variables.
}
\usage{SESCREE(data, Ncases=NULL, corkind, verbose=TRUE)}
\arguments{
 \item{data}{
 \code{}An all-numeric dataframe where the rows are cases & the columns are the variables,
        or a correlation matrix with ones on the diagonal. The function internally determines 
        whether the data are a correlation matrix.}

 \item{Ncases}{
  \code{}The number of cases. Required only if data is a correlation matrix.}

 \item{corkind}{
 \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
        The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required only if
        the entered data is not a correlation matrix.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\value{The number of components according to the Standard Error Scree test.}
\references{
	    Zoski, K., & Jurs, S. (1996). An objective counterpart to the visual scree test
        for factor analysis: the standard error scree test. 
        \emph{Educational and Psychological Measurement, 56(3),} 443-451.
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman correlation matrix
SESCREE(data_Harman, Ncases=305, verbose=TRUE)

# the Rosenberg Self-Esteem Scale (RSE) using Pearson correlations
SESCREE(data_RSE, corkind='pearson', verbose=TRUE)

# the Rosenberg Self-Esteem Scale (RSE) using polychoric correlations
SESCREE(data_RSE, corkind='polychoric', verbose=TRUE)

# the NEO-PI-R scales
SESCREE(data_NEOPIR, verbose=TRUE)
}
}
