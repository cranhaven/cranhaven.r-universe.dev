\name{SCREE_PLOT}
\alias{SCREE_PLOT}
\title{Scree plot of eigenvalues}
\description{Produces a scree plot of eigenvalues for raw data or for a correlation matrix.}
\usage{SCREE_PLOT(data, corkind, Ncases, verbose)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are 
  the variables, or a correlation matrix with ones on the diagonal.The function 
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
\value{ \item{totvarexpl}{The eigenvalues and total variance explained}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# Field's RAQ factor analysis data
SCREE_PLOT(data_Field, corkind='pearson')

# the Harman (1967) correlation matrix
SCREE_PLOT(data_Harman)

# Rosenberg Self-Esteem scale items
SCREE_PLOT(data_RSE, corkind='polychoric')

# NEO-PI-R scales
SCREE_PLOT(data_RSE)
}
}
