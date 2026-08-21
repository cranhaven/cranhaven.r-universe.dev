\name{data_Harman}
\alias{data_Harman}
\docType{data}
\title{Correlation matrix from Harman (1967, p. 80).}
\description{The correlation matrix for eight physical variables for 305 cases from Harman (1967, p. 80).}
\usage{data(data_Harman)}
\references{Harman, H. H. (1967). \emph{Modern factor analysis (2nd. ed.).} Chicago: University of Chicago Press.}
\examples{
\donttest{
# MAP test on the Harman correlation matrix
MAP(data_Harman, verbose=TRUE)

# DIMTESTS on the Harman correlation matrix
DIMTESTS(data_Harman, tests = c('EMPKC','HULL','RAWPAR','NEVALSGT1'), Ncases=305, display=2)

# parallel analysis of the Harman correlation matrix
RAWPAR(data_Harman, extraction='PCA', Ndatasets=100, percentile=95,
       Ncases=305, verbose=TRUE)
}
}
