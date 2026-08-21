\name{data_NEOPIR}
\alias{data_NEOPIR}
\docType{data}
\title{data_NEOPIR}
\description{A data frame with scores for 1000 cases on 30 variables that have  
	the same intercorrelations as those for the Big 5 facets on pp. 100-101 of 
	the NEO-PI-R manual (Costa & McCrae, 1992).}
\usage{data(data_NEOPIR)}
\references{Costa, P. T., & McCrae, R. R. (1992). \emph{Revised NEO personality 
		inventory (NEO-PIR) and NEO five-factor inventory (NEO-FFI): Professional 
		manual.} Odessa, FL: Psychological Assessment Resources.}
\examples{
\donttest{
# MAP test on the data_NEOPIR data
MAP(data_NEOPIR, corkind='pearson', verbose=TRUE)

# DIMTESTS on the data_NEOPIR data
DIMTESTS(data_NEOPIR, tests = c('EMPKC','HULL','RAWPAR','NEVALSGT1'), Ncases=1000, display=2)

# parallel analysis of the data_NEOPIR data
RAWPAR(data_NEOPIR, extraction='PCA', Ndatasets=100, percentile=95,
       corkind='pearson', verbose=TRUE)
}
}
