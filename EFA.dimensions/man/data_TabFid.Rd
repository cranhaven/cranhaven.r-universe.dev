\name{data_TabFid}
\alias{data_TabFid}
\docType{data}
\title{data_TabFid}
\description{A data frame with scores for 340 cases on 44 Bem Sex Role Inventory items, used 
	by Tabacknick & Fidell (2013, p. 656) in their chapter on exploratory factor analysis.}
\usage{data(data_TabFid)}
\references{Tabachnik, B. G., & Fidell, L. S. (2013). \emph{Using multivariate statistics.} New York, NY: Pearson.}
\examples{
\donttest{
# MAP test on the data_TabFid data
MAP(data_TabFid, corkind='pearson', verbose=TRUE)

# parallel analysis of the data_TabFid data
RAWPAR(data_TabFid, extraction='PCA', Ndatasets=100, percentile=95,
       corkind='pearson', verbose=TRUE)
       
# DIMTESTS on the data_TabFid data
DIMTESTS(data_TabFid, tests = c('EMPKC','HULL','RAWPAR'), corkind='pearson', display=1)

# principal axis factor analysis of the data_TabFid data
EFA(data_TabFid, corkind='pearson', extraction='paf', Nfactors = 5, iterpaf = 50, 
      rotation='promax', ppower = 4, verbose=TRUE)       
}
}
