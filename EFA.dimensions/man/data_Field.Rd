\name{data_Field}
\alias{data_Field}
\docType{data}
\title{data_Field}
\description{A data frame with scores on 23 variables for 2571 cases. This is a simulated 
	dataset that has the exact same correlational structure as the "R Anxiety Questionnaire"   
	data used by Field et al. (2012) in their chapter on Exploratory Factor Analysis.}
\usage{data(data_Field)}

\source{
Field, A., Miles, J., & Field, Z. (2012). \emph{Discovering statistics using R.} Los Angeles, CA: Sage.}

\examples{
\donttest{
# MAP test
MAP(data_Field, corkind='pearson', verbose=TRUE)

# DIMTESTS
DIMTESTS(data_Field, corkind='pearson', 
         tests = c('CD','EMPKC','HULL','RAWPAR','NEVALSGT1'), display=2)

# principal components analysis	
PCA(data_Field, corkind='pearson', Nfactors=4, rotation='none', verbose=TRUE)
}
}
