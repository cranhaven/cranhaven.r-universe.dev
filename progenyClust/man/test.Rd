\name{test}
\alias{test}
\docType{data}
\title{
3-cluster 2-dimensional test dataset
%%   ~~ data name/kind ... ~~
}
\description{
This test dataset contains 3 clusters centered around (-1,2),(2,0) and (-1,-2) in a 2-dimensional space. Each cluster consists of 50 samples that were drawn from bivariate normal distributions with a common identity covariance matrix.
}
\usage{data("test")}
\format{
  A data frame with 150 observations on the following 2 variables. 
  \describe{
  \item{V1}{numeric vector of coordinates in x axis}
  \item{V2}{numeric vector of coordinates in y axis}
  }
}

\references{
Hu, C.W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific reports 5 (2015). \cr
http://www.nature.com/articles/srep12894
}
\examples{
data(test)
}
\keyword{datasets}
