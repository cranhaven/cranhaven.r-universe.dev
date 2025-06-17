\name{cell}
\alias{cell}
\docType{data}
\title{
cell imaging metrics dataset
}
\description{
This cell dataset contains the first three principal components of the imaging metrics for 444 cells that were engineered into four patterns. The dataset therefore should include four clusters of cell samples in theory. See the references for  more experimental and imaging analysis details of this data.
}
\usage{data("cell")}
\format{
  A data frame with 444 observations on the following 3 variables.
  \describe{
    \item{\code{PC1}}{The first principal component of imaging metrics}
    \item{\code{PC2}}{The second principal component of imaging metrics}
    \item{\code{PC3}}{The third principal component of imaging metrics}
  }
}
\references{
Slater, John, et al. "Recapitulation and Modulation of the Cellular Architecture of a User-Chosen Cell-of-Interest Using Cell-Derived, Biomimetic Patterning." ACS nano (2015).\cr
Hu, C.W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific reports 5 (2015). \cr
}
\examples{
data(cell)
}
\keyword{datasets}
