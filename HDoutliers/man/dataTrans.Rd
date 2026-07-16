\name{dataTrans}
\alias{dataTrans}
\title{
  Data Transformation for Leland Wilkinson's \emph{hdoutliers} Algorithm
}
\description{
 Transforms the data according to the specifications in Wilkinson's
 \emph{hdoutliers} algorithm.
}

\usage{
dataTrans(data) 
}

\arguments{
  \item{data}{
    A vector, matrix, or data frame consisting of numeric and/or categorical
    variables.
  }
}

\details{
  Replaces each categorical variables with a numeric variable corresponding
  to its first component in multiple correspondence analysis, then maps the
  data to the unit square. There is no porvision for handling missing data.
  Functions \code{HDoutliers} and \code{getHDoutliers} apply this 
  transformation to their input data.
}

\value{
  The transformed data, according to Wilkinson's specifications for the
  \emph{hdoutliers} algorithm.
}

\section{References}{
  Wilkinson, L. (2016). Visualizing Outliers.
}
\seealso{
  \code{\link{HDoutliers}},
  \code{\link{getHDoutliers}}
}

\examples{
 require(FactoMineR)
 data(tea)
 head(tea)
 dataTrans(tea[,-1])
}

\keyword{cluster}
