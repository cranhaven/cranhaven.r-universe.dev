\name{Outliers}
\alias{Outliers}
\title{Computes outliers}
\description{Computes outlierness scores and detects outliers.}
\usage{
  Outliers(prox, cls=NULL, data=NULL, threshold=10)
}
\arguments{
  \item{prox}{a proximity matrix (a square matrix with 1 on the diagonal and values between 0 and 1 in the off-diagonal positions).}
  \item{cls}{Factor. The classes the rows in the proximity matrix belong to. If NULL (default), all data are assumed to come from the same class.}
  \item{data}{A data frame of variables to describe the outliers (optional).}
  \item{threshold}{Numeric. The value of outlierness above which an observation is considered an outlier. Default is 10.}
}

\details{
The outlierness score of a case is computed as n / sum(squared proximity), normalized by
  subtracting the median and divided by the MAD, within each class.
}

\value{
  A list with the following elements :
  \item{scores}{numeric vector containing the outlierness scores}
  \item{outliers}{numeric vector of indexes of the outliers, or a data frame with the outliers and their characteristics}
}

\note{
  The code is adapted from \code{outlier} function in \code{randomForest} package.
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  prox=proximity(iris.cf)
  Outliers(prox, iris2$Species, iris2[,1:4])
}

\keyword{classif}