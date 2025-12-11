\name{Prototypes}
\alias{Prototypes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prototypes of groups
}
\description{
Prototypes are `representative' cases of a group of data points, given the similarity matrix among the points. They are very similar to medoids.
}
\usage{
Prototypes(label, x, prox, nProto = 5, nNbr = floor((min(table(label)) - 1)/nProto))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{
the response variable. Should be a factor.
}
  \item{x}{
matrix or data frame of predictor variables.
}
  \item{prox}{
the proximity (or similarity) matrix, assumed to be symmetric with 1 on the diagonal and in [0, 1] off the diagonal (the order of row/column must match that of x)
}
  \item{nProto}{
number of prototypes to compute for each value of the response variables.
}
  \item{nNbr}{
number of nearest neighbors used to find the prototypes.
}
}
\details{
For each case in x, the nNbr nearest neighors are found. Then, for each class, the case that has most neighbors of that class is identified. The prototype for that class is then the medoid of these neighbors (coordinate-wise medians for numerical variables and modes for categorical variables). One then remove the neighbors used and iterate the first steps to find a second prototype, etc.
}

\value{
A list of data frames with prototypes. The number of data frames is equal to the number of classes of the response variable.
}

\references{
Random Forests, by Leo Breiman and Adele Cutler
https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#prototype
}
\author{
Nicolas Robette
}
\note{
The code is an extension of \code{classCenter} function in \code{randomForest} package.
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  prox=proximity(iris.cf)
  Prototypes(iris2$Species,iris2[,1:4],prox)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
