\name{ccls}
\alias{ccls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pairwise Constrained Clustering by Local Search
}
\description{
This function takes an unlabeled dataset and two lists of must-link and cannot-link constraints as input and produce a clustering as output.
}
\usage{
ccls(data, k = -1, mustLink, cantLink, maxIter = 1, tabuIter = 100, tabuLength = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The unlabeled dataset.
}
  \item{k}{
Number of clusters.
}
  \item{mustLink}{
A list of must-link constraints
}
  \item{cantLink}{
A list of cannot-link constraints
}
  \item{maxIter}{
Number of iteration
}
  \item{tabuIter}{
Number of iteration in Tabu search
}
  \item{tabuLength}{
The number of elements in the Tabu list
}
}
\details{
This algorithm minimizes the clustering cost function using Tabu search.
}
\value{
A vector that represents the labels (clusters) of the data points
}
\references{
Tran Khanh Hiep, Nguyen Minh Duc, Bui Quoc Trung (2016), Pairwise Constrained Clustering by Local Search.
}
\author{
Tran Khanh Hiep
Nguyen Minh Duc
}
\note{
This is the first algorithm for pairwise constrained clustering by local search.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Tran Khanh Hiep, Nguyen Minh Duc, Bui Quoc Trung (2016), Pairwise Constrained Clustering by Local Search.
}
\examples{
data = matrix(c(0, 1, 1, 0, 0, 0, 1, 1), nrow = 4)
mustLink = matrix(c(1, 2), nrow = 1)
cantLink = matrix(c(1, 4), nrow = 1)
k = 2
pred = ckmeans(data, k, mustLink, cantLink)
pred
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ccls }
\keyword{ constraint }% __ONLY ONE__ keyword per line
\keyword{ clustering }
\keyword{ local search }
\keyword{ Tabu search }