\name{ckmeans}
\alias{ckmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
COP K-means algorithm
}
\description{
This function takes an unlabeled dataset and two lists of must-link and cannot-link constraints as input and produce a clustering as output.
}
\usage{
ckmeans(data, k, mustLink, cantLink, maxIter = 100)
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
}
\details{
This algorithm produces a clustering that satisfies all given constraints.
}
\value{
A vector that represents the labels (clusters) of the data points
}
\references{
Wagstaff, Cardie, Rogers, Schrodl (2001), Constrained K-means Clustering with Background Knowledge
}
\author{
Tran Khanh Hiep
Nguyen Minh Duc
}
\note{
The constraints should be consistent in order for the algorithm to work.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Wagstaff, Cardie, Rogers, Schrodl (2001), Constrained K-means Clustering with Background Knowledge
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
\keyword{ copkmeans }
\keyword{ constraint }% __ONLY ONE__ keyword per line
\keyword{ clustering }
