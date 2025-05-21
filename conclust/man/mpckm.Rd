\name{mpckm}
\alias{mpckm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MPC K-means algorithm
}
\description{
This function takes an unlabeled dataset and two lists of must-link and cannot-link constraints as input and produce a clustering as output.
}
\usage{
mpckm(data, k, mustLink, cantLink, maxIter = 10)
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
This algorithm finds a clustering that satisfies as many constraints as possible
}
\value{
A vector that represents the labels (clusters) of the data points
}
\references{
Bilenko, Basu, Mooney (2004), Integrating Constraints and Metric Learning in Semi-Supervised Clustering
}
\author{
Tran Khanh Hiep
Nguyen Minh Duc
}
\note{
This is one of the best algorithm for clustering with constraints.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Bilenko, Basu, Mooney (2004), Integrating Constraints and Metric Learning in Semi-Supervised Clustering
}
\examples{
data = matrix(c(0, 1, 1, 0, 0, 0, 1, 1), nrow = 4)
mustLink = matrix(c(1, 2), nrow = 1)
cantLink = matrix(c(1, 4), nrow = 1)
k = 2
pred = mpckm(data, k, mustLink, cantLink)
pred
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mpckmeans }
\keyword{ constraint }% __ONLY ONE__ keyword per line
\keyword{ clustering }
