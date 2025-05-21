\name{conclust-package}
\alias{conclust-package}
\alias{conclust}
\docType{package}
\title{
\packageTitle{conclust}
}
\description{
\packageDescription{conclust}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{conclust}
\packageIndices{conclust}
There are 4 main functions in this package: ckmeans(), lcvqe(), mpckm() and ccls(). They take an unlabeled dataset and two lists of must-link and cannot-link constraints as input and produce a clustering as output.
}
\author{
\packageAuthor{conclust}

Maintainer: \packageMaintainer{conclust}
}
\references{
Wagstaff, Cardie, Rogers, Schrodl (2001), Constrained K-means Clustering with Background Knowledge
Bilenko, Basu, Mooney (2004), Integrating Constraints and Metric Learning in Semi-Supervised Clustering
Dan Pelleg, Dorit Baras (2007), K-means with large and noisy constraint sets
}

\keyword{ copkmeans }
\keyword{ mpckmeans }
\keyword{ lcvqe }
\keyword{ constraint }
\keyword{ clustering }
\keyword{ ccls }
\keyword{ local search }
\keyword{ Tabu search }
\seealso{
Wagstaff, Cardie, Rogers, Schrodl (2001), Constrained K-means Clustering with Background Knowledge
Bilenko, Basu, Mooney (2004), Integrating Constraints and Metric Learning in Semi-Supervised Clustering
Dan Pelleg, Dorit Baras (2007), K-means with large and noisy constraint sets
}
\examples{
data = matrix(c(0, 1, 1, 0, 0, 0, 1, 1), nrow = 4)
mustLink = matrix(c(1, 2), nrow = 1)
cantLink = matrix(c(1, 4), nrow = 1)
k = 2
pred = ckmeans(data, k, mustLink, cantLink)
pred
pred = mpckm(data, k, mustLink, cantLink)
pred
pred = lcvqe(data, k, mustLink, cantLink)
pred
pred = ccls(data, k, mustLink, cantLink)
pred
}