\name{Adj.Rand}
\alias{Adj.Rand}
\title{Adjusted Rand index}
\description{Adjusted Rand index to compare different clusterings}

\usage{Adj.Rand(cl1, cl2, ...)}

\arguments{
\item{cl1}{First classification (character vector of group names)}
\item{cl2}{Second classification}
\item{...}{Further arguments to table()}
}

\details{

Use 'useNA="ifany"' or similar option to take NAs as a separate class
(for more explanations, see help for table() command).

Note that in rare cases, Adjusted Rand Index might become negative, this
might be some evidence that differences between two partitions are "worse
than random", i.e., there is a pattern in differences.

}

\value{Similarity: numerical vector of length 1}

\references{Hubert L. and Arabie P. 1985. Comparing partitions. Journal of Classification. 2. 193--218.}

\author{Alexey Shipunov}

\seealso{\code{\link{Misclass}}}

\examples{
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.hclust <- hclust(iris.dist)
iris.3 <- cutree(iris.hclust, 3)
Adj.Rand(iris.3, iris[, 5])
}

\keyword{cluster}
