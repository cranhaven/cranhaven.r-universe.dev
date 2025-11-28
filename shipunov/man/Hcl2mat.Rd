\name{Hcl2mat}
\alias{Hcl2mat}
\title{Clustering to matrix}

\description{Converts clustering to matrix}

\usage{
  Hcl2mat(hcl)
}

\arguments{
  \item{hcl}{\code{hclust} object}
}

\details{
  This function converts 'hclust' object into binary matrix
  in accordance with clusterings.

  It has many uses: clustering bootstrap, clustering compare, and
  matrix representation of hierarchical clustering.
}

\seealso{
  \code{\link{Bclust}}, \code{\link{MRH}}
}

\examples{

head(Hcl2mat(hclust(dist(iris[, -5]))))

}

\keyword{cluster}
