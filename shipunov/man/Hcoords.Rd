\name{Hcoords}
\alias{Hcoords}
\title{Calculates coordinates of nodes from 'hclust' plot}
\description{Takes the 'hclust' plot and calculates coordinates of ann internal nodes}

\usage{
  Hcoords(hcl)
}

\arguments{
  \item{hcl}{\code{hclust} object}
}

\details{

  This function calculates coordinates for each 'hclust' node. Inspired
  by pvclust::hc2axes().

  Hcoords() is useful in connection with Bclust() family (namely,
  Bclabels()) and also separately. Since Hcoords() allows to label
  separate nodes, it can be used to label selected clusters (see
  examples).

}

\seealso{
  \code{\link{Bclust}}
}

\examples{

head(Hcoords(hclust(dist(iris[, -5]))))

## simple example: number all nodes
hcl <- hclust(UScitiesD, "ward.D2")
plot(hcl)
hcoo <- Hcoords(hcl)
text(hcoo, labels=1:nrow(hcoo), pos=1)

## complex example:
## find MCCN (Most Close Common Node)
## and label it
plot(hcl)
mat <- Hcl2mat(hcl)
nodes <- 1:nrow(mat) # nodes are rows
colnames(mat) <- hcl$labels
## take two tips and select those rows (nodes) where both present
sel1 <- rowSums(mat[, colnames(mat) \%in\% c("Denver", "Chicago")]) > 1
## MCCN is the node with both our tips but with the minimum of other tips
MCCN1 <- nodes[sel1][which.min(rowSums(mat[sel1, ]))]
text(hcoo[MCCN1, , drop=FALSE], labels="Eastern + Central", pos=1)
sel2 <- rowSums(mat[, colnames(mat) \%in\% c("Miami", "Chicago")]) > 1
MCCN2 <- nodes[sel2][which.min(rowSums(mat[sel2, ]))]
text(hcoo[MCCN2, , drop=FALSE], labels="Eastern", pos=1)

}

\keyword{cluster}
