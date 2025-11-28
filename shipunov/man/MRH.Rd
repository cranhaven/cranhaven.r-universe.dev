\name{MRH}
\alias{MRH}
\title{Matrix Representation of Hierarchical Clustering}

\description{Matrix Representation of Hierarchical clustering (MRH)}

\usage{
MRH(hcl, dim=NULL, method="groups")
}

\arguments{
  \item{hcl}{'hclust' object}

  \item{dim}{Number of desired dimensions, if defaults are not suitable}

  \item{method}{Either "groups" (default), or "height", or "branches", or "cophenetic" (see below for explanations)}
}

\value{
 Matrix with default number of columns equal to number of objects (n) minus 1 (method="branches" or method="cophenetic")
 or 'n-2' (method="groups"), or '2*n' (method="height").

 Rows are objects, values are either cluster numbers (method="groups" or method="height")
 so matrix consist of whole positive numbers,  binary cluster memberships (method="branches")
 or decimal MDS scores (method="cophenetic").
}

\details{
 This function calls cutree(), or Hcl2mat(), or cmdscale(cophenetic()) in order to output the
 Matrix Representation of Hierarchical clustering (MRH).

 If method="groups" then clustering tree is cut by all possible numbers of clusters 'k'
 (excluding 'k=1' and 'k=n' which bring no information) so 'dim' is always 'n-2'.

 If method="height" then clustering tree is cut by equally spaced agglomeration heights
 (excluding minimal and maximal heights which bring no information). Default 'dim' here is '2*n',
 but higher values might work even better.

 If method="branches" then use Hcl2mat() to transform object into the binary matrix of memberships,
 always with  'n-1' dimensions (so user-specified 'dim' is not taken into account).
 Each column in this matrix represents the tree branch.

 If method="cophenetic" then multidimensional scaling scores with maximum dimensionality on cophenetic
 distances are computed. Default 'dim' is 'n-1' but lesser numbers might work better.

 The main feature of the resulted matrices is that they provide the "bridge" of conversion between original data,
 distance matrices and clustering (including phylogenetic trees) results. After conversion, many interesting applications
 become possible. For example, if converted trees represent the _same_ objects,  it is possible to "hyper-bind",
 or "average" (Ashkenazy et al., 2018) them.

 To work with 'phylo' objects, convert them first to 'hclust' with as.hclust(), and before that, possibly also apply
 compute.brlen(), multi2di() and collapse.singles().
}

\references{
 Ashkenazy H., Sela I., Levy Karin E., Landan G., Pupko T. 2018. Multiple sequence alignment averaging
 improves phylogeny reconstruction. Systematic Biology. 68: 117--130.
}

\seealso{
  \code{\link{cutree}}, \code{link{cmdscale}}, \code{link{Hcl2mat}}
}

\examples{
aa.h <- hclust(dist(t(atmospheres)))
plot(aa.h)

(aa.mrh1 <- MRH(aa.h))
plot(hclust(dist(aa.mrh1)))

aa.mrh2 <- MRH(aa.h, method="height", dim=100) # here 'dim' should better be large
str(aa.mrh2)
plot(hclust(dist(aa.mrh2)))

plot(hclust(dist(cbind(aa.mrh1, aa.mrh2)))) # hyper-bind

(aa.mrh3 <- MRH(aa.h, method="branches"))
plot(hclust(dist(aa.mrh3)))

(aa.mrh4 <- MRH(aa.h, method="cophenetic"))
plot(hclust(dist(aa.mrh4)))

library(ape)
tree <- read.tree(text="((A:1,B:1):2,(C:3,D:4):2):3;")
(tree.mrh3 <- MRH(as.hclust(compute.brlen(tree)), method="branches"))
}

\keyword{cluster}
