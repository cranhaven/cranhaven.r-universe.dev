\name{Bclust}
\alias{Bclust}
\alias{plot.Bclust}
\title{Bootstrapped hclust}

\description{Bootstraps (or jacknifes) hierarchical clustering}

\usage{

Bclust(data, method.d="euclidean", method.c="ward.D", FUN=function(.x)
 hclust(dist(.x, method=method.d), method=method.c),iter=1000,
 mc.cores=1, monitor=TRUE, bootstrap=TRUE, relative=FALSE, hclist=NULL)

\method{plot}{Bclust}(x, main="", xlab=NULL, ...)

}

\arguments{

  \item{data}{Data suitable for the chosen distance method}

  \item{method.d}{Method for dist()}

  \item{method.c}{Method for hclust()}

  \item{FUN}{Function to make 'hclust' objects}

  \item{iter}{Number of replicates}

  \item{mc.cores}{\code{integer}, number of processes to run in parallel}

  \item{monitor}{If TRUE (default), prints a dot for each replicate}

  \item{bootstrap}{If FALSE (not default), performs jacknife (and makes
  'iter=ncol(data)')}

  \item{relative}{If TRUE (not default), use the relative matching of
  branches (see in Details)}

  \item{hclist}{Allows to supply the list of 'hclust' objects}

  \item{x}{Object of the class 'Bclust'}

  \item{main}{Plot title}

  \item{xlab}{Horizontal axis label}

  \item{...}{Additional arguments to the plot.hclust()}

}

\value{

  Returns object of class 'Bclust' which is a list with components:
  'values' for bootstrapped frequencies of each node, 'hcl' for original
  'hclust' object, 'consensus' which is a sum of all Hcl2mat() matrices,
  'meth' (bootstrap or jacknife), and 'iter', for number of iterations.

}

\details{

  This function provides bootstrapping for hierarchical clustering
  (\code{\link{hclust}} objects). Internally, it uses Hcl2mat() which
  converts 'hclust' objects into binary matrix of cluster memberships.

  The default clustering method is the variance-minimizing "ward.D"
  (which works better with Euclidean distances); to make it coherent with
  hclust() default, specify 'method.c="complete"'. Also, it sometimes
  makes sense to transform non-Euclidean distances into Euclidean with
  'dist(_non_euclidean_dist_)'.

  Bclust() and companion functions were based on functions from the
  'bootstrap' package of Sebastian Gibb.

  Option 'hclist' presents the special case when list of 'hclust' objects
  is pre-build. In that case, other arguments (except 'mc.cores' and
  'monitor') will be ignored, and the first component of 'hclist', that
  is 'hclist[[1]]', will be used as "original" clustering to compare with
  all other objects in the 'hclist'. Number of replicates is the length
  of 'hclist' minus one.

  Option 'relative' changes the mechanism of how branches of reference
  clustering ("original") and bootstrapped clustering ("current")
  compared. If 'relative=FALSE' (default), only absolute matches (present
  or absent) are count, and vector of matches is binary (either 0 or 1).
  If 'relative=TRUE', branches of "original" which have no matches in
  "current", are checked additionally for the similarity with all
  branches of "current", and the minimal (asymmetric) binary
  dissimilarity value is used as a match. Therefore, the matching vector
  in this case is numeric instead of binary. This will typically result
  in the reliable raising of bootstrap values. The underlying methodology
  is similar to what is defined in  Lemoine et al. (2018) as a "transfer
  bootstrap". As the asymmetric binary is the _proportion_ of items in
  which only one is "1" amongst those which have one or two "1", it is
  possible to rephrase Lemoine et al. (2018), and say that this distance
  is equal to the _proportion_ of items that must be _removed_ to make
  both branches identical. Please note that with 'relative=TRUE', the
  whole algorithm is several times slower then default.

  Please note that Bclust() frequently underestimates the cluster
  stability when number of characters is relatively small. One of
  possible remedies is to use hyper-binding (like "cbind(data, data,
  data)") to reach the reliable number of characters.

  plot.Bclust() designed for quick plotting and plots labels (bootstrap
  support values) with the following defaults: 'percent=TRUE, pos=3,
  offset=0.1'. To change how labels are plotted, use separate Bclabels()
  command.

}

\references{

  Felsenstein J. 1985. Confidence limits on phylogenies: an approach
  using the bootstrap. Evolution. 39 (4): 783--791.

  Efron B., Halloran E., Holmes S. 1996. Bootstrap confidence levels for
  phylogenetic trees. Proceedings of the National Academy of Sciences. 93
  (23): 13429--13429.

  Lemoine F. et al. 2018. Renewing Felsenstein's phylogenetic bootstrap
  in the era of big data. Nature, 556(7702): 452--456

}

\seealso{
  \code{\link{Jclust}}, \code{\link{BootA}}, \code{\link{Hcl2mat}},
  \code{\link{Bclabels}}, \code{\link{Hcoords}}
}

\examples{

data <- t(atmospheres)

## standard use
(bb <- Bclust(data)) # specify 'mc.cores=4' or similar to speed up the process
plot(bb)

## more advanced plotting with Bclabels()
plot(bb$hclust)
Bclabels(bb$hclust, bb$values, threshold=0.5, col="grey", pos=1)

## how to use the consensus data
plot(hclust(dist(bb$consensus)), main="Net consensus tree") # net consensus
## majority rule is 'consensus >= 0.5', strict is like 'round(consensus) == 1'

## how to make user-defined function
bb1 <- Bclust(t(atmospheres), FUN=function(.x) hclust(Gower.dist(.x)))
plot(bb1)

## how to jacknife
bb2 <- Bclust(data, bootstrap=FALSE, monitor=FALSE)
plot(bb2)

## how to make (and use) the pre-build list of clusterings
hclist <- vector("list", length=0)
hclist[[1]] <- hclust(dist(data)) # "orig" is the first
for (n in 2:101) hclist[[n]] <- hclust(dist(data[, sample.int(ncol(data), replace=TRUE)]))
(bb3 <- Bclust(hclist=hclist))
plot(bb3)

## how to use the relative matching
bb4 <- Bclust(data, relative=TRUE)
plot(bb4)

## how to hyper-bind
bb5 <- Bclust(cbind(data, data, data)) # now data has 24 characters
plot(bb5)

## how to use hclust() defaults
bb6 <- Bclust(data, method.c="complete")
plot(bb6)

}

\keyword{cluster}
