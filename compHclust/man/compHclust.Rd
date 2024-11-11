\name{compHclust}

\alias{compHclust}

\title{Complementary Hierarchical Clustering}

\description{Performs the complementary hierarchical clustering
  procedure and returns X' (the expected residual matrix) and a vector
  of the relative gene importances.} 

\usage{
compHclust(x, xhc)
}

\arguments{
  \item{x}{A numeric matrix X, where interest lies in clustering its
  columns.}
  \item{xhc}{An object of class \code{hclust}, specifically, a hierarchical
    clustering of the columns of X.}
}

\details{This function performs the complementary hierarchical
  clustering procedure, as described in Nowak and Tibshirani (2008).
  Although applicable to any numeric matrix X, we typically think of X
  as microarray data with the rows as genes and the columns as samples,
  with the number of genes much greater than the number of samples.  The
  goal of the procedure is to uncover structures present in the data
  that arise from \sQuote{weak} genes.

  Given X and a hierarchical clustering of the columns of X, the
  function returns X', which represents a modified version of X with the
  structural features arising from the strong genes removed.  Using
  information present in the hierarchical clustering, we perform a
  series of linear regressions and set X' to be the expected value
  of the resulting residuals.  Details are given in Nowak and Tibshirani
  (2008).  The user can then apply a hierarchical clustering algorithm
  to cluster the columns of X' to discover any important structures
  arising from the weaker genes.

  The function also returns a vector of length equal to the number of
  rows of X, where the \emph{i}th element is equal to the relative gene
  importance of the \emph{i}th gene.  The relative gene importance lies
  between 0 and 1, with a value close to 1 indicating that a gene (row)
  was strongly influential in the hierarchical clustering of the columns
  of X.}

\value{A list with components:
  \item{x.prime}{The expected residual matrix X'.}
  \item{gene.imp}{A vector of the relative gene importances.}
}

\references{Nowak, G. and Tibshirani, R.  (2008)  Complementary
  hierarchical clustering.  \emph{Biostatistics}, \bold{9}(3),
  467--483.}

\author{Gen Nowak \email{gen.nowak@gmail.com} and Robert Tibshirani}

\seealso{\code{\link{hclust}}}

\examples{
## Creating example microarray data with rows as genes and columns as
## samples.  Rows 1-20 represent the 'strong' genes which differentiate
## samples 1-4 from samples 5-8.  Rows 31-50 represent the 'weak' genes
## which differentiate the odd numbered samples from the even numbered
## samples.
set.seed(4872)
x <- matrix(0,nrow=50,ncol=8)
x[1:20,1:4] <- 8
x[1:20,5:8] <- -8
x[31:50,c(1,3,5,7)] <- 4
x[31:50,-c(1,3,5,7)] <- -4
x <- x + matrix(rnorm(50*8),ncol=8)

## Hierarchically cluster the columns of x.
x.hc <- hclust(as.dist(1-cor(x)))

## Run complementary hierarchical clustering.
x.chc <- compHclust(x,x.hc)
xp <- x.chc$x.prime
x.gi <- x.chc$gene.imp

## Hierarchically cluster the columns of x'.
xp.hc <- hclust(as.dist(1-cor(xp)))
xp.gi <- compHclust(xp,xp.hc)$gene.imp

## We use the function 'compHclust.heatmap' to display the
## initial and complementary clusterings.
## The initial clustering.
compHclust.heatmap(x,x.hc,x.gi,d.title="Initial Clustering")
## The complementary clustering.
compHclust.heatmap(xp,xp.hc,xp.gi,d.title="Complementary Clustering")
}

\keyword{cluster}
\keyword{regression}
