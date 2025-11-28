\name{NC.dist}
\alias{NC.dist}
\title{Normalized Compression Distance}
\description{Calculates the normalized compression distance}

\usage{NC.dist(data, method="gzip", character=TRUE)}

\arguments{

\item{data}{Matrix (or data frame) with variables that should be used in
the computation of the distance between rows.}

\item{method}{Taken from memCompress(): either "gzip", or "bzip2", or
"xz"; the last is very slow}

\item{character}{Convert to character mode (default), or use as raw?}

}

\details{

NC.dist() computes the distance based on the sizes of the compressed
vectors. It is calculated as

dissimilarity(x, y) = B(x, y) - max(B(x), B(y)) / min(B(x), B(y))

where B(x) and B(y) are the bytesizes of the compressed 'x' and 'y', and
B(x, y) is the comressed bytesize of concatenated 'x' and 'y'. The
algorithm uses basic memCompress() function.

If argument is the data frame, NC.dist() internally converts it into the
matrix. All columns by default will be converted into character mode (and
if 'character=FALSE', into raw). This default behavior allows NC.dist() to be
the universal distance which also does not mind NAs and zeroes.

}

\value{Distance object with distances among rows of 'data'}

\references{

Cilibrasi, R., & Vitanyi, P. M. (2005). Clustering by compression.
Information Theory, IEEE Transactions on, 51(4), 1523-1545.

}

\author{Alexey Shipunov}

\seealso{\code{\link{memCompress}}}

\examples{

## converts variables into character, universal method
iris.nc <- NC.dist(iris[, -5])
iris.hnc <- hclust(iris.nc, method="ward.D2")
## amazingly, it works even for vectors with length=4 (iris data rows)
plot(prcomp(iris[, -5])$x, col=cutree(iris.hnc, 3))

## using variables as raw, it is good when they are uniform
iris.nc2 <- NC.dist(iris[, -5], character=FALSE)
iris.hnc2 <- hclust(iris.nc2, method="ward.D2")
plot(prcomp(iris[, -5])$x, col=cutree(iris.hnc2, 3))

## bzip2 uses Burrows-Wheeler transform
NC.dist(matrix(runif(100), ncol=10), method="bzip2")

}
